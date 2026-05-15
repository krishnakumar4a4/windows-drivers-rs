// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP tracing provider implementation.
//!
//! Contains `WppProvider` — one instance per trace control block declaration —
//! with its own state for level, flags, logger handle, and auto-log context.
//! The provider instances are stored in a global array initialised during
//! `DriverEntry` and live for the entire lifetime of the driver.
//!
//! Uses modern ETW APIs (`EtwRegister` / `EtwWriteTransfer`) instead of
//! legacy WMI classic providers (`EtwRegisterClassicProvider` / `WmiTraceMessage`).

use core::{cell::UnsafeCell, mem, ptr};
use core::sync::atomic::{AtomicU8, AtomicU16, AtomicU32, AtomicU64, AtomicPtr, Ordering};

use wdk_sys::{
    DRIVER_OBJECT,
    LPCGUID,
    LPGUID,
    NTSTATUS,
    PCUNICODE_STRING,
    PDRIVER_OBJECT,
    PVOID,
    REGHANDLE,
    TRACEHANDLE,
    UCHAR,
    ULONG,
    USHORT,
};

use wdk::println;

// ---------------------------------------------------------------------------
// UnsafeOnceCell
// ---------------------------------------------------------------------------

/// A container like [`core::cell::OnceCell`] that is
/// set once and read multiple times.
///
/// # Safety
/// The reason this type has the prefix `Unsafe` in its name
/// is because it is not thread safe. To use it safely
/// the user must uphold the following invariants:
/// - The [`set`] method must not be called concurrently with itself or the
///   [`get`] method.
/// - The `get` method must not be called concurrently with the `set` method.
///
/// The typical pattern is to call `set` first from
/// a single thread and initialize the value, and then
/// call `get` from multiple threads as needed.
pub(crate) struct UnsafeOnceCell<T> {
    val: UnsafeCell<Option<T>>,
}

impl<T> UnsafeOnceCell<T> {
    pub const fn new() -> Self {
        Self {
            val: UnsafeCell::new(None),
        }
    }

    pub unsafe fn get(&self) -> Option<&T> {
        // SAFETY: Safe because we assume that the call to this method
        // is not concurrent with the `set` method.
        let val_ref = unsafe { &*self.val.get() };
        val_ref.as_ref()
    }

    pub unsafe fn set(&self, val: T) {
        // SAFETY: Safe because we assume that the call to this method
        // is not concurrent with itself or the `get` method.
        unsafe {
            let val_ptr = self.val.get();
            assert!((*val_ptr).is_none(), "value should not be already set");
            *val_ptr = Some(val);
        };
    }
}

unsafe impl<T> Sync for UnsafeOnceCell<T> where T: Sync {}

// ---------------------------------------------------------------------------
// Globals (expected by IFR / WinDbg)
// ---------------------------------------------------------------------------

/// These globals are expected by IFR functionality such as the
/// windbg extensions used to read IFR logs
#[unsafe(no_mangle)]
pub static mut WPP_GLOBAL_Control: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();

#[unsafe(no_mangle)]
pub static mut WPP_RECORDER_INITIALIZED: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();

// ---------------------------------------------------------------------------
// Trace control codes
// ---------------------------------------------------------------------------

// Control codes passed to the ETW enable callback.
const ETW_CONTROL_ENABLE: u32 = 1;
const ETW_CONTROL_DISABLE: u32 = 0;

// ---------------------------------------------------------------------------
// FFI
// ---------------------------------------------------------------------------

unsafe extern "C" {
    /// C runtime strlen function - exposed for macro use
    pub fn strlen(str: *const core::ffi::c_char) -> usize;
}

// ---------------------------------------------------------------------------
// WPP control block types
// ---------------------------------------------------------------------------

/// Minimum-sized declaration of the WPP project control block.
#[repr(C)]
pub union WPP_PROJECT_CONTROL_BLOCK {
    pub Control: mem::ManuallyDrop<WPP_TRACE_CONTROL_BLOCK>,
    ReserveSpace: [UCHAR; mem::size_of::<WPP_TRACE_CONTROL_BLOCK>()],
}

#[repr(C)]
pub struct WPP_TRACE_CONTROL_BLOCK {
    Callback: PVOID, // Kept for C ABI compatibility; unused by Rust code
    pub ControlGuid: LPCGUID,
    pub Next: *const WPP_TRACE_CONTROL_BLOCK,
    pub Logger: TRACEHANDLE,
    RegistryPath: PVOID, // PUNICODE_STRING in C; kept as PVOID
    pub FlagsLen: UCHAR,
    pub Level: UCHAR,
    Reserved: USHORT,
    pub Flags: [ULONG; 1],
    ReservedFlags: ULONG,
    pub RegHandle: REGHANDLE,
    pub AutoLogContext: PVOID,
    pub AutoLogVerboseEnabled: USHORT,
    AutoLogAttachToMiniDump: USHORT,
}

impl WPP_TRACE_CONTROL_BLOCK {
    /// Creates a new zeroed `WPP_TRACE_CONTROL_BLOCK` with the given `flags_len`.
    #[doc(hidden)]
    pub const fn new(flags_len: UCHAR) -> Self {
        Self {
            Callback: ptr::null_mut(),
            ControlGuid: ptr::null(),
            Next: ptr::null(),
            Logger: 0,
            RegistryPath: ptr::null_mut(),
            FlagsLen: flags_len,
            Level: 0,
            Reserved: 0,
            Flags: [0; 1],
            ReservedFlags: 0,
            RegHandle: 0,
            AutoLogContext: ptr::null_mut(),
            AutoLogVerboseEnabled: 0,
            AutoLogAttachToMiniDump: 0,
        }
    }
}



unsafe extern "C" {
    fn WppAutoLogStart(
        WppCb: *mut WPP_PROJECT_CONTROL_BLOCK,
        DrvObj: PDRIVER_OBJECT,
        RegPath: PCUNICODE_STRING,
    );
    fn WppAutoLogStop(WppCb: *mut WPP_PROJECT_CONTROL_BLOCK, DrvObj: PDRIVER_OBJECT);
    /// WPP Auto Log Trace function - exposed for macro-generated code
    #[doc(hidden)]
    pub fn WppAutoLogTrace(
        AutoLogContext: PVOID,
        MessageLevel: UCHAR,
        MessageFlags: ULONG,
        MessageGuid: LPGUID,
        MessageNumber: USHORT,
        ...
    ) -> NTSTATUS;
}

// ---------------------------------------------------------------------------
// WppProvider — one instance per control block
// ---------------------------------------------------------------------------

/// A WPP trace provider instance.
///
/// Each control block declaration produces one `WppProvider` with its own
/// state for level, flags, logger handle, and auto-log context.  These
/// fields are updated atomically by the ETW callback and read lock-free
/// by the generated `trace_N` methods.
///
/// Uses modern `EtwRegister` / `EtwWriteTransfer` APIs instead of legacy
/// WMI classic providers.
pub struct WppProvider {
    logger: AtomicU64,
    level: AtomicU8,
    flags: AtomicU32,
    auto_log_context: AtomicPtr<core::ffi::c_void>,
    auto_log_verbose_enabled: AtomicU16,
    reg_handle: AtomicU64,
    init_state: AtomicU8,
    control_block: *mut WPP_TRACE_CONTROL_BLOCK,
}

impl WppProvider {
    fn new(control_block: *mut WPP_TRACE_CONTROL_BLOCK) -> Self {
        Self {
            logger: AtomicU64::new(0),
            level: AtomicU8::new(0),
            flags: AtomicU32::new(0),
            auto_log_context: AtomicPtr::new(ptr::null_mut()),
            auto_log_verbose_enabled: AtomicU16::new(0),
            reg_handle: AtomicU64::new(0),
            init_state: AtomicU8::new(crate::provider::UNINITIALIZED),
            control_block,
        }
    }

    /// Returns `true` if the given flag bit is enabled.
    #[inline]
    pub fn is_flag_enabled(&self, flags: u32) -> bool {
        self.logger.load(Ordering::Relaxed) != 0
            && (self.flags.load(Ordering::Relaxed) & (1 << ((flags - 1) & 31))) != 0
    }

    /// Returns `true` if the given level is at or below the enabled level.
    #[inline]
    pub fn is_level_enabled(&self, level: u8) -> bool {
        self.logger.load(Ordering::Relaxed) != 0
            && level <= self.level.load(Ordering::Relaxed)
    }

    /// Returns `true` if auto-log verbose mode is enabled.
    #[inline]
    pub fn is_auto_log_verbose_enabled(&self) -> bool {
        self.auto_log_verbose_enabled.load(Ordering::Relaxed) != 0
    }

    /// Returns the auto-log context pointer for WppAutoLogTrace calls.
    #[doc(hidden)]
    #[inline]
    pub fn get_auto_log_context(&self) -> PVOID {
        self.auto_log_context.load(Ordering::Relaxed)
    }

    /// Returns the TRACEHANDLE logger (set to reg_handle when enabled).
    #[doc(hidden)]
    #[inline]
    pub fn get_wpp_logger(&self) -> TRACEHANDLE {
        self.logger.load(Ordering::Relaxed)
    }

    /// Returns the ETW registration handle for `EtwWriteTransfer` calls.
    #[doc(hidden)]
    #[inline]
    pub fn reg_handle(&self) -> u64 {
        self.reg_handle.load(Ordering::Relaxed)
    }

    // Pre-generated `trace_0`..`trace_8` inherent methods.
    wpp_macros::define_trace_writer_methods!();
}

// SAFETY: All mutable state in WppProvider uses atomics.  The raw pointer
// `control_block` points to a static that outlives the driver.
unsafe impl Sync for WppProvider {}

// ---------------------------------------------------------------------------
// ETW enable callback — updates BOTH provider atomics + control block
// ---------------------------------------------------------------------------

/// ETW enable callback invoked when a trace controller enables/disables
/// this provider. Uses the modern `EtwRegister` callback signature.
///
/// # Safety
///
/// `callback_context` must point to a valid `WppProvider`.
unsafe extern "system" fn etw_enable_callback(
    _source_id: *const crate::GUID,
    control_code: u32,
    level: u8,
    match_any_keyword: u64,
    _match_all_keyword: u64,
    _filter_data: *const core::ffi::c_void,
    callback_context: *mut core::ffi::c_void,
) {
    if callback_context.is_null() {
        return;
    }

    let provider = unsafe { &*(callback_context as *const WppProvider) };

    unsafe {
        let cb = provider.control_block;

        match control_code {
            ETW_CONTROL_ENABLE => {
                let enable_flags = match_any_keyword as u32;
                let reg = provider.reg_handle.load(Ordering::Relaxed);

                provider.flags.store(enable_flags, Ordering::Release);
                provider.level.store(level, Ordering::Release);
                // Set logger to reg_handle so is_flag_enabled/is_level_enabled
                // fast-path checks see a non-zero value.
                provider.logger.store(reg, Ordering::Release);

                (*cb).Flags[0] = enable_flags;
                (*cb).Level = level;
                (*cb).Logger = reg;
            }
            ETW_CONTROL_DISABLE => {
                provider.flags.store(0, Ordering::Release);
                provider.level.store(0, Ordering::Release);
                provider.logger.store(0, Ordering::Release);

                (*cb).Flags[0] = 0;
                (*cb).Level = 0;
                (*cb).Logger = 0;
            }
            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Global tracing state
// ---------------------------------------------------------------------------

struct WppTracingState {
    providers: alloc::boxed::Box<[WppProvider]>,
    control_blocks_ptr: *mut WPP_PROJECT_CONTROL_BLOCK,
    wdm_driver: *mut DRIVER_OBJECT,
    #[allow(dead_code)]
    reg_path: PCUNICODE_STRING,
}

// SAFETY: The raw pointers inside WppTracingState point to statics / kernel
// objects that outlive the driver and are only mutated under WPP's own
// serialization guarantees.
unsafe impl Sync for WppTracingState {}

static WPP_STATE: UnsafeOnceCell<WppTracingState> = UnsafeOnceCell::new();

// ---------------------------------------------------------------------------
// Public API — init / cleanup / get_provider
// ---------------------------------------------------------------------------

/// Initialises WPP tracing.  Called from `call_safe_driver_entry` in `wdf`.
///
/// Uses modern `EtwRegister` instead of legacy `EtwRegisterClassicProvider`.
#[doc(hidden)]
pub unsafe fn init_tracing(
    control_blocks: *mut WPP_PROJECT_CONTROL_BLOCK,
    num_controls: usize,
    wdm_driver: *mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
) {
    let mut providers_vec = alloc::vec::Vec::with_capacity(num_controls);
    let mut wpp_reg: *mut WPP_TRACE_CONTROL_BLOCK =
        control_blocks.cast::<WPP_TRACE_CONTROL_BLOCK>();

    while !wpp_reg.is_null() {
        unsafe {
            providers_vec.push(WppProvider::new(wpp_reg));
            wpp_reg = (*wpp_reg).Next.cast_mut();
        }
    }

    let providers: alloc::boxed::Box<[WppProvider]> = providers_vec.into_boxed_slice();

    // Register each provider using the modern EtwRegister API.
    for provider in providers.iter() {
        if provider
            .init_state
            .compare_exchange(
                crate::provider::UNINITIALIZED,
                crate::provider::INITIALIZING,
                Ordering::Acquire,
                Ordering::Relaxed,
            )
            .is_err()
        {
            continue;
        }

        unsafe {
            let cb = provider.control_block;
            let guid_ptr = (*cb).ControlGuid as *const crate::GUID;

            let (status, handle) = crate::etw::register(
                &*guid_ptr,
                Some(etw_enable_callback),
                (provider as *const WppProvider).cast_mut().cast(),
            );
            println!("EtwRegister status: {status:?}");

            provider.reg_handle.store(handle, Ordering::Relaxed);
            (*cb).RegHandle = handle;
        }

        provider.init_state.store(
            crate::provider::INITIALIZED,
            Ordering::Release,
        );
    }

    unsafe {
        WPP_GLOBAL_Control = control_blocks;
        WppAutoLogStart(control_blocks, wdm_driver, reg_path);
        WPP_RECORDER_INITIALIZED = WPP_GLOBAL_Control;
    }

    let primary_ctx = if !providers.is_empty() {
        unsafe { &*providers[0].control_block }.AutoLogContext
    } else {
        ptr::null_mut()
    };

    for provider in providers.iter() {
        let cb = unsafe { &*provider.control_block };
        let ctx = if cb.AutoLogContext.is_null() {
            primary_ctx
        } else {
            cb.AutoLogContext
        };
        provider.auto_log_context.store(ctx, Ordering::Release);
        provider
            .auto_log_verbose_enabled
            .store(cb.AutoLogVerboseEnabled, Ordering::Release);
    }

    let state = WppTracingState {
        providers,
        control_blocks_ptr: control_blocks,
        wdm_driver,
        reg_path,
    };
    unsafe {
        WPP_STATE.set(state);
    }
}

/// Stops WPP tracing.  Called from `wdf_driver_unload`.
///
/// Unregisters providers using `EtwUnregister`.
#[doc(hidden)]
pub fn cleanup_tracing() {
    if let Some(state) = unsafe { WPP_STATE.get() } {
        for provider in state.providers.iter() {
            if provider
                .init_state
                .compare_exchange(
                    crate::provider::INITIALIZED,
                    crate::provider::UNINITIALIZED,
                    Ordering::Acquire,
                    Ordering::Relaxed,
                )
                .is_err()
            {
                continue;
            }

            provider.level.store(0, Ordering::Relaxed);
            provider.flags.store(0, Ordering::Relaxed);
            provider.logger.store(0, Ordering::Relaxed);

            let handle = provider.reg_handle.swap(0, Ordering::Relaxed);
            if handle != 0 {
                let status = unsafe { crate::etw::unregister(handle) };
                println!("EtwUnregister status: {status:?}");
                unsafe {
                    (*provider.control_block).RegHandle = 0;
                }
            }
        }

        unsafe {
            WppAutoLogStop(state.control_blocks_ptr, state.wdm_driver);
            WPP_RECORDER_INITIALIZED = ptr::null_mut();
            WPP_GLOBAL_Control = ptr::null_mut();
        }
    }
}

/// Returns a reference to the provider at `index`, if tracing is initialised.
pub fn get_provider(index: usize) -> Option<&'static WppProvider> {
    // SAFETY: Not concurrent with set — set runs once in DriverEntry.
    unsafe { WPP_STATE.get() }.and_then(|state| state.providers.get(index))
}
