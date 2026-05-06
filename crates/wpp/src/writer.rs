// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP tracing provider implementation.
//!
//! Contains `WppProvider` — one instance per trace control block declaration —
//! with its own state for level, flags, logger handle, and auto-log context.
//! The provider instances are stored in a global array initialised during
//! `DriverEntry` and live for the entire lifetime of the driver.

use core::{cell::UnsafeCell, mem, ptr};
use core::sync::atomic::{AtomicU8, AtomicU16, AtomicU32, AtomicU64, AtomicPtr, Ordering};

use wdk_sys::{
    DRIVER_OBJECT,
    LPCGUID,
    LPGUID,
    NTSTATUS,
    PCUNICODE_STRING,
    PDRIVER_OBJECT,
    PREGHANDLE,
    PULONG,
    PUNICODE_STRING,
    PVOID,
    REGHANDLE,
    TRACEHANDLE,
    UCHAR,
    ULONG,
    ULONG64,
    UNICODE_STRING,
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
// Unicode string helper (avoids depending on wdf::string)
// ---------------------------------------------------------------------------

/// Creates a UNICODE_STRING from a Rust &str. Returns the boxed buffer
/// (to keep it alive) and the UNICODE_STRING.
fn create_unicode_string(s: &str) -> (alloc::boxed::Box<[u16]>, UNICODE_STRING) {
    let buf: alloc::boxed::Box<[u16]> = s
        .encode_utf16()
        .chain(core::iter::once(0))
        .collect::<alloc::vec::Vec<_>>()
        .into_boxed_slice();
    let byte_len = (buf.len() * 2) as u16;
    let unicode_str = UNICODE_STRING {
        Length: byte_len - 2,
        MaximumLength: byte_len,
        Buffer: buf.as_ptr().cast_mut().cast(),
    };
    (buf, unicode_str)
}

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

// Trace control codes used by WPP classic provider callback
// These were originally defined in evntrace.h as below
// #define EVENT_CONTROL_CODE_DISABLE_PROVIDER 0
// #define EVENT_CONTROL_CODE_ENABLE_PROVIDER  1
// #define EVENT_CONTROL_CODE_CAPTURE_STATE    2
enum TraceControlCode {
    Enable = 1,
    Disable = 0,
    #[allow(dead_code)]
    CaptureState = 2,
}

// ---------------------------------------------------------------------------
// FFI
// ---------------------------------------------------------------------------

unsafe extern "C" {
    fn MmGetSystemRoutineAddress(SystemRoutineName: PUNICODE_STRING) -> PVOID;
    /// C runtime strlen function - exposed for macro use
    pub fn strlen(str: *const core::ffi::c_char) -> usize;
}

macro_rules! get_routine_addr {
    ($name:expr, $callback_type:ty) => {{
        let (_buf, unicode_str) = create_unicode_string($name);
        let addr = unsafe {
            MmGetSystemRoutineAddress((&unicode_str as *const UNICODE_STRING).cast_mut())
        };
        unsafe { mem::transmute::<PVOID, Option<$callback_type>>(addr) }
    }};
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
    Callback: Option<WMIENTRY_NEW>,
    pub ControlGuid: LPCGUID,
    pub Next: *const WPP_TRACE_CONTROL_BLOCK,
    pub Logger: TRACEHANDLE,
    RegistryPath: PUNICODE_STRING,
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
            Callback: None,
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

#[repr(C)]
struct WPP_TRACE_ENABLE_CONTEXT {
    LoggerId: USHORT,
    Level: UCHAR,
    InternalFlag: UCHAR,
    EnableFlags: ULONG,
}

type WMIENTRY_NEW = extern "C" fn(
    MinorFunction: UCHAR,
    DataPath: PVOID,
    BufferLength: ULONG,
    Buffer: PVOID,
    Context: PVOID,
    Size: PULONG,
) -> u64;

/// WPP Trace Message function - exposed for macro-generated code
#[doc(hidden)]
pub type WppTraceMessage = extern "C" fn(
    LoggerHandle: ULONG64,
    MessageFlags: ULONG,
    MessageGuid: LPCGUID,
    MessageNumber: USHORT,
    ...
) -> NTSTATUS;

type EtwClassicCallback =
    extern "C" fn(Guid: LPCGUID, ControlCode: UCHAR, EnableContext: PVOID, CallbackContext: PVOID);
type EtwRegisterClassicProvider = extern "C" fn(
    ProviderGuid: LPCGUID,
    Type: ULONG,
    EnableCallback: EtwClassicCallback,
    CallbackContext: PVOID,
    RegHandle: PREGHANDLE,
) -> NTSTATUS;
type EtwUnregister = extern "C" fn(RegHandle: REGHANDLE) -> NTSTATUS;

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
    fn imp_WppRecorderReplay(
        WppCb: PVOID,
        WppTraceHandle: TRACEHANDLE,
        EnableFlags: ULONG,
        EnableLevel: UCHAR,
    );
}

// ---------------------------------------------------------------------------
// WPP_TRACE_OPTIONS
// ---------------------------------------------------------------------------

// Trace options used by WPP tracing - exposed for macro use
// These are defined in evntrace.h as follows:
// #define TRACE_MESSAGE_SEQUENCE              1
// #define TRACE_MESSAGE_GUID                  2
// #define TRACE_MESSAGE_COMPONENTID           4
// #define TRACE_MESSAGE_TIMESTAMP             8
// #define TRACE_MESSAGE_PERFORMANCE_TIMESTAMP 16
// #define TRACE_MESSAGE_SYSTEMINFO            32
const TRACE_MESSAGE_SEQUENCE: ULONG = 1;
const TRACE_MESSAGE_GUID: ULONG = 2;
const TRACE_MESSAGE_SYSTEMINFO: ULONG = 32;
const TRACE_MESSAGE_TIMESTAMP: ULONG = 8;

#[doc(hidden)]
pub const WPP_TRACE_OPTIONS: ULONG = TRACE_MESSAGE_SEQUENCE
    | TRACE_MESSAGE_GUID
    | TRACE_MESSAGE_SYSTEMINFO
    | TRACE_MESSAGE_TIMESTAMP;

// ---------------------------------------------------------------------------
// WppProvider — one instance per control block
// ---------------------------------------------------------------------------

/// A WPP trace provider instance.
///
/// Each control block declaration produces one `WppProvider` with its own
/// state for level, flags, logger handle, and auto-log context.  These
/// fields are updated atomically by the ETW callback and read lock-free
/// by the generated `trace_N` methods.
pub struct WppProvider {
    logger: AtomicU64,
    level: AtomicU8,
    flags: AtomicU32,
    auto_log_context: AtomicPtr<core::ffi::c_void>,
    auto_log_verbose_enabled: AtomicU16,
    wpp_trace_message: Option<WppTraceMessage>,
    etw_unregister: Option<EtwUnregister>,
    control_block: *mut WPP_TRACE_CONTROL_BLOCK,
}

impl WppProvider {
    fn new(
        control_block: *mut WPP_TRACE_CONTROL_BLOCK,
        wpp_trace_message: Option<WppTraceMessage>,
        etw_unregister: Option<EtwUnregister>,
    ) -> Self {
        Self {
            logger: AtomicU64::new(0),
            level: AtomicU8::new(0),
            flags: AtomicU32::new(0),
            auto_log_context: AtomicPtr::new(ptr::null_mut()),
            auto_log_verbose_enabled: AtomicU16::new(0),
            wpp_trace_message,
            etw_unregister,
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

    /// Returns the TRACEHANDLE logger.
    #[doc(hidden)]
    #[inline]
    pub fn get_wpp_logger(&self) -> TRACEHANDLE {
        self.logger.load(Ordering::Relaxed)
    }

    /// Returns the WppTraceMessage function pointer, if resolved.
    #[doc(hidden)]
    #[inline]
    pub fn get_wpp_trace_message(&self) -> Option<WppTraceMessage> {
        self.wpp_trace_message
    }

    // Pre-generated `trace_0`..`trace_8` inherent methods.
    wpp_macros::define_trace_writer_methods!();
}

// SAFETY: All mutable state in WppProvider uses atomics.  The raw pointer
// `control_block` points to a static that outlives the driver.
unsafe impl Sync for WppProvider {}

// ---------------------------------------------------------------------------
// WppClassicProviderCallback — updates BOTH provider + control block
// ---------------------------------------------------------------------------

extern "C" fn WppClassicProviderCallback(
    _Guid: LPCGUID,
    ControlCode: UCHAR,
    EnableContext: PVOID,
    CallbackContext: PVOID,
) {
    let provider = CallbackContext.cast::<WppProvider>();
    let trace_context = EnableContext.cast::<WPP_TRACE_ENABLE_CONTEXT>();

    if ControlCode != TraceControlCode::Enable as u8
        && ControlCode != TraceControlCode::Disable as u8
    {
        return;
    }

    unsafe {
        let cb = (*provider).control_block;

        if ControlCode != TraceControlCode::Disable as u8 {
            let enable_flags = (*trace_context).EnableFlags;
            let enable_level = (*trace_context).Level as UCHAR;
            let logger = *(trace_context.cast::<TRACEHANDLE>());

            (*provider).flags.store(enable_flags, Ordering::Release);
            (*provider).level.store(enable_level, Ordering::Release);
            (*provider).logger.store(logger, Ordering::Release);

            (*cb).Flags[0] = enable_flags;
            (*cb).Level = enable_level;
            (*cb).Logger = logger;

            // TODO: This should be called only when (NTDDI_VERSION >= NTDDI_WIN10_RS1)
            imp_WppRecorderReplay(
                WPP_GLOBAL_Control.cast(),
                logger,
                enable_flags,
                enable_level,
            );
        } else {
            (*provider).flags.store(0, Ordering::Release);
            (*provider).level.store(0, Ordering::Release);
            (*provider).logger.store(0, Ordering::Release);

            (*cb).Flags[0] = 0;
            (*cb).Level = 0;
            (*cb).Logger = 0;
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
#[doc(hidden)]
pub unsafe fn init_tracing(
    control_blocks: *mut WPP_PROJECT_CONTROL_BLOCK,
    num_controls: usize,
    wdm_driver: *mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
) {
    let etw_register = get_routine_addr!("EtwRegisterClassicProvider", EtwRegisterClassicProvider);
    let etw_unregister = if etw_register.is_some() {
        get_routine_addr!("EtwUnregister", EtwUnregister)
    } else {
        None
    };
    let wpp_trace_message = get_routine_addr!("WmiTraceMessage", WppTraceMessage);

    let mut providers_vec = alloc::vec::Vec::with_capacity(num_controls);
    let mut wpp_reg: *mut WPP_TRACE_CONTROL_BLOCK =
        control_blocks.cast::<WPP_TRACE_CONTROL_BLOCK>();

    while !wpp_reg.is_null() {
        unsafe {
            providers_vec.push(WppProvider::new(wpp_reg, wpp_trace_message, etw_unregister));
            wpp_reg = (*wpp_reg).Next.cast_mut();
        }
    }

    let providers: alloc::boxed::Box<[WppProvider]> = providers_vec.into_boxed_slice();

    if let Some(etw_register_provider) = etw_register {
        for provider in providers.iter() {
            unsafe {
                let cb = provider.control_block;
                (*cb).RegHandle = 0;
                let status = etw_register_provider(
                    (*cb).ControlGuid,
                    0,
                    WppClassicProviderCallback,
                    (provider as *const WppProvider).cast_mut().cast(),
                    &raw mut (*cb).RegHandle,
                );
                println!("EtwRegisterClassicProvider status: {status:?}");
            }
        }
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
#[doc(hidden)]
pub fn cleanup_tracing() {
    if let Some(state) = unsafe { WPP_STATE.get() } {
        for provider in state.providers.iter() {
            if let Some(etw_unregister) = provider.etw_unregister {
                let reg_handle = unsafe { (*provider.control_block).RegHandle };
                if reg_handle != 0 {
                    let status = etw_unregister(reg_handle);
                    println!("EtwUnregister status: {status:?}");
                    unsafe {
                        (*provider.control_block).RegHandle = 0;
                    }
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
