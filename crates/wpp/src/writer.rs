// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP trace writer implementation.
//!
//! Contains `WppWriter` (the safe wrapper around WPP tracing), control block
//! types, FFI externs, globals, and accessor functions for macro-generated code.

use core::{cell::UnsafeCell, mem, ptr};

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
// UnsafeOnceCell — duplicated from wdf for use in this crate
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
// WppWriter
// ---------------------------------------------------------------------------

/// A writer for WPP tracing (renamed from TraceWriter).
///
/// It is a safe wrapper around the WPP tracing system.
/// It maintains data necessary to call WPP functions such
/// as the control block. This data is passed to WPP functions
/// as `*mut T` sometimes (e.g. in the [`start`] function)
/// even though the wrapper function itself (i.e. [`start`])
/// takes `&self` instead of `&mut self`.
/// This is safe because the WPP functions are supposed to modify
/// the passed data in a thread-safe manner and `WppWriter` itself
/// never modifies them.
/// In this way `WppWriter` implements interior mutability.
pub struct WppWriter {
    pub(crate) trace_config: TraceConfig,
    wdm_driver: *mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
}

impl WppWriter {
    /// Creates a new `WppWriter` and registers all trace providers with ETW.
    ///
    /// This mirrors the C WPP `WppInitKm` function. The macro-generated code
    /// has already populated the `WPP_MAIN_CB` array (control GUIDs, `Next`
    /// linked-list pointers, `FlagsLen`). This function walks the linked list
    /// starting at `&WPP_CB[0].Control` and calls `EtwRegisterClassicProvider`
    /// for each control block, exactly as `WppInitKm` does in the TMH.
    ///
    /// # Safety
    ///
    /// * `control_blocks_ptr` must point to a valid array of
    ///   `WPP_PROJECT_CONTROL_BLOCK` with `num_controls` elements, whose
    ///   `Control.Next` pointers form a null-terminated linked list.
    /// * The array must have `'static` lifetime (i.e. be a module-level static).
    /// * `wdm_driver` and `reg_path` must be valid for the lifetime of tracing.
    pub unsafe fn init(
        control_blocks_ptr: *mut WPP_PROJECT_CONTROL_BLOCK,
        num_controls: usize,
        wdm_driver: *mut DRIVER_OBJECT,
        reg_path: PCUNICODE_STRING,
    ) -> Self {
        // Create a safe slice view of the control block array for Rust-side
        // indexed access.
        //
        // SAFETY: The caller guarantees the pointer is valid, properly aligned,
        // and points to `num_controls` contiguous elements with 'static lifetime.
        let control_blocks: &'static [WPP_PROJECT_CONTROL_BLOCK] = unsafe {
            core::slice::from_raw_parts(control_blocks_ptr, num_controls)
        };

        let etw_register_classic_provider =
            get_routine_addr!("EtwRegisterClassicProvider", EtwRegisterClassicProvider);

        let mut etw_unregister = None;

        if let Some(etw_register_provider) = etw_register_classic_provider {
            etw_unregister = get_routine_addr!("EtwUnregister", EtwUnregister);

            let mut wpp_reg: *mut WPP_TRACE_CONTROL_BLOCK =
                control_blocks_ptr.cast::<WPP_TRACE_CONTROL_BLOCK>();

            while !wpp_reg.is_null() {
                unsafe {
                    (*wpp_reg).RegHandle = 0;
                    let status = etw_register_provider(
                        (*wpp_reg).ControlGuid,
                        0,
                        WppClassicProviderCallback,
                        wpp_reg.cast::<core::ffi::c_void>(),
                        &raw mut (*wpp_reg).RegHandle,
                    );
                    println!("EtwRegisterClassicProvider status: {status:?}");
                    wpp_reg = (*wpp_reg).Next.cast_mut();
                }
            }
        }

        let wpp_trace_message = get_routine_addr!("WmiTraceMessage", WppTraceMessage);

        let trace_config = TraceConfig {
            control_blocks_ptr,
            control_blocks,
            wpp_trace_message,
            etw_unregister,
        };

        WppWriter {
            trace_config,
            wdm_driver,
            reg_path,
        }
    }

    /// Starts WPP tracing.
    pub fn start(&self) {
        let cb = self.trace_config.control_blocks_ptr;
        unsafe {
            WPP_GLOBAL_Control = cb;
            WppAutoLogStart(cb, self.wdm_driver, self.reg_path);
            WPP_RECORDER_INITIALIZED = WPP_GLOBAL_Control;
        }
    }

    /// Stops WPP tracing.
    pub fn stop(&self) {
        let cb = self.trace_config.control_blocks_ptr;
        unsafe {
            if let Some(etw_unregister) = self.trace_config.etw_unregister {
                let mut wpp_reg: *mut WPP_TRACE_CONTROL_BLOCK =
                    cb.cast::<WPP_TRACE_CONTROL_BLOCK>();

                while !wpp_reg.is_null() {
                    if (*wpp_reg).RegHandle != 0 {
                        let status = etw_unregister((*wpp_reg).RegHandle);
                        println!("EtwUnregister status: {status:?}");
                        (*wpp_reg).RegHandle = 0;
                    }
                    wpp_reg = (*wpp_reg).Next.cast_mut();
                }
            }

            WppAutoLogStop(cb, self.wdm_driver);

            WPP_RECORDER_INITIALIZED = ptr::null_mut();
            WPP_GLOBAL_Control = ptr::null_mut();
        }
    }

    /// Checks if the specified flag is enabled in the control block at `control_index`.
    #[inline]
    pub fn is_flag_enabled(&self, control_index: usize, flags: u32) -> bool {
        let control = unsafe { &self.trace_config.control_blocks[control_index].Control };
        control.Logger != 0 && (control.Flags[( (0xFFFF & (flags-1) ) / 32) as usize] & (1 << ((flags-1) & 31))) != 0
    }

    /// Checks if `AutoLogVerboseEnabled` is set for the control block at `control_index`.
    #[inline]
    pub fn is_auto_log_verbose_enabled(&self, control_index: usize) -> bool {
        let control = unsafe { &self.trace_config.control_blocks[control_index].Control };
        control.AutoLogVerboseEnabled != 0
    }

    /// Checks if the passed level is enabled for the control block at `control_index`.
    #[inline]
    pub fn is_level_enabled(&self, control_index: usize, level: u8) -> bool {
        let control = unsafe { &self.trace_config.control_blocks[control_index].Control };
        control.Logger != 0 && level <= control.Level
    }

    // Pre-generated `trace_0`..`trace_8` inherent methods.
    wpp_macros::define_trace_writer_methods!();
}

impl Drop for WppWriter {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Safety: whatever state `WppWriter` stores
/// in itself is never modified and is only
/// passed to WPP functions which are thread safe
unsafe impl Sync for WppWriter {}

// ---------------------------------------------------------------------------
// WPP control block types
// ---------------------------------------------------------------------------

/// Minimum-sized declaration of the WPP project control block.
#[repr(C)]
pub union WPP_PROJECT_CONTROL_BLOCK {
    pub Control: mem::ManuallyDrop<WPP_TRACE_CONTROL_BLOCK>,
    ReserveSpace: [UCHAR;
        mem::size_of::<WPP_TRACE_CONTROL_BLOCK>()],
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

pub(crate) struct TraceConfig {
    pub(crate) control_blocks_ptr: *mut WPP_PROJECT_CONTROL_BLOCK,
    pub(crate) control_blocks: &'static [WPP_PROJECT_CONTROL_BLOCK],
    pub(crate) wpp_trace_message: Option<WppTraceMessage>,
    pub(crate) etw_unregister: Option<EtwUnregister>,
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

extern "C" fn WppClassicProviderCallback(
    _Guid: LPCGUID,
    ControlCode: UCHAR,
    EnableContext: PVOID,
    CallbackContext: PVOID,
) {
    let trace_cb = CallbackContext.cast::<WPP_TRACE_CONTROL_BLOCK>();
    let trace_context = EnableContext.cast::<WPP_TRACE_ENABLE_CONTEXT>();

    if ControlCode != TraceControlCode::Enable as u8 && ControlCode != TraceControlCode::Disable as u8 {
        return;
    }

    unsafe {
        if ControlCode != TraceControlCode::Disable as u8 {
            (*trace_cb).Flags[0] = (*trace_context).EnableFlags;
            (*trace_cb).Level = (*trace_context).Level as UCHAR;
            (*trace_cb).Logger = *(trace_context.cast::<TRACEHANDLE>());

            imp_WppRecorderReplay(
                WPP_GLOBAL_Control.cast(),
                (*trace_cb).Logger,
                (*trace_context).EnableFlags,
                (*trace_context).Level,
            );
        } else {
            (*trace_cb).Level = 0;
            (*trace_cb).Flags[0] = 0;
            (*trace_cb).Logger = 0;
        }
    }
}

// Trace options used by WPP tracing - exposed for macro use
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
// WPP_WRITER static + accessor functions
// ---------------------------------------------------------------------------

static WPP_WRITER: UnsafeOnceCell<WppWriter> = UnsafeOnceCell::new();

/// Initializes WPP tracing. Called from `call_safe_driver_entry` in `wdf`.
///
/// Creates a `WppWriter`, registers providers, starts tracing, and stores
/// the writer in a global static.
///
/// # Safety
///
/// * `control_blocks` must point to a valid `WPP_MAIN_CB` array.
/// * `num_controls` must match the array length.
/// * Must be called from single-threaded DriverEntry context.
#[doc(hidden)]
pub unsafe fn init_tracing(
    control_blocks: *mut WPP_PROJECT_CONTROL_BLOCK,
    num_controls: usize,
    wdm_driver: *mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
) {
    let writer = unsafe { WppWriter::init(control_blocks, num_controls, wdm_driver, reg_path) };
    writer.start();
    // SAFETY: Single-threaded DriverEntry context.
    unsafe { WPP_WRITER.set(writer); }
}

/// Stops WPP tracing. Called from `wdf_driver_unload` in `wdf`.
#[doc(hidden)]
pub fn cleanup_tracing() {
    if let Some(writer) =
        // SAFETY: Not concurrent with set — called during driver unload.
        unsafe { WPP_WRITER.get() }
    {
        writer.stop();
    }
}

/// Returns a reference to the WPP writer, if initialized.
pub fn get_wpp_writer() -> Option<&'static WppWriter> {
    // SAFETY: Not concurrent with set — set runs once in DriverEntry.
    unsafe { WPP_WRITER.get() }
}

/// Returns the AutoLogContext pointer for WppAutoLogTrace calls.
#[doc(hidden)]
pub unsafe fn get_auto_log_context() -> Option<PVOID> {
    get_wpp_writer().map(|tw| {
        unsafe { &tw.trace_config.control_blocks[0].Control }.AutoLogContext
    })
}

/// Returns the TRACEHANDLE logger for WPP tracing.
#[doc(hidden)]
pub unsafe fn get_wpp_logger() -> Option<TRACEHANDLE> {
    get_wpp_writer().map(|tw| {
        unsafe { &tw.trace_config.control_blocks[0].Control }.Logger
    })
}

/// Returns the WppTraceMessage function pointer for WPP tracing.
#[doc(hidden)]
pub unsafe fn get_wpp_trace_message() -> Option<WppTraceMessage> {
    get_wpp_writer().and_then(|tw| tw.trace_config.wpp_trace_message)
}
