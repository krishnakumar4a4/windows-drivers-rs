use core::{mem, ptr};

use wdk_sys::{
    DRIVER_OBJECT,
    GUID,
    LPCGUID,
    LPCSTR,
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

use crate::api::string::UnicodeString;

use crate::println;

/// These globals are expected by IFR functionality such as the
/// windbg extensions used to read IFR logs
#[unsafe(no_mangle)]
pub static mut WPP_GLOBAL_Control: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();

#[unsafe(no_mangle)]
pub static mut WPP_RECORDER_INITIALIZED: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();

// Trace control codes used by WPP classic provider callback
// These were originally defined in evntrace.h as below
// #define EVENT_CONTROL_CODE_DISABLE_PROVIDER 0
// #define EVENT_CONTROL_CODE_ENABLE_PROVIDER  1
// #define EVENT_CONTROL_CODE_CAPTURE_STATE    2
// The enum corresponds to these control codes and is used in the WppClassicProviderCallback function
enum TraceControlCode {
    Enable = 1,
    Disable = 0,
    CaptureState = 2,
}

unsafe extern "C" {
    fn MmGetSystemRoutineAddress(SystemRoutineName: PUNICODE_STRING) -> PVOID;
    /// C runtime strlen function - exposed for macro use
    pub fn strlen(str: *const core::ffi::c_char) -> usize;
}

macro_rules! get_routine_addr {
    ($name:expr, $callback_type:ty) => {{
        let name_unicode_string = UnicodeString::from_rust_str($name);
        let name_unicode_string_raw = name_unicode_string.as_raw();

        let addr = unsafe {
            MmGetSystemRoutineAddress((name_unicode_string_raw as *const UNICODE_STRING).cast_mut())
        };
        unsafe { mem::transmute::<PVOID, Option<$callback_type>>(addr) }
    }};
}

/// A writer for WPP tracing
///
/// It is a safe wrapper around the WPP tracing system.
/// It maintains data necessary to call WPP functions such
/// as the control block. This data is passed to WPP functions
/// as `*mut T` sometimes (e.g. in the [`start`] function)
/// even though the wrapper function itself (i.e. [`start`])
/// takes `&self` instead of `&mut self`.
/// This is safe because the WPP functions are supposed to modify
/// the passed data in a thread-safe manner and `TraceWriter` itself
/// never modifies them.
/// In this way `TraceWriter` implements interior mutability.
pub struct TraceWriter {
    pub(crate) trace_config: TraceConfig,
    wdm_driver: *mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
}

impl TraceWriter {
    /// Creates a new `TraceWriter` and registers all trace providers with ETW.
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
        // indexed access. The array lives in a module-level static so it
        // outlives the TraceWriter.
        //
        // SAFETY: The caller guarantees the pointer is valid, properly aligned,
        // and points to `num_controls` contiguous elements with 'static lifetime.
        let control_blocks: &'static [WPP_PROJECT_CONTROL_BLOCK] = unsafe {
            core::slice::from_raw_parts(control_blocks_ptr, num_controls)
        };

        let etw_register_classic_provider =
            get_routine_addr!("EtwRegisterClassicProvider", EtwRegisterClassicProvider);

        let mut etw_unregister = None;

        // Walk the linked list of control blocks and register each provider,
        // mirroring the WppInitKm loop:
        //   WppReg = &WPP_CB[0].Control;
        //   while (WppReg) { EtwRegisterClassicProvider(...); WppReg = WppReg->Next; }
        //
        // Raw pointers are required here because the C EtwRegisterClassicProvider
        // API takes PVOID context and PREGHANDLE output parameters.
        if let Some(etw_register_provider) = etw_register_classic_provider {
            etw_unregister = get_routine_addr!("EtwUnregister", EtwUnregister);

            // Start at &WPP_CB[0].Control — since Control is at offset 0 and
            // ManuallyDrop is repr(transparent), the union pointer equals the
            // WPP_TRACE_CONTROL_BLOCK pointer.
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

        TraceWriter {
            trace_config,
            wdm_driver,
            reg_path,
        }
    }

    /// Starts WPP tracing.
    ///
    /// Sets `WPP_GLOBAL_Control` to point to the control block array, calls
    /// `WppAutoLogStart` with `&WPP_CB[0]`, and marks the recorder as
    /// initialised. This matches the tail of the C `WPP_INIT_TRACING` macro.
    pub fn start(&self) {
        let cb = self.trace_config.control_blocks_ptr;
        unsafe {
            WPP_GLOBAL_Control = cb;
            WppAutoLogStart(cb, self.wdm_driver, self.reg_path);
            WPP_RECORDER_INITIALIZED = WPP_GLOBAL_Control;
        }
    }

    /// Stops WPP tracing.
    ///
    /// Walks the linked list of control blocks and calls `EtwUnregister` for
    /// each, then calls `WppAutoLogStop`. This mirrors C `WppCleanupKm`.
    pub fn stop(&self) {
        let cb = self.trace_config.control_blocks_ptr;
        unsafe {
            // Walk the linked list and unregister each provider.
            // Raw pointers required for the C EtwUnregister FFI call.
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
    ///
    /// Each trace control definition occupies one entry in the `WPP_MAIN_CB` array.
    /// The `control_index` selects which entry to check, mirroring the C macro
    /// `WPP_CONTROL(CTL)` which indexes into `WPP_CB[WPP_CTRL_NO(CTL)]`.
    ///
    /// Uses safe slice indexing on the Rust side — no raw pointer arithmetic.
    #[inline]
    pub fn is_flag_enabled(&self, control_index: usize, flags: u32) -> bool {
        // SAFETY: Union field access requires unsafe. Reading `.Control` is
        // sound because the array was initialised with valid ManuallyDrop
        // values and the C WPP runtime updates these fields thread-safely.
        let control = unsafe { &self.trace_config.control_blocks[control_index].Control };
        control.Logger != 0 && (control.Flags[( (0xFFFF & (flags-1) ) / 32) as usize] & (1 << ((flags-1) & 31))) != 0
    }

    /// Checks if `AutoLogVerboseEnabled` is set for the control block at `control_index`.
    #[inline]
    pub fn is_auto_log_verbose_enabled(&self, control_index: usize) -> bool {
        let control = unsafe { &self.trace_config.control_blocks[control_index].Control };
        control.AutoLogVerboseEnabled != 0
    }

    /// Checks if the passed level is enabled (at or below the control block's level)
    /// for the control block at `control_index`.
    ///
    /// A trace message fires when `level <= control.Level`. Lower numeric values
    /// are more severe (Critical=1, Error=2, …).
    #[inline]
    pub fn is_level_enabled(&self, control_index: usize, level: u8) -> bool {
        let control = unsafe { &self.trace_config.control_blocks[control_index].Control };
        control.Logger != 0 && level <= control.Level
    }

    // Pre-generated `trace_0`..`trace_8` inherent methods. Each method is
    // generic over its argument types (each constrained by `TraceArgData`)
    // and feeds the values to WPP / AutoLog as `(*const u8, usize)` pairs.
    // The `trace!` macro picks the matching arity and calls into one of
    // these instead of synthesising a new trait+impl per call site.
    wdf_macros::define_trace_writer_methods!();
}

impl Drop for TraceWriter {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Safety: whatever state `TraceWriter` stores
/// in itself is never modified and is only
/// passed to WPP functions which are thread safe
/// and hence the TraceWriter can be `Sync`
unsafe impl Sync for TraceWriter {}

/// Minimum-sized declaration of the WPP project control block.
///
/// The actual `WPP_PROJECT_CONTROL_BLOCK` instance with the correctly-sized
/// `ReserveSpace` (accounting for `WPP_FLAG_LEN`) is generated by the
/// `#[driver_entry]` proc-macro in the user's driver crate. That
/// macro-generated union has a larger `ReserveSpace` array
/// (`sizeof(WPP_TRACE_CONTROL_BLOCK) + sizeof(ULONG) * (WPP_FLAG_LEN - 1)`)
/// and its pointer is cast to `*mut WPP_PROJECT_CONTROL_BLOCK` before being
/// passed into this crate.
///
/// This declaration exists so that the `wdf` crate can define functions,
/// statics, and extern signatures that operate on
/// `*mut WPP_PROJECT_CONTROL_BLOCK` without depending on the driver-specific
/// flag count. All field access goes through the `Control` variant, whose
/// layout is identical regardless of the outer union's reserve size.
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
    /// Raw pointer to the first element of the `WPP_MAIN_CB` array.
    /// Used when calling C WPP APIs (`WppAutoLogStart`, `WppAutoLogStop`,
    /// `EtwRegisterClassicProvider`, etc.) which expect `*mut WPP_PROJECT_CONTROL_BLOCK`.
    pub(crate) control_blocks_ptr: *mut WPP_PROJECT_CONTROL_BLOCK,
    /// Safe slice view of the control block array for Rust-side indexed access
    /// (e.g. `is_flag_enabled`, `is_level_enabled`). The slice has the same
    /// lifetime as the static `WPP_MAIN_CB` array.
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
        // Explicitly ignore Capture state control code
        return;
    }

    unsafe {
        if ControlCode != TraceControlCode::Disable as u8 {
            (*trace_cb).Flags[0] = (*trace_context).EnableFlags;
            (*trace_cb).Level = (*trace_context).Level as UCHAR;
            (*trace_cb).Logger = *(trace_context.cast::<TRACEHANDLE>());

            // TODO: This should be called only when (NTDDI_VERSION >= NTDDI_WIN10_RS1)
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
// These are defined in evntrace.h as follows:
// //
// // Flags used by WMI Trace Message
// // Note that the order or value of these flags should NOT be changed as they are processed
// // in this order.
// // #define TRACE_MESSAGE_SEQUENCE              1  // Message should include a sequence number
// // #define TRACE_MESSAGE_GUID                  2  // Message includes a GUID
// // #define TRACE_MESSAGE_COMPONENTID           4  // Message has no GUID, Component ID instead
// // #define TRACE_MESSAGE_TIMESTAMP             8  // Message includes a timestamp
// // #define TRACE_MESSAGE_PERFORMANCE_TIMESTAMP 16 // *Obsolete* Clock type is controlled by the logger
// // #define TRACE_MESSAGE_SYSTEMINFO            32 // Message includes system information TID,PID
const TRACE_MESSAGE_SEQUENCE: ULONG = 1;
const TRACE_MESSAGE_GUID: ULONG = 2;
const TRACE_MESSAGE_SYSTEMINFO: ULONG = 32;
const TRACE_MESSAGE_TIMESTAMP: ULONG = 8;
const TRACE_MESSAGE_COMPONENTID: ULONG = 4;
const TRACE_MESSAGE_PERFORMANCE_TIMESTAMP: ULONG = 16;

#[doc(hidden)]
pub const WPP_TRACE_OPTIONS: ULONG = TRACE_MESSAGE_SEQUENCE 
                                    | TRACE_MESSAGE_GUID 
                                    | TRACE_MESSAGE_SYSTEMINFO 
                                    | TRACE_MESSAGE_TIMESTAMP;