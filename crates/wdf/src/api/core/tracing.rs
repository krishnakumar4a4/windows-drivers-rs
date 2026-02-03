use alloc::boxed::Box;
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

use crate::api::{guid::Guid, string::UnicodeString};

/// These globals are expected by IFR functionality such as the
/// windbg extensions used to read IFR logs
#[unsafe(no_mangle)]
pub static mut WPP_GLOBAL_Control: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();

#[unsafe(no_mangle)]
pub static mut WPP_RECORDER_INITIALIZED: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();

pub static mut WPP_MAIN_CB: WPP_PROJECT_CONTROL_BLOCK = WPP_PROJECT_CONTROL_BLOCK {
    Control: mem::ManuallyDrop::new(WPP_TRACE_CONTROL_BLOCK {
        Callback: None,
        ControlGuid: ptr::null(),
        Next: ptr::null(),
        Logger: 0,
        RegistryPath: ptr::null_mut(),
        FlagsLen: WPP_FLAG_LEN,
        Level: 0,
        Reserved: 0,
        Flags: [0; 1],
        ReservedFlags: 0,
        RegHandle: 0,
        AutoLogContext: ptr::null_mut(),
        AutoLogVerboseEnabled: 0,
        AutoLogAttachToMiniDump: 0,
    }),
};

const WPP_FLAG_LEN: UCHAR = 1;

unsafe extern "C" {
    fn MmGetSystemRoutineAddress(SystemRoutineName: PUNICODE_STRING) -> PVOID;
    fn strlen(str: *const core::ffi::c_char) -> usize;
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
    _control_guid: Box<Guid>, /* This field exists only ensure it stays alive because its
                               * address is used in trace_config */
    trace_config: TraceConfig,
    wdm_driver: *mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
}

impl TraceWriter {
    /// Creates a new TraceWriter instance and initializes WPP tracing.
    ///
    /// # Safety
    ///
    /// The pointers `wdm_driver` and `reg_path` must be valid.
    /// 
    /// This function is implemented in a hacky way to test tracing. It should
    /// only be called once since it sets up global state for WPP tracing.
    /// Calling it multiple times may lead to undefined behavior due to multiple
    /// initializations of the WPP control block and potential conflicts in the
    /// global state.
    /// 
    /// TODO: Create a proper singleton pattern to ensure only one instance
    /// of `TraceWriter` can exist at a time.
    pub unsafe fn init(
        control_guid: Guid,
        wdm_driver: *mut DRIVER_OBJECT,
        reg_path: PCUNICODE_STRING,
    ) -> Self {
        // Boxing control_guid to ensure it has a stable address
        // which we can use in WPP_PROJECT_CONTROL_BLOCK::ControlBlock below
        let control_guid = Box::new(control_guid);

        unsafe {
            (*WPP_MAIN_CB.Control).ControlGuid = control_guid.as_lpcguid();
        }

        // TODO: check if any of these functions can fail and handle errors accordingly

        let etw_register_classic_provider =
            get_routine_addr!("EtwRegisterClassicProvider", EtwRegisterClassicProvider);

        let mut etw_unregister = None;
        if let Some(etw_register_provider) = etw_register_classic_provider {
            etw_unregister = get_routine_addr!("EtwUnregister", EtwUnregister);

            let control = unsafe { &mut *WPP_MAIN_CB.Control };
            etw_register_provider(
                control.ControlGuid,
                0,
                WppClassicProviderCallback,
                unsafe { mem::transmute(&mut *control) },
                &mut control.RegHandle,
            );
        }

        let wpp_trace_message = get_routine_addr!("WmiTraceMessage", WppTraceMessage);

        let trace_config = TraceConfig {
            control_block: &raw mut WPP_MAIN_CB,
            wpp_trace_message,
            etw_unregister,
        };

        TraceWriter {
            _control_guid: control_guid,
            trace_config,
            wdm_driver,
            reg_path,
        }
    }

    /// Starts WPP tracing
    pub fn start(&self) {
        let control_block_ptr = self.trace_config.control_block;
        unsafe {
            WPP_GLOBAL_Control = control_block_ptr;
            WppAutoLogStart(control_block_ptr, self.wdm_driver, self.reg_path);
            WPP_RECORDER_INITIALIZED = WPP_GLOBAL_Control;
        }
    }

    /// Stops WPP tracing
    pub fn stop(&self) {
        let control_block_ptr = self.trace_config.control_block;
        unsafe {
            if let Some(etw_unregister) = self.trace_config.etw_unregister {
                etw_unregister((&(*control_block_ptr).Control).RegHandle);
            }

            WppAutoLogStop(control_block_ptr, self.wdm_driver);
        }
    }

    /// Writes a trace message to the WPP tracing system.
    ///
    /// This function is used to log messages with the WPP tracing system.
    /// It requires a valid tracing context to be set up beforehand.
    pub fn write(&self, message: &str) {
        let message_c_str = alloc::ffi::CString::new(message).unwrap();

        self.wpp_recorder_sf_ds(
            0,
            0,
            0,
            &TRACE_GUID,
            42, // printing an arbitrary number for now
            message_c_str.as_ptr(),
        );
    }

    fn wpp_recorder_sf_ds(
        &self,
        level: UCHAR,
        flags: ULONG,
        id: USHORT,
        traceGuid: LPCGUID,
        a1: i32,
        a2: LPCSTR,
    ) {
        let a1_len = mem::size_of::<i32>();

        let null_c_str = alloc::ffi::CString::new("NULL").unwrap();
        let a2 = if !a2.is_null() {
            a2
        } else {
            null_c_str.as_ptr()
        };

        let a2_len = if !a2.is_null() {
            unsafe { strlen(a2) + 1 }
        } else {
            5 // length of "NULL" + terminator
        };

        unsafe {
            let control_block = &(*self.trace_config.control_block).Control;
            if let Some(wpp_trace_message) = self.trace_config.wpp_trace_message {
                wpp_trace_message(
                    control_block.Logger,
                    WPP_TRACE_OPTIONS,
                    traceGuid,
                    id,
                    &a1,
                    a1_len,
                    a2,
                    a2_len,
                    core::ptr::null::<core::ffi::c_void>(), // sentinel value for variadic args
                );
            }

            WppAutoLogTrace(
                control_block.AutoLogContext,
                level,
                flags,
                traceGuid.cast_mut().cast(),
                id,
                &a1,
                a1_len,
                a2,
                a2_len,
                core::ptr::null::<core::ffi::c_void>(), // sentinel value for variadic args
            );
        }
    }
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

#[repr(C)]
pub union WPP_PROJECT_CONTROL_BLOCK {
    Control: mem::ManuallyDrop<WPP_TRACE_CONTROL_BLOCK>,
    ReserveSpace: [UCHAR;
        mem::size_of::<WPP_TRACE_CONTROL_BLOCK>()
            + mem::size_of::<ULONG>() * (WPP_FLAG_LEN as usize - 1)],
}

#[repr(C)]
pub struct WPP_TRACE_CONTROL_BLOCK {
    Callback: Option<WMIENTRY_NEW>,
    ControlGuid: LPCGUID,
    Next: *const WPP_TRACE_CONTROL_BLOCK,
    Logger: TRACEHANDLE,
    RegistryPath: PUNICODE_STRING,
    FlagsLen: UCHAR,
    Level: UCHAR,
    Reserved: USHORT,
    Flags: [ULONG; 1],
    ReservedFlags: ULONG,
    RegHandle: REGHANDLE,
    AutoLogContext: PVOID,
    AutoLogVerboseEnabled: USHORT,
    AutoLogAttachToMiniDump: USHORT,
}

#[repr(C)]
struct WPP_TRACE_ENABLE_CONTEXT {
    LoggerId: USHORT,
    Level: UCHAR,
    InternalFlag: UCHAR,
    EnableFlags: ULONG,
}

struct TraceConfig {
    control_block: *mut WPP_PROJECT_CONTROL_BLOCK,
    wpp_trace_message: Option<WppTraceMessage>,
    etw_unregister: Option<EtwUnregister>,
}

type WMIENTRY_NEW = extern "C" fn(
    MinorFunction: UCHAR,
    DataPath: PVOID,
    BufferLength: ULONG,
    Buffer: PVOID,
    Context: PVOID,
    Size: PULONG,
) -> u64;
type WppTraceMessage = extern "C" fn(
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
    fn WppAutoLogTrace(
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
    let TraceCb = CallbackContext.cast::<WPP_TRACE_CONTROL_BLOCK>();
    let TraceContext = EnableContext.cast::<WPP_TRACE_ENABLE_CONTEXT>();

    if ControlCode != 1 && ControlCode != 0 {
        return;
    }

    unsafe {
        if ControlCode != 0 {
            (*TraceCb).Flags[0] = (*TraceContext).EnableFlags;
            (*TraceCb).Level = (*TraceContext).Level as UCHAR;
            (*TraceCb).Logger = *(TraceContext.cast::<TRACEHANDLE>());

            imp_WppRecorderReplay(
                TraceCb as PVOID,
                (*TraceCb).Logger,
                (*TraceContext).EnableFlags,
                (*TraceContext).Level,
            );
        } else {
            (*TraceCb).Level = 0;
            (*TraceCb).Flags[0] = 0;
            (*TraceCb).Logger = 0;
        }
    }
}

const WPP_TRACE_OPTIONS: ULONG = 1 | 2 | 32 | 8;
const TRACE_GUID: GUID = GUID {
    Data1: 0xE7602A7B,
    Data2: 0x5034,
    Data3: 0x321B,
    Data4: [0xD4, 0x50, 0xA9, 0x86, 0x11, 0x3F, 0xC2, 0xE1],
};
