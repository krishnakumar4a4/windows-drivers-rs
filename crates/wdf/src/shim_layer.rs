use crate::api::*;
use wdk::println;

use wdk_sys::{
    GUID, LONG, LPCGUID, LPCSTR, LPGUID, NTSTATUS, NT_SUCCESS, DRIVER_OBJECT, PDRIVER_OBJECT,
    PREGHANDLE, PULONG, PUNICODE_STRING, PCUNICODE_STRING, PVOID, REGHANDLE, TRACEHANDLE,
    UCHAR, ULONG, ULONG64, UNICODE_STRING, USHORT, WDFDEVICE_INIT, WDFDRIVER, WDF_DRIVER_CONFIG,
    WDF_NO_HANDLE, WDFOBJECT, WDFREQUEST, WDF_NO_OBJECT_ATTRIBUTES, call_unsafe_wdf_function_binding
};

extern crate alloc;

use alloc::{string::String, vec::Vec, boxed::Box};

use core::{mem, ptr};


static mut EVT_DEVICE_ADD: Option<fn(_: &mut DeviceInit) -> Result<(), NtError>> = None;

static mut CONTROL_GUID: Option<Guid> = None;

static mut TRACING_CONFIG: Option<TracingConfig> = None;

const WPP_FLAG_LEN: UCHAR = 1;


/// These globals are expected by IFR functionality such as the
/// windbg extensions used to read IFR logs
#[no_mangle]
pub static mut WPP_GLOBAL_Control: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();

#[no_mangle]
pub static mut WPP_RECORDER_INITIALIZED: *mut WPP_PROJECT_CONTROL_BLOCK = ptr::null_mut();


extern "C" {
    fn MmGetSystemRoutineAddress( SystemRoutineName: PUNICODE_STRING ) -> PVOID;
}

macro_rules! get_routine_addr {
    ($name:expr, $callback_type:ty) => {{
        let name = to_utf16_buf($name);
        let mut name = to_unicode_string(name.as_ref());
        let addr = unsafe { MmGetSystemRoutineAddress(&mut name as *mut _) };
        unsafe { mem::transmute::<PVOID, Option<$callback_type>>(addr) }
    }};
}

/// Represents a driver
pub struct Driver {
    driver_obj: PDRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
}


impl Driver {
    /// Registers a callback for the `EvtDeviceAdd` event
    pub fn on_evt_device_add(&self, callback: fn(&mut DeviceInit) -> Result<(), NtError>) {
        unsafe {
            EVT_DEVICE_ADD = Some(callback);
        }
    }

    /// Enables tracing for the driver
    pub fn enable_tracing(&mut self, control_guid: Guid) {
        unsafe {
            CONTROL_GUID = Some(control_guid);
        }
        let mut control_block = WPP_TRACE_CONTROL_BLOCK {
            Callback: None,
            ControlGuid: unsafe { CONTROL_GUID.as_ref().unwrap().as_lpcguid() },
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
        };

        let wpp_trace_message= get_routine_addr!("WmiTraceMessage", WppTraceMessage);

        let etw_register_classic_provider = get_routine_addr!("EtwRegisterClassicProvider", EtwRegisterClassicProvider);

        let mut etw_unregister = None;
        if let Some(etw_register_provider) = etw_register_classic_provider {
            etw_unregister = get_routine_addr!("EtwUnregister", EtwUnregister);

            etw_register_provider(
                control_block.ControlGuid,
                0,
                WppClassicProviderCallback,
                unsafe { mem::transmute(&mut control_block) },
                &mut control_block.RegHandle,
            );
        }

        unsafe {
            TRACING_CONFIG = Some(TracingConfig {
                control_block: WPP_PROJECT_CONTROL_BLOCK { Control: mem::ManuallyDrop::new(control_block) },
                wpp_trace_message,
                etw_unregister,
            });

            WPP_GLOBAL_Control = &mut (TRACING_CONFIG.as_mut().unwrap().control_block) as *mut _;
            WPP_RECORDER_INITIALIZED = WPP_GLOBAL_Control;

            WppAutoLogStart(&mut (TRACING_CONFIG.as_mut().unwrap().control_block) as *mut _, self.driver_obj, self.reg_path);
        }
    }
}



#[link_section = "PAGE"]
extern "C" fn evt_driver_device_add(
    _driver: WDFDRIVER,
    device_init: *mut WDFDEVICE_INIT,
) -> NTSTATUS {
    if let Some(cb) = unsafe { EVT_DEVICE_ADD } {
        let mut device_init = DeviceInit::from(device_init);
        match cb(&mut device_init) {
            Ok(_) => 0,
            Err(e) => e.ntstatus(),
        }
    } else {
        0
    }
}

extern "C" fn driver_unload(driver: *mut DRIVER_OBJECT) {
    // println!("Goodbye World!");
    // println!("Driver Exit Complete!");

    clean_up_tracing(driver);
}

/// Calls the safe driver entry function
#[doc(hidden)]
pub fn call_safe_driver_entry(
    driver: &mut DRIVER_OBJECT,
    registry_path: PCUNICODE_STRING,
    safe_entry: fn(&mut Driver, &str) -> Result<(), i32>,
) -> NTSTATUS {
    driver.DriverUnload = Some(driver_unload);

    let mut driver_config = WDF_DRIVER_CONFIG {
        Size: mem::size_of::<WDF_DRIVER_CONFIG>() as ULONG,
        EvtDriverDeviceAdd: Some(evt_driver_device_add),
        ..WDF_DRIVER_CONFIG::default()
    };

    let driver_handle_output = WDF_NO_HANDLE.cast::<WDFDRIVER>();

    let nt_status = unsafe { call_unsafe_wdf_function_binding!(
        WdfDriverCreate,
        driver as PDRIVER_OBJECT,
        registry_path,
        WDF_NO_OBJECT_ATTRIBUTES,
        &mut driver_config,
        driver_handle_output
    ) };

    if !NT_SUCCESS(nt_status) {
        println!("WdfDriverCreate failed. status: {nt_status:#02x}");
        return nt_status;
    }

    let mut safe_driver = Driver { driver_obj: driver, reg_path: registry_path  };

    // Translate UTF16 string to rust string
    let registry_path: UNICODE_STRING =
        // SAFETY: This dereference is safe since `registry_path` is:
        //         * provided by `DriverEntry` and is never null
        //         * a valid pointer to a `UNICODE_STRING`
        unsafe { *registry_path };

    let registry_path = to_rust_str(registry_path);
    match safe_entry(&mut safe_driver, &registry_path) {
        Ok(_) => 0,
        Err(nt_status) => {
            clean_up_tracing(driver);
            nt_status
        },
    }
}

struct Reqeust(WDFREQUEST);

impl WdfObject for Reqeust {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self(inner as _)
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0 as _
    }
}

pub fn trace(message: &str) {
    if let Some(tracing_config) = unsafe { TRACING_CONFIG.as_ref() } {
        let message_c_str = alloc::ffi::CString::new(message).unwrap();

        wpp_recorder_sf_ds(
            unsafe { tracing_config.control_block.Control.AutoLogContext },
            0,
            0,
            0,
            &TRACE_GUID,
            42, // printing an arbitrary number for now
            message_c_str.as_ptr(),
        );
    }
}

#[derive(Debug)]
pub struct Guid(GUID);

impl Guid {
     pub fn parse(guid_str: &str) -> Result<Self, &'static str> {
        // Remove dashes from the input string
        let guid_str = guid_str.replace("-", "");

        let err = "Invalid GUID format";

        if guid_str.len() != 32 {
            return Err(err);
        }

        let data1 = u32::from_str_radix(&guid_str[0..8], 16).map_err(|_| err)?;
        let data2 = u16::from_str_radix(&guid_str[8..12], 16).map_err(|_| err)?;
        let data3 = u16::from_str_radix(&guid_str[12..16], 16).map_err(|_| err)?;

        let mut data4 = [0u8; 8];
        for i in 0..8 {
            data4[i] = u8::from_str_radix(&guid_str[16 + i * 2..18 + i * 2], 16).map_err(|_| err)?;
        }

        Ok(Guid(GUID {
                Data1: data1,
                Data2: data2,
                Data3: data3,
                Data4: data4,
            },
        ))
    }

    pub fn as_lpcguid(&self) -> *const GUID {
        &self.0
    }
}


#[repr(C)]
pub union WPP_PROJECT_CONTROL_BLOCK {
    Control: mem::ManuallyDrop<WPP_TRACE_CONTROL_BLOCK>,
    ReserveSpace: [UCHAR; mem::size_of::<WPP_TRACE_CONTROL_BLOCK>() + mem::size_of::<ULONG>() * (WPP_FLAG_LEN as usize - 1)]
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
    EnableFlags: ULONG
}


struct TracingConfig {
    control_block: WPP_PROJECT_CONTROL_BLOCK,
    wpp_trace_message: Option<WppTraceMessage>,
    etw_unregister: Option<EtwUnregister>,
}


type WMIENTRY_NEW = extern "C" fn (MinorFunction: UCHAR, DataPath: PVOID, BufferLength: ULONG, Buffer: PVOID, Context: PVOID, Size: PULONG) -> u64;
type WppTraceMessage = extern "C" fn(LoggerHandle: ULONG64, MessageFlags: ULONG, MessageGuid: LPCGUID, MessageNumber: USHORT, ...) -> LONG;
type EtwClassicCallback = extern "C" fn(Guid: LPCGUID, ControlCode: UCHAR, EnableContext: PVOID, CallbackContext: PVOID);
type EtwRegisterClassicProvider = extern "C" fn(ProviderGuid: LPCGUID, Type: ULONG, EnableCallback:  EtwClassicCallback, CallbackContext: PVOID, RegHandle: PREGHANDLE) -> NTSTATUS;
type EtwUnregister = extern "C" fn (RegHandle: REGHANDLE ) -> NTSTATUS;

extern "C" {
    fn WppAutoLogStart(WppCb: *mut WPP_PROJECT_CONTROL_BLOCK, DrvObj: PDRIVER_OBJECT, RegPath: PCUNICODE_STRING);
    fn WppAutoLogStop(WppCb: *mut WPP_PROJECT_CONTROL_BLOCK, DrvObj: PDRIVER_OBJECT);
    fn WppAutoLogTrace(AutoLogContext: PVOID, MessageLevel: UCHAR, MessageFlags: ULONG, MessageGuid: LPGUID, MessageNumber: USHORT, ...);
    fn imp_WppRecorderReplay(WppCb: PVOID, WppTraceHandle: TRACEHANDLE, EnableFlags: ULONG, EnableLevel: UCHAR);
}

extern "C" fn WppClassicProviderCallback(_Guid: LPCGUID, ControlCode: UCHAR, EnableContext: PVOID, CallbackContext: PVOID) {
    let TraceCb = CallbackContext as *mut WPP_TRACE_CONTROL_BLOCK;
    let TraceContext = EnableContext as *mut WPP_TRACE_ENABLE_CONTEXT;

    if ControlCode != 1 && ControlCode != 0 {
        return;
    }

    unsafe {
        if ControlCode != 0 {
            (*TraceCb).Flags[0] = (*TraceContext).EnableFlags;
            (*TraceCb).Level = (*TraceContext).Level as UCHAR;
            (*TraceCb).Logger = *(TraceContext as *const TRACEHANDLE);

            imp_WppRecorderReplay(TraceCb as PVOID, (*TraceCb).Logger, (*TraceContext).EnableFlags, (*TraceContext).Level);
        } else {
            (*TraceCb).Level = 0;
            (*TraceCb).Flags[0] = 0;
            (*TraceCb).Logger = 0;
        }
    }
}

fn to_rust_str(unicode_str: UNICODE_STRING) -> String {
    let unicode_slice = unsafe { core::slice::from_raw_parts(unicode_str.Buffer, unicode_str.Length as usize / 2) };
    String::from_utf16_lossy(unicode_slice)
}

fn to_utf16_buf(rust_str: &str) -> Box<[u16]> {
    let utf16_vec = rust_str.encode_utf16()
        .chain(core::iter::once(0)) // Append null terminator
        .collect::<Vec<_>>();
    utf16_vec.into_boxed_slice()
}

fn to_unicode_string(buf: &[u16]) -> UNICODE_STRING {
    let byte_len = (buf.len() * 2) as u16;
    UNICODE_STRING {
        Length: byte_len - 2, // Length excluding the null terminator
        MaximumLength: byte_len,
        Buffer: buf.as_ptr() as *mut _,
    }
}


fn clean_up_tracing(driver: *mut DRIVER_OBJECT) {
    unsafe {
        if let Some(tracing_config) = TRACING_CONFIG.as_mut() {
            if let Some(etw_unregister) = tracing_config.etw_unregister {
                etw_unregister(tracing_config.control_block.Control.RegHandle);
            }
            WppAutoLogStop(&mut tracing_config.control_block as *mut _, driver);
        }
    }
}

const WPP_TRACE_OPTIONS: ULONG = 1 | 2 | 32 | 8;
const TRACE_GUID: GUID = GUID { Data1: 0xe7602a7b, Data2: 0x5034, Data3: 0x321b, Data4: [0xd4, 0x50, 0xa9, 0x86, 0x11, 0x3f, 0xc2, 0xe1] };

extern "C" {
    fn strlen(str: *const core::ffi::c_char) -> usize;
}

fn wpp_recorder_sf_ds(
    AutoLogContext: PVOID,
    level: UCHAR,
    flags: ULONG,
    id: USHORT,
    traceGuid:LPCGUID,
    a1: i32,
    a2: LPCSTR
    )
{
    let a1_len = mem::size_of::<i32>() as usize;


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
        if let Some(tracing_config) = TRACING_CONFIG.as_ref() {
            if let Some(wpp_trace_message) = tracing_config.wpp_trace_message {

                println!("wpp_recorder_sf_ds: calling wpp_trace_message");
                let cb = &tracing_config.control_block;
                wpp_trace_message(cb.Control.Logger, WPP_TRACE_OPTIONS, traceGuid, id, &a1, a1_len, a2, a2_len, 0);
            }
        }

        println!("wpp_recorder_sf_ds: calling WppAutoLogTrace");
        WppAutoLogTrace(AutoLogContext, level, flags,  traceGuid as *mut _, id, &a1, a1_len, a2, a2_len, 0);
    }
}