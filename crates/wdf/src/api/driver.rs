extern crate alloc;

use alloc::string::String;
use core::{mem, ptr};

use wdk_sys::{
    call_unsafe_wdf_function_binding,
    PDRIVER_OBJECT,
    ULONG,
    UNICODE_STRING,
    WDFDEVICE_INIT,
    WDFDRIVER,
    WDF_DRIVER_CONFIG,
    WDF_DRIVER_VERSION_AVAILABLE_PARAMS,
    WDF_NO_OBJECT_ATTRIBUTES,
};
#[doc(hidden)]
pub use wdk_sys::{
    DRIVER_OBJECT,
    NTSTATUS,
    NT_SUCCESS,
    PCUNICODE_STRING,
    WDFOBJECT,
    WDF_OBJECT_ATTRIBUTES,
    WDF_OBJECT_CONTEXT_TYPE_INFO,
};

use crate::api::{
    error::NtResult,
    guid::Guid,
    object::wdf_struct_size,
    string::{to_rust_str, WString},
    sync::AtomicOnceCell,
    tracing::TraceWriter,
    *,
};

static TRACE_WRITER: AtomicOnceCell<TraceWriter> = AtomicOnceCell::new();
static EVT_DEVICE_ADD: AtomicOnceCell<fn(&mut DeviceInit) -> Result<(), NtError>> =
    AtomicOnceCell::new();

/// Represents a driver
pub struct Driver {
    wdm_driver: PDRIVER_OBJECT,
    wdf_driver: WDFDRIVER,
    reg_path: PCUNICODE_STRING,
}

impl Driver {
    pub fn retrieve_version_string(&self) -> NtResult<String> {
        let string = WString::create()?;

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDriverRetrieveVersionString,
                self.wdf_driver,
                string.as_ptr() as *mut _,
            )
        };

        if !NT_SUCCESS(status) {
            return Err(status.into());
        }

        Ok(string.to_rust_string())
    }

    pub fn is_version_available(&self, major_vesion: u32, minor_version: u32) -> bool {
        let mut params = WDF_DRIVER_VERSION_AVAILABLE_PARAMS {
            Size: wdf_struct_size!(WDF_DRIVER_VERSION_AVAILABLE_PARAMS),
            MajorVersion: major_vesion,
            MinorVersion: minor_version,
        };

        let res = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDriverIsVersionAvailable,
                self.wdf_driver,
                &raw mut params,
            )
        };

        res != 0
    }

    /// Registers a callback for the `EvtDeviceAdd` event
    pub fn on_evt_device_add(
        &mut self,
        callback: fn(&mut DeviceInit) -> Result<(), NtError>,
    ) -> NtResult<()> {
        if EVT_DEVICE_ADD.get().is_some() {
            return Err(NtError::from(1)); // TODO: Change this to a proper error
                                          // code
        }

        EVT_DEVICE_ADD.set(callback);

        Ok(())
    }

    /// Enables tracing for the driver
    pub fn enable_tracing(&mut self, control_guid: Guid) -> NtResult<()> {
        if TRACE_WRITER.get().is_some() {
            return Err(NtError::from(1)); // TODO: Change this to a proper error
                                          // code
        }

        let trace_writer =
            unsafe { TraceWriter::init(control_guid, self.wdm_driver, self.reg_path) };

        trace_writer.start();

        TRACE_WRITER.set(trace_writer);

        Ok(())
    }
}

fn cleanup_tracing() {
    if let Some(trace_writer) = TRACE_WRITER.get() {
        trace_writer.stop();
    }
}

/// Calls the safe driver entry function
///
/// It is meant to be called by the driver entry function generated
/// in the user's driver code by the `driver_entry` procedural
/// macro attribute
#[doc(hidden)]
pub fn call_safe_driver_entry(
    wdm_driver: &mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
    safe_entry: fn(&mut Driver, &str) -> Result<(), NtError>,
) -> NTSTATUS {
    wdm_driver.DriverUnload = Some(driver_unload);

    let mut driver_config = WDF_DRIVER_CONFIG {
        Size: mem::size_of::<WDF_DRIVER_CONFIG>() as ULONG,
        EvtDriverDeviceAdd: Some(evt_driver_device_add),
        ..WDF_DRIVER_CONFIG::default()
    };

    let mut wdf_driver: WDFDRIVER = ptr::null_mut();

    let nt_status = unsafe {
        call_unsafe_wdf_function_binding!(
            WdfDriverCreate,
            wdm_driver,
            reg_path,
            WDF_NO_OBJECT_ATTRIBUTES,
            &mut driver_config,
            &raw mut wdf_driver,
        )
    };

    if !NT_SUCCESS(nt_status) {
        return nt_status;
    }

    let mut safe_driver = Driver {
        wdm_driver,
        wdf_driver,
        reg_path,
    };

    // Translate UTF16 string to rust string
    let reg_path: UNICODE_STRING =
        // SAFETY: This dereference is safe since `registry_path` is:
        //         * provided by `DriverEntry` and is never null
        //         * a valid pointer to a `UNICODE_STRING`
        unsafe { *reg_path };

    let reg_path = to_rust_str(reg_path);
    match safe_entry(&mut safe_driver, &reg_path) {
        Ok(_) => NtStatus::Success.into(),
        Err(e) => {
            cleanup_tracing();
            e.nt_status()
        }
    }
}

#[link_section = "PAGE"]
extern "C" fn evt_driver_device_add(
    _driver: WDFDRIVER,
    device_init: *mut WDFDEVICE_INIT,
) -> NTSTATUS {
    if let Some(cb) = EVT_DEVICE_ADD.get() {
        let mut device_init = unsafe { DeviceInit::from(device_init) };
        match cb(&mut device_init) {
            Ok(_) => 0,
            Err(e) => e.nt_status(),
        }
    } else {
        0
    }
}

extern "C" fn driver_unload(_driver: *mut DRIVER_OBJECT) {
    println!("Driver unload");

    cleanup_tracing();

    println!("Driver unload done");
}

pub fn trace(message: &str) {
    if let Some(trace_writer) = TRACE_WRITER.get() {
        trace_writer.write(message);
    }
}
