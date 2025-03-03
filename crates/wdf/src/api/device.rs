use wdk_sys::{WDFDEVICE, WDFDEVICE_INIT, WDF_NO_HANDLE, WDF_NO_OBJECT_ATTRIBUTES, WDFOBJECT, call_unsafe_wdf_function_binding};
use crate::api::error::NtResult;

use super::WdfObject;

pub struct Device(WDFDEVICE);

impl Device {
    pub unsafe fn new(inner: WDFDEVICE) -> Self {
        Self(inner)
    }
    pub fn create(device_init: &mut DeviceInit) -> NtResult<Self> {
        let mut device: WDFDEVICE = WDF_NO_HANDLE.cast();
        let mut device_init_ptr: *mut WDFDEVICE_INIT = device_init.as_ptr_mut();

        let status = unsafe { call_unsafe_wdf_function_binding!(
            WdfDeviceCreate,
            &mut device_init_ptr as *mut _,
            WDF_NO_OBJECT_ATTRIBUTES,
            &mut device,
        ) };

        match status {
            0 => Ok(unsafe { Self::new(device) }),
            status => Err(status.into()),
        }
    }
}

impl WdfObject for Device {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0 as WDFOBJECT
    }
}

pub struct DeviceInit(*mut WDFDEVICE_INIT);

impl DeviceInit {
    pub unsafe fn from(inner: *mut WDFDEVICE_INIT) -> Self {
        Self(inner)
    }

    pub fn as_ptr_mut(&self) -> *mut WDFDEVICE_INIT {
        self.0
    }
}
