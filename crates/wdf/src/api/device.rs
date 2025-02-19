use wdk_sys::{WDFDEVICE, WDFDEVICE_INIT, WDF_NO_HANDLE, WDF_NO_OBJECT_ATTRIBUTES, call_unsafe_wdf_function_binding};
use crate::api::{error::NtError, WdfRc};

pub struct Device(WDFDEVICE);

impl Device {
    pub fn create(device_init: &mut DeviceInit) -> Result<Self, NtError> {
        let mut device: WDFDEVICE = WDF_NO_HANDLE.cast();
        let mut device_init_ptr: *mut WDFDEVICE_INIT = device_init.as_ptr_mut();

        let status = unsafe { call_unsafe_wdf_function_binding!(
            WdfDeviceCreate,
            &mut device_init_ptr as *mut _,
            WDF_NO_OBJECT_ATTRIBUTES,
            &mut device,
        ) };

        match status {
            0 => Ok(Self(device)),
            status => Err(status.into()),
        }
    }
}

pub struct DeviceInit(*mut WDFDEVICE_INIT);

impl DeviceInit {
    pub fn from(inner: *mut WDFDEVICE_INIT) -> Self {
        Self(inner)
    }

    pub fn as_ptr_mut(&self) -> *mut WDFDEVICE_INIT {
        self.0
    }
}
