use wdk_sys::{WDFDEVICE, WDFDEVICE_INIT, WDF_NO_HANDLE, WDF_NO_OBJECT_ATTRIBUTES, WDFOBJECT, call_unsafe_wdf_function_binding};
use crate::api::{error::NtResult, guid::Guid, string::{to_unicode_string, to_utf16_buf}};

use super::{FrameworkObject, FrameworkObjectType};

pub struct Device(WDFDEVICE);

impl Device {
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
            0 => Ok(unsafe { Self::from_ptr(device as *mut _) }),
            status => Err(status.into()),
        }
    }

    pub fn create_interface(&self, interaface_class_guid: &Guid, reference_string: Option<&str>) -> NtResult<()> {
        let ref_str_buf = reference_string.map(to_utf16_buf);
        let unicode_ref_str = ref_str_buf.map(|b| to_unicode_string(b.as_ref()));

        let status = unsafe { call_unsafe_wdf_function_binding!(
            WdfDeviceCreateDeviceInterface,
            self.as_ptr() as *mut _,
            interaface_class_guid.as_lpcguid(),
            unicode_ref_str.map_or(core::ptr::null(), |s| &s as _))
         };

        match status {
            0 => Ok(()),
            status => Err(status.into()),
        }
    }
}


impl FrameworkObject for Device {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self(inner as WDFDEVICE)
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0 as WDFOBJECT
    }

    fn object_type() -> FrameworkObjectType {
        FrameworkObjectType::Device
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