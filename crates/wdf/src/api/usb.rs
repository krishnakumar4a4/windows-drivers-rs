use core::sync::atomic::AtomicUsize;

use wdf_macros::internal_object_context;
use wdk_sys::{
    call_unsafe_wdf_function_binding,
    NT_SUCCESS,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDFUSBDEVICE,
    WDF_USB_DEVICE_CREATE_CONFIG,
};

use crate::api::{
    device::Device,
    error::NtError,
    object::{Handle, impl_ref_counted_handle, wdf_struct_size},
    sync::Arc,
};

impl_ref_counted_handle!(UsbDevice, UsbDeviceContext);

impl UsbDevice {
    pub fn create_with_parameters(
        device: &Device,
        config: &UsbDeviceCreateConfig
    ) -> Result<Arc<Self>, NtError> {
        let mut usb_device: WDFUSBDEVICE = core::ptr::null_mut();
        let mut config = to_unsafe_config(config);

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceCreateWithParameters,
                device.as_ptr() as *mut _,
                &mut config,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut usb_device
            )
        };

        if NT_SUCCESS(status) {
            let ctxt = UsbDeviceContext {
                ref_count: AtomicUsize::new(0),
            };

            UsbDeviceContext::attach(unsafe { &*(usb_device as *mut _) }, ctxt)?;

            let usb_device = unsafe { Arc::from_raw(usb_device as *mut _) };
            Ok(usb_device)
        } else {
            Err(status.into())
        }
    }

}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFQUEUE do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for UsbDevice {}
unsafe impl Sync for UsbDevice {}


pub struct UsbDeviceCreateConfig {
    pub usbd_client_contract_version: u32
}

fn to_unsafe_config(safe_config: &UsbDeviceCreateConfig) -> WDF_USB_DEVICE_CREATE_CONFIG {
    let mut config =
        unsafe { core::mem::MaybeUninit::<WDF_USB_DEVICE_CREATE_CONFIG>::zeroed().assume_init() };

    config.Size = wdf_struct_size!(WDF_USB_DEVICE_CREATE_CONFIG);
    config.USBDClientContractVersion = safe_config.usbd_client_contract_version;

    config
}

#[internal_object_context(UsbDevice)]
struct UsbDeviceContext {
    ref_count: AtomicUsize,
}

