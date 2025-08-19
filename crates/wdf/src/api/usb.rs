use core::{ptr, sync::atomic::AtomicUsize};

use bitflags::bitflags;
use wdf_macros::internal_object_context;
use wdk_sys::{
    _WdfUsbTargetDeviceSelectConfigType,
    call_unsafe_wdf_function_binding,
    NT_SUCCESS,
    USBD_VERSION_INFORMATION,
    WDFUSBDEVICE,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDF_USB_DEVICE_CREATE_CONFIG,
    WDF_USB_DEVICE_INFORMATION,
    WDF_USB_DEVICE_SELECT_CONFIG_PARAMS,
};

use super::core::{
    device::Device,
    error::NtResult,
    object::{impl_handle, impl_ref_counted_handle, wdf_struct_size, Handle},
    sync::Arc,
};

impl_ref_counted_handle!(UsbDevice, UsbDeviceContext);

impl UsbDevice {
    pub fn create_with_parameters(
        device: &Device,
        config: &UsbDeviceCreateConfig,
    ) -> NtResult<Arc<Self>> {
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

    pub fn retrieve_information(&self) -> NtResult<UsbDeviceInformation> {
        let mut information = WDF_USB_DEVICE_INFORMATION::default();
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceRetrieveInformation,
                self.as_ptr() as *mut _,
                &mut information
            )
        };

        if NT_SUCCESS(status) {
            Ok(information.into())
        } else {
            Err(status.into())
        }
    }

    pub fn select_config_single_interface<'a>(&self) -> NtResult<UsbSingleInterfaceConfig<'a>> {
        let mut select_config = WDF_USB_DEVICE_SELECT_CONFIG_PARAMS::default();
        select_config.Size = wdf_struct_size!(WDF_USB_DEVICE_SELECT_CONFIG_PARAMS);
        select_config.Type =
            _WdfUsbTargetDeviceSelectConfigType::WdfUsbTargetDeviceSelectConfigTypeSingleInterface;

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceSelectConfig,
                self.as_ptr() as *mut _,
                ptr::null_mut(),
                &mut select_config
            )
        };

        if NT_SUCCESS(status) {
            let interface_config = UsbSingleInterfaceConfig {
                number_of_configured_pipes: unsafe {
                    select_config.Types.SingleInterface.NumberConfiguredPipes
                },
                configured_usb_interface: unsafe {
                    &*(select_config.Types.SingleInterface.ConfiguredUsbInterface as *const _)
                },
            };
            Ok(interface_config)
        } else {
            Err(status.into())
        }
    }
}

#[internal_object_context(UsbDevice)]
struct UsbDeviceContext {
    ref_count: AtomicUsize,
}

pub struct UsbDeviceCreateConfig {
    pub usbd_client_contract_version: u32,
}

pub struct UsbDeviceInformation {
    pub usbd_version_information: UsbdVersionInformation,
    pub traits: UsbDeviceTraits,
}

impl From<WDF_USB_DEVICE_INFORMATION> for UsbDeviceInformation {
    fn from(info: WDF_USB_DEVICE_INFORMATION) -> Self {
        Self {
            usbd_version_information: info.UsbdVersionInformation.into(),
            traits: UsbDeviceTraits::from_bits_retain(info.Traits),
        }
    }
}

pub struct UsbdVersionInformation {
    pub usbd_version: u32,
    pub supported_usb_version: u32,
}

impl From<USBD_VERSION_INFORMATION> for UsbdVersionInformation {
    fn from(info: USBD_VERSION_INFORMATION) -> Self {
        Self {
            usbd_version: info.USBDI_Version,
            supported_usb_version: info.Supported_USB_Version,
        }
    }
}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct UsbDeviceTraits: u32 {
        const SELF_POWERED = 0x00000001;
        const REMOTE_WAKE_CAPABLE = 0x00000002;
        const AT_HIGH_SPEED = 0x00000004;
    }
}

fn to_unsafe_config(safe_config: &UsbDeviceCreateConfig) -> WDF_USB_DEVICE_CREATE_CONFIG {
    let mut config = WDF_USB_DEVICE_CREATE_CONFIG::default();

    config.Size = wdf_struct_size!(WDF_USB_DEVICE_CREATE_CONFIG);
    config.USBDClientContractVersion = safe_config.usbd_client_contract_version;

    config
}

pub struct UsbSingleInterfaceConfig<'a> {
    pub number_of_configured_pipes: u8,
    pub configured_usb_interface: &'a UsbInterface,
}

impl_handle!(UsbInterface);