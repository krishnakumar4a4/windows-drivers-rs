use core::{ptr, sync::atomic::AtomicUsize};

use bitflags::bitflags;
use wdf_macros::{object_context, object_context_with_ref_count_check};
use wdk_sys::{
    _WdfUsbTargetDeviceSelectConfigType,
    call_unsafe_wdf_function_binding,
    BOOLEAN,
    NTSTATUS,
    USBD_STATUS,
    USBD_VERSION_INFORMATION,
    WDFCONTEXT,
    WDFMEMORY,
    WDFMEMORY_OFFSET,
    WDFUSBDEVICE,
    WDFUSBPIPE,
    WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS,
    WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDF_USB_CONTINUOUS_READER_CONFIG,
    WDF_USB_DEVICE_CREATE_CONFIG,
    WDF_USB_DEVICE_INFORMATION,
    WDF_USB_DEVICE_SELECT_CONFIG_PARAMS,
    WDF_USB_PIPE_INFORMATION,
    WDF_USB_PIPE_TYPE,
};

use super::core::{
    device::{Device, DevicePowerPolicyIdleSettings, DevicePowerPolicyWakeSettings},
    enum_mapping,
    io_target::{to_buffer_ptrs, IoTarget, RequestFormatBuffer},
    memory::Memory,
    object::{impl_handle, impl_ref_counted_handle, Handle},
    request::Request,
    result::{NtResult, NtStatus, StatusCodeExt},
    sync::Arc,
    wdf_struct_size,
};

impl_ref_counted_handle!(UsbDevice, UsbDeviceContext);

impl UsbDevice {
    pub fn create(device: &Device, config: &UsbDeviceCreateConfig) -> NtResult<Arc<Self>> {
        let mut usb_device: WDFUSBDEVICE = core::ptr::null_mut();
        let mut config = config.into();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceCreateWithParameters,
                device.as_ptr() as *mut _,
                &mut config,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut usb_device
            )
        }
        .and_then_try(|| {
            let ctxt = UsbDeviceContext {
                ref_count: AtomicUsize::new(0),
            };

            UsbDeviceContext::attach(unsafe { &*(usb_device as *mut _) }, ctxt)?;

            let usb_device = unsafe { Arc::from_raw(usb_device as *mut _) };
            Ok(usb_device)
        })
    }

    pub fn retrieve_information(&self) -> NtResult<UsbDeviceInformation> {
        let mut information = WDF_USB_DEVICE_INFORMATION::default();
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceRetrieveInformation,
                self.as_ptr() as *mut _,
                &mut information
            )
        }
        .and_then(|| information.into())
    }

    pub fn select_config_single_interface<'a>(
        &self,
    ) -> NtResult<UsbSingleInterfaceInformation<'a>> {
        let mut config = WDF_USB_DEVICE_SELECT_CONFIG_PARAMS::default();
        config.Size = wdf_struct_size!(WDF_USB_DEVICE_SELECT_CONFIG_PARAMS);
        config.Type =
            _WdfUsbTargetDeviceSelectConfigType::WdfUsbTargetDeviceSelectConfigTypeSingleInterface;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceSelectConfig,
                self.as_ptr() as *mut _,
                ptr::null_mut(),
                &mut config
            )
        }
        .and_then(|| UsbSingleInterfaceInformation {
            number_of_configured_pipes: unsafe {
                config.Types.SingleInterface.NumberConfiguredPipes
            },
            configured_usb_interface: unsafe {
                &*(config.Types.SingleInterface.ConfiguredUsbInterface as *const _)
            },
        })
    }

    pub fn assign_s0_idle_settings(&self) -> NtResult<DevicePowerPolicyIdleSettings> {
        let mut settings = WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS::default();
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDeviceAssignS0IdleSettings,
                self.as_ptr() as *mut _,
                &mut settings
            )
        }
        .and_then(|| settings.into())
    }

    pub fn assign_sx_wake_settings(&self) -> NtResult<DevicePowerPolicyWakeSettings> {
        let mut settings = WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS::default();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDeviceAssignSxWakeSettings,
                self.as_ptr() as *mut _,
                &mut settings
            )
        }
        .and_then(|| settings.into())
    }
}

#[object_context_with_ref_count_check(UsbDevice)]
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

impl From<&UsbDeviceCreateConfig> for WDF_USB_DEVICE_CREATE_CONFIG {
    fn from(safe_config: &UsbDeviceCreateConfig) -> Self {
        let mut config = WDF_USB_DEVICE_CREATE_CONFIG::default();
        config.Size = wdf_struct_size!(WDF_USB_DEVICE_CREATE_CONFIG);
        config.USBDClientContractVersion = safe_config.usbd_client_contract_version;

        config
    }
}

pub struct UsbSingleInterfaceInformation<'a> {
    pub number_of_configured_pipes: u8,
    pub configured_usb_interface: &'a UsbInterface,
}

impl_handle!(UsbInterface);

impl UsbInterface {
    // TODO - UNSOUNDNESS: the framework can delete the pipe returned here
    // while the caller has got a hold hold of it. it happens if the
    // alternate settings of this interface are changed.
    // Need to plug this soundness hole.
    pub fn get_configured_pipe<'a>(&self, pipe_index: u8) -> Option<&'a UsbPipe> {
        self.get_configured_pipe_impl(pipe_index, false)
            .map(|(pipe, _)| pipe)
    }

    pub fn get_configured_pipe_with_information<'a>(
        &self,
        pipe_index: u8,
    ) -> Option<(&'a UsbPipe, UsbPipeInformation)> {
        self.get_configured_pipe_impl(pipe_index, true)
            .map(|(pipe, info)| {
                (
                    pipe,
                    info.expect("framework should return pipe information if pipe exists"),
                )
            })
    }

    pub fn get_configured_pipe_impl<'a>(
        &self,
        pipe_index: u8,
        get_info: bool,
    ) -> Option<(&'a UsbPipe, Option<UsbPipeInformation>)> {
        let mut pipe_info = WDF_USB_PIPE_INFORMATION::default();
        let pipe_info_ptr = if get_info {
            &raw mut pipe_info
        } else {
            ptr::null_mut()
        };

        let pipe = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbInterfaceGetConfiguredPipe,
                self.as_ptr() as *mut _,
                pipe_index,
                pipe_info_ptr
            )
        };

        if pipe.is_null() {
            None
        } else {
            let pipe_ref = unsafe { &*(pipe as *const UsbPipe) };
            let tuple = if get_info {
                (pipe_ref, Some(pipe_info.into()))
            } else {
                (pipe_ref, None)
            };

            Some(tuple)
        }
    }
}

impl_handle!(UsbPipe);

impl UsbPipe {
    pub fn get_io_target(&self) -> &IoTarget {
        // SAFETY: The pipe pointer is also a valid
        // I/O target pointer. Hence this case is safe
        unsafe { &*(self.as_ptr() as *const IoTarget) }
    }

    pub fn set_no_maximum_packet_size_check(&self) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetPipeSetNoMaximumPacketSizeCheck,
                self.as_ptr() as *mut _
            )
        }
    }

    pub fn config_continuous_reader(&self, config: &UsbContinuousReaderConfig) -> NtResult<()> {
        // TODO: if this function is called more than once, we need to handle
        // the case where the context is already attached.
        // Actually we plan to not have this function at all and in fact
        // allow the user to set up the reader only once while creating
        // the USB device. Then this problem will be removed entirely.
        let ctxt = UsbPipeContinuousReaderContext {
            read_complete_callback: config.read_complete_callback,
            readers_failed_callback: config.readers_failed_callback,
        };

        UsbPipeContinuousReaderContext::attach(self, ctxt)?;

        let mut config = config.into();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetPipeConfigContinuousReader,
                self.as_ptr() as *mut _,
                &mut config
            )
        }
        .ok()
    }

    pub fn format_request_for_read(
        &self,
        request: &mut Request,
        output_buffer: RequestFormatBuffer,
    ) -> NtResult<()> {
        let mut buffer_offset = WDFMEMORY_OFFSET::default();
        let (buffer_ptr, buffer_offset_ptr) =
            to_buffer_ptrs(request, output_buffer, &mut buffer_offset, false)?;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetPipeFormatRequestForRead,
                self.as_ptr() as *mut _,
                request.as_ptr() as *mut _,
                buffer_ptr as *mut _,
                buffer_offset_ptr,
            )
        }
        .ok()
    }

    pub fn format_request_for_write(
        &self,
        request: &mut Request,
        input_buffer: RequestFormatBuffer,
    ) -> NtResult<()> {
        let mut buffer_offset = WDFMEMORY_OFFSET::default();
        let (buffer_ptr, buffer_offset_ptr) =
            to_buffer_ptrs(request, input_buffer, &mut buffer_offset, true)?;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetPipeFormatRequestForRead,
                self.as_ptr() as *mut _,
                request.as_ptr() as *mut _,
                buffer_ptr as *mut _,
                buffer_offset_ptr,
            )
        }
        .ok()
    }
}

#[object_context(UsbPipe)]
struct UsbPipeContinuousReaderContext {
    read_complete_callback: Option<fn(&UsbPipe, &Memory, usize)>,
    readers_failed_callback: Option<fn(&UsbPipe, NtStatus, UsbdStatus) -> bool>,
}

pub struct UsbPipeInformation {
    pub maximum_packet_size: u32,
    pub endpoint_address: u8,
    pub interval: u8,
    pub setting_index: u8,
    pub pipe_type: UsbPipeType,
    pub maximum_transfer_size: u32,
}

impl From<WDF_USB_PIPE_INFORMATION> for UsbPipeInformation {
    fn from(info: WDF_USB_PIPE_INFORMATION) -> Self {
        Self {
            maximum_packet_size: info.MaximumPacketSize,
            endpoint_address: info.EndpointAddress,
            interval: info.Interval,
            setting_index: info.SettingIndex,
            pipe_type: UsbPipeType::try_from(info.PipeType)
                .expect("framework should return correct pipe type"),
            maximum_transfer_size: info.MaximumTransferSize,
        }
    }
}

enum_mapping! {
    pub enum UsbPipeType: WDF_USB_PIPE_TYPE {
        Control = WdfUsbPipeTypeControl,
        Isochronous = WdfUsbPipeTypeIsochronous,
        Bulk = WdfUsbPipeTypeBulk,
        Interrupt = WdfUsbPipeTypeInterrupt
    }
}

pub struct UsbContinuousReaderConfig {
    pub transfer_length: usize,
    pub header_length: usize,
    pub trailer_length: usize,
    pub num_pending_reads: u8,

    // TODO: omitting the context param for now (see below comment)
    pub read_complete_callback: Option<fn(&UsbPipe, &Memory, usize)>,
    // TODO: for now not passing any context because it is hard to
    // decide what it type should be how and when we will drop it
    // WDF seems to internally keep the context pointer and doesn't
    // tell us when it's done with it which makes drop hard.
    // pub read_complete_context: Option<Arc<dyn super::object::ObjectContext>>,
    pub readers_failed_callback: Option<fn(&UsbPipe, NtStatus, UsbdStatus) -> bool>,
}

impl From<&UsbContinuousReaderConfig> for WDF_USB_CONTINUOUS_READER_CONFIG {
    fn from(safe_config: &UsbContinuousReaderConfig) -> Self {
        let mut unsafe_config = WDF_USB_CONTINUOUS_READER_CONFIG::default();
        unsafe_config.Size = wdf_struct_size!(WDF_USB_CONTINUOUS_READER_CONFIG);
        unsafe_config.TransferLength = safe_config.transfer_length;
        unsafe_config.HeaderLength = safe_config.header_length;
        unsafe_config.TrailerLength = safe_config.trailer_length;
        unsafe_config.NumPendingReads = safe_config.num_pending_reads;

        // TODO: setting to no attributes for now.
        // Will come back to it later
        unsafe_config.BufferAttributes = WDF_NO_OBJECT_ATTRIBUTES;

        if safe_config.read_complete_callback.is_some() {
            unsafe_config.EvtUsbTargetPipeReadComplete = Some(__evt_usb_target_pipe_read_complete);
        }

        // TODO: not supporting context for now because we're not
        // clear on what type it should have and the soundness issues.
        unsafe_config.EvtUsbTargetPipeReadCompleteContext = ptr::null_mut();

        if safe_config.readers_failed_callback.is_some() {
            unsafe_config.EvtUsbTargetPipeReadersFailed =
                Some(__evt_usb_target_pipe_readers_failed);
        }

        unsafe_config
    }
}

pub struct UsbdStatus(u32);

impl UsbdStatus {
    pub fn new(status: u32) -> Self {
        Self(status)
    }

    pub fn inner(&self) -> u32 {
        self.0
    }
}

pub extern "C" fn __evt_usb_target_pipe_read_complete(
    pipe: WDFUSBPIPE,
    buffer: WDFMEMORY,
    num_bytes_transferred: usize,
    _context: WDFCONTEXT,
) {
    let pipe = unsafe { &*(pipe as *const UsbPipe) };

    if let Some(ctxt) = UsbPipeContinuousReaderContext::get(pipe) {
        if let Some(callback) = ctxt.read_complete_callback {
            let buffer: &Memory = unsafe { &*(buffer as *const Memory) };
            callback(pipe, buffer, num_bytes_transferred);
            return;
        }
    }

    panic!("User did not provide callback read_complete_callback but we subscribed to it");
}

pub extern "C" fn __evt_usb_target_pipe_readers_failed(
    pipe: WDFUSBPIPE,
    status: NTSTATUS,
    usbd_status: USBD_STATUS,
) -> BOOLEAN {
    let pipe = unsafe { &*(pipe as *const UsbPipe) };

    if let Some(ctxt) = UsbPipeContinuousReaderContext::get(pipe) {
        if let Some(callback) = ctxt.readers_failed_callback {
            let nt_status: NtStatus = status.into();
            let usbd = UsbdStatus::new(usbd_status as u32);
            let result = callback(pipe, nt_status, usbd);
            return if result { 1 } else { 0 };
        }
    }

    panic!("User did not provide callback readers_failed_callback but we subscribed to it");
}
