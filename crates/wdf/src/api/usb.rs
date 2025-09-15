use core::{mem, ptr, sync::atomic::AtomicUsize};

use bitflags::bitflags;
use wdf_macros::{object_context, object_context_with_ref_count_check};
use wdk_sys::{
    _WdfUsbTargetDeviceSelectConfigType,
    _WdfUsbTargetDeviceSelectSettingType,
    call_unsafe_wdf_function_binding,
    BOOLEAN,
    NTSTATUS,
    USBD_STATUS,
    USBD_VERSION_INFORMATION,
    USB_INTERFACE_DESCRIPTOR,
    WDFCONTEXT,
    WDFMEMORY,
    WDFMEMORY_OFFSET,
    WDFUSBDEVICE,
    WDFUSBINTERFACE,
    WDFUSBPIPE,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDF_REQUEST_SEND_OPTIONS,
    WDF_USB_BMREQUEST_DIRECTION,
    WDF_USB_BMREQUEST_RECIPIENT,
    WDF_USB_BMREQUEST_TYPE,
    WDF_USB_CONTINUOUS_READER_CONFIG,
    WDF_USB_CONTROL_SETUP_PACKET,
    WDF_USB_DEVICE_CREATE_CONFIG,
    WDF_USB_DEVICE_INFORMATION,
    WDF_USB_DEVICE_SELECT_CONFIG_PARAMS,
    WDF_USB_INTERFACE_SELECT_SETTING_PARAMS,
    WDF_USB_PIPE_INFORMATION,
    WDF_USB_PIPE_TYPE,
    WDF_USB_REQUEST_COMPLETION_PARAMS,
    WDF_USB_REQUEST_TYPE,
    _WDF_REQUEST_SEND_OPTIONS_FLAGS,
};

use super::core::{
    device::Device,
    enum_mapping,
    init_wdf_struct,
    io_target::{to_buffer_ptrs, IoTarget, RequestFormatBuffer},
    memory::Memory,
    object::{impl_handle, impl_ref_counted_handle, Handle},
    request::Request,
    result::{status_codes, NtResult, NtStatus, StatusCodeExt},
    sync::Arc,
    Timeout,
};

impl_ref_counted_handle!(UsbDevice, UsbDeviceContext);

impl UsbDevice {
    pub fn create<F: Fn(&mut Self) -> NtResult<()>>(
        device: &Device,
        config: &UsbDeviceCreateConfig,
        configure: F,
    ) -> NtResult<Arc<Self>> {
        let mut usb_device: WDFUSBDEVICE = core::ptr::null_mut();
        let mut config = config.into();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceCreateWithParameters,
                device.as_ptr().cast(),
                &mut config,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut usb_device
            )
        }
        .and_then(|| {
            let ctxt = UsbDeviceContext {
                ref_count: AtomicUsize::new(0),
            };

            UsbDeviceContext::attach(unsafe { &*(usb_device.cast()) }, ctxt)?;

            let usb_device_mut_ref = unsafe { &mut *(usb_device.cast()) };
            configure(usb_device_mut_ref)?;

            let usb_device = unsafe { Arc::from_raw(usb_device.cast()) };

            Ok(usb_device)
        })
    }

    pub fn retrieve_config_descriptor(
        &self,
        config_descriptor: Option<&mut [u8]>,
    ) -> NtResult<u16> {
        let mut length: u16 = 0;
        let config_descriptor_is_none = config_descriptor.is_none();
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceRetrieveConfigDescriptor,
                self.as_ptr().cast(),
                config_descriptor
                    .map_or(ptr::null_mut(), |b| b.as_mut_ptr())
                    .cast(),
                &mut length
            )
        };

        if status == status_codes::STATUS_BUFFER_TOO_SMALL && config_descriptor_is_none {
            Ok(length)
        } else {
            Err(status.into())
        }
    }

    pub fn retrieve_information(&self) -> NtResult<UsbDeviceInformation> {
        let mut information = init_wdf_struct!(WDF_USB_DEVICE_INFORMATION);
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceRetrieveInformation,
                self.as_ptr().cast(),
                &mut information
            )
        }
        .map(|| information.into())
    }

    pub fn select_config_single_interface<'a>(
        // `&mut self` ensures there are no outstanding references to
        // any `UsbPipe`s of this device while we are selecting a config
        // because the change in config could internally delete them
        // causing those references to dangle
        &'a mut self,
    ) -> NtResult<UsbSingleInterfaceInformation<'a>> {
        let mut config = init_wdf_struct!(WDF_USB_DEVICE_SELECT_CONFIG_PARAMS);
        config.Type =
            _WdfUsbTargetDeviceSelectConfigType::WdfUsbTargetDeviceSelectConfigTypeSingleInterface;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceSelectConfig,
                self.as_ptr().cast(),
                ptr::null_mut(),
                &mut config
            )
        }
        .map(|| UsbSingleInterfaceInformation {
            number_of_configured_pipes: unsafe {
                config.Types.SingleInterface.NumberConfiguredPipes
            },
            configured_usb_interface: unsafe {
                &*(config.Types.SingleInterface.ConfiguredUsbInterface.cast())
            },
        })
    }

    pub fn get_interface(&self, interface_index: u8) -> Option<&UsbInterface> {
        let interface = self.get_interface_ptr(interface_index);
        unsafe { (interface.cast::<UsbInterface>()).as_ref() }
    }

    pub fn get_interface_mut(&mut self, interface_index: u8) -> Option<&mut UsbInterface> {
        let interface = self.get_interface_ptr(interface_index);
        unsafe { (interface.cast::<UsbInterface>()).as_mut() }
    }

    pub fn reset_port_synchronously(&self) -> NtResult<()> {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceResetPortSynchronously,
                self.as_ptr().cast()
            )
        }
        .ok()
    }

    // WdfUsbTargetDeviceSendControlTransferSynchronously

    pub fn send_control_transfer_synchronously(
        &self,
        // TODO: support request
        // request: Option<Request>,
        setup_packet: &UsbControlSetupPacket,
        timeout: Timeout,
        // TODO: support memory descriptor
        // memory_descriptor: Option<&MemoryDescriptor>,
    ) -> NtResult<u32> {
        let mut setup_packet = setup_packet.into();

        let mut send_options = init_wdf_struct!(WDF_REQUEST_SEND_OPTIONS);
        send_options.Flags |=
            _WDF_REQUEST_SEND_OPTIONS_FLAGS::WDF_REQUEST_SEND_OPTION_TIMEOUT as u32;
        send_options.Timeout = timeout.as_wdf_timeout();

        let mut bytes_transferred: u32 = 0;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceSendControlTransferSynchronously,
                self.as_ptr().cast(),
                ptr::null_mut(),
                &mut send_options,
                &mut setup_packet,
                ptr::null_mut(), // TODO: support memory descriptor
                &mut bytes_transferred
            )
        }
        .map(|| bytes_transferred)
    }

    fn get_interface_ptr(&self, interface_index: u8) -> WDFUSBINTERFACE {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetDeviceGetInterface,
                self.as_ptr().cast(),
                interface_index
            )
        }
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
        let mut config = init_wdf_struct!(WDF_USB_DEVICE_CREATE_CONFIG);
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
    pub fn get_configured_pipe<'a>(&self, pipe_index: u8) -> Option<&'a UsbPipe> {
        self.get_configured_pipe_impl(pipe_index, false)
            .map(|(pipe, _)| unsafe { &*(pipe.cast::<UsbPipe>()) })
    }

    pub fn get_configured_pipe_with_information<'a>(
        &self,
        pipe_index: u8,
    ) -> Option<(&'a UsbPipe, UsbPipeInformation)> {
        self.get_configured_pipe_impl(pipe_index, true)
            .map(|(pipe, info)| {
                (
                    unsafe { &*(pipe.cast::<UsbPipe>()) },
                    info.expect("framework should return pipe information if pipe exists"),
                )
            })
    }

    pub fn get_configured_pipe_mut<'a>(&mut self, pipe_index: u8) -> Option<&'a mut UsbPipe> {
        self.get_configured_pipe_impl(pipe_index, false)
            .map(|(pipe, _)| unsafe { &mut *(pipe.cast::<UsbPipe>()) })
    }

    pub fn get_configured_pipe_with_information_mut<'a>(
        &mut self,
        pipe_index: u8,
    ) -> Option<(&'a mut UsbPipe, UsbPipeInformation)> {
        self.get_configured_pipe_impl(pipe_index, true)
            .map(|(pipe, info)| {
                (
                    unsafe { &mut *(pipe.cast::<UsbPipe>()) },
                    info.expect("framework should return pipe information if pipe exists"),
                )
            })
    }

    pub fn select_setting(
        // `&mut self` ensures there are no outstanding references to
        // any `UsbPipe`s of this interface while we are selecting a config
        // because the change in config could internally delete them
        // causing those references to dangle
        &mut self,
        params: &UsbInterfaceSelectSettingParams,
    ) -> NtResult<()> {
        let mut raw_params = init_wdf_struct!(WDF_USB_INTERFACE_SELECT_SETTING_PARAMS);
        let mut raw_descriptor: USB_INTERFACE_DESCRIPTOR;

        match params {
            UsbInterfaceSelectSettingParams::Descriptor(descriptor) => {
                raw_params.Type = _WdfUsbTargetDeviceSelectSettingType:: WdfUsbInterfaceSelectSettingTypeDescriptor;
                raw_descriptor = descriptor.into();
                raw_params.Types.Descriptor.InterfaceDescriptor = &mut raw_descriptor;
            }
            UsbInterfaceSelectSettingParams::SettingIndex(index) => {
                raw_params.Type =
                    _WdfUsbTargetDeviceSelectSettingType::WdfUsbInterfaceSelectSettingTypeSetting;
                raw_params.Types.Interface.SettingIndex = *index;
            }
        }

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbInterfaceSelectSetting,
                self.as_ptr().cast(),
                ptr::null_mut(),
                &mut raw_params
            )
        }
        .ok()
    }

    fn get_configured_pipe_impl<'a>(
        &self,
        pipe_index: u8,
        get_info: bool,
    ) -> Option<(*mut UsbPipe, Option<UsbPipeInformation>)> {
        let mut pipe_info = WDF_USB_PIPE_INFORMATION::default();
        let pipe_info_ptr = if get_info {
            &raw mut pipe_info
        } else {
            ptr::null_mut()
        };

        let pipe = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbInterfaceGetConfiguredPipe,
                self.as_ptr().cast(),
                pipe_index,
                pipe_info_ptr
            )
        };

        if pipe.is_null() {
            None
        } else {
            let pipe_ptr = pipe.cast::<UsbPipe>();
            let tuple = if get_info {
                (pipe_ptr, Some(pipe_info.into()))
            } else {
                (pipe_ptr, None)
            };

            Some(tuple)
        }
    }
}

pub enum UsbInterfaceSelectSettingParams {
    Descriptor(UsbInterfaceDescriptor),
    SettingIndex(u8),
    // TODO: implement all the types needed
    // to enable this variant
    // Urb(Urb)
}

pub struct UsbInterfaceDescriptor {
    length: u8,
    descriptor_type: u8,
    interface_number: u8,
    alternate_setting: u8,
    num_endpoints: u8,
    interface_class: u8,
    interface_sub_class: u8,
    interface_protocol: u8,
    interface: u8,
}

impl From<&UsbInterfaceDescriptor> for USB_INTERFACE_DESCRIPTOR {
    fn from(descriptor: &UsbInterfaceDescriptor) -> Self {
        Self {
            bLength: descriptor.length,
            bDescriptorType: descriptor.descriptor_type,
            bInterfaceNumber: descriptor.interface_number,
            bAlternateSetting: descriptor.alternate_setting,
            bNumEndpoints: descriptor.num_endpoints,
            bInterfaceClass: descriptor.interface_class,
            bInterfaceSubClass: descriptor.interface_sub_class,
            bInterfaceProtocol: descriptor.interface_protocol,
            iInterface: descriptor.interface,
        }
    }
}

impl_handle!(UsbPipe);

impl UsbPipe {
    pub fn get_io_target(&self) -> &IoTarget {
        // SAFETY: The pipe pointer is also a valid
        // I/O target pointer. Hence this case is safe
        unsafe { &*(self.as_ptr().cast::<IoTarget>()) }
    }

    pub fn set_no_maximum_packet_size_check(&self) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetPipeSetNoMaximumPacketSizeCheck,
                self.as_ptr().cast()
            )
        }
    }

    pub fn config_continuous_reader(&mut self, config: &UsbContinuousReaderConfig) -> NtResult<()> {
        if let Some(ctxt) = UsbPipeContinuousReaderContext::try_get_mut(self) {
            ctxt.read_complete_callback = config.read_complete_callback;
            ctxt.readers_failed_callback = config.readers_failed_callback;
        } else {
            let ctxt = UsbPipeContinuousReaderContext {
                read_complete_callback: config.read_complete_callback,
                readers_failed_callback: config.readers_failed_callback,
            };

            UsbPipeContinuousReaderContext::attach(self, ctxt)?;
        }

        let mut config = config.into();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfUsbTargetPipeConfigContinuousReader,
                self.as_ptr().cast(),
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
                self.as_ptr().cast(),
                request.as_ptr().cast(),
                buffer_ptr.cast(),
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
                self.as_ptr().cast(),
                request.as_ptr().cast(),
                buffer_ptr.cast(),
                buffer_offset_ptr,
            )
        }
        .ok()
    }

    pub fn is_in_endpoint(&self) -> bool {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfUsbTargetPipeIsInEndpoint, self.as_ptr().cast())
                != 0
        }
    }

    pub fn is_out_endpoint(&self) -> bool {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfUsbTargetPipeIsOutEndpoint, self.as_ptr().cast())
                != 0
        }
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

impl UsbContinuousReaderConfig {
    pub fn new(
        transfer_length: usize,
        read_complete_callback: Option<fn(&UsbPipe, &Memory, usize)>,
    ) -> Self {
        Self {
            transfer_length,
            header_length: 0,
            trailer_length: 0,
            num_pending_reads: 1,
            read_complete_callback,
            // read_complete_context: None,
            readers_failed_callback: None,
        }
    }
}

impl From<&UsbContinuousReaderConfig> for WDF_USB_CONTINUOUS_READER_CONFIG {
    fn from(safe_config: &UsbContinuousReaderConfig) -> Self {
        let mut unsafe_config = init_wdf_struct!(WDF_USB_CONTINUOUS_READER_CONFIG);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UsbdStatus(i32);

impl UsbdStatus {
    pub fn new(status: i32) -> Self {
        Self(status)
    }

    pub fn inner(&self) -> i32 {
        self.0
    }
}

#[derive(Debug)]
pub struct UsbRequestCompletionParams<'a> {
    pub usbd_status: UsbdStatus,
    pub request_type: UsbRequestType,
    pub parameters: UsbRequestCompletionParamDetails<'a>,
}

enum_mapping! {
    pub enum UsbRequestType: WDF_USB_REQUEST_TYPE {
        DeviceString = WdfUsbRequestTypeDeviceString,
        DeviceControlTransfer = WdfUsbRequestTypeDeviceControlTransfer,
        DeviceUrb = WdfUsbRequestTypeDeviceUrb,
        PipeWrite = WdfUsbRequestTypePipeWrite,
        PipeRead = WdfUsbRequestTypePipeRead,
        PipeAbort = WdfUsbRequestTypePipeAbort,
        PipeReset = WdfUsbRequestTypePipeReset,
        PipeUrb = WdfUsbRequestTypePipeUrb
    }
}

impl<'a> From<&'a WDF_USB_REQUEST_COMPLETION_PARAMS> for UsbRequestCompletionParams<'a> {
    fn from(raw: &'a WDF_USB_REQUEST_COMPLETION_PARAMS) -> Self {
        let request_type = UsbRequestType::try_from(raw.Type)
            .expect("framework should return correct request type");

        let parameters = match request_type {
            UsbRequestType::DeviceString => {
                let dev_string = unsafe { &raw.Parameters.DeviceString };
                UsbRequestCompletionParamDetails::DeviceString {
                    buffer: unsafe { &*(dev_string.Buffer.cast::<Memory>()) },
                    lang_id: dev_string.LangID,
                    string_index: dev_string.StringIndex,
                    required_size: dev_string.RequiredSize,
                }
            }
            UsbRequestType::DeviceControlTransfer => {
                let transfer = unsafe { &raw.Parameters.DeviceControlTransfer };
                UsbRequestCompletionParamDetails::DeviceControlTransfer {
                    buffer: unsafe { &*(transfer.Buffer.cast::<Memory>()) },
                    setup_packet: UsbControlSetupPacket::from(&transfer.SetupPacket),
                    length: transfer.Length,
                }
            }
            UsbRequestType::DeviceUrb => UsbRequestCompletionParamDetails::DeviceUrb {
                buffer: unsafe { &*(raw.Parameters.DeviceUrb.Buffer.cast::<Memory>()) },
            },
            UsbRequestType::PipeWrite => {
                let write = unsafe { &raw.Parameters.PipeWrite };
                UsbRequestCompletionParamDetails::PipeWrite {
                    buffer: unsafe { &*(write.Buffer.cast::<Memory>()) },
                    length: write.Length as usize,
                    offset: write.Offset as usize,
                }
            }
            UsbRequestType::PipeRead => {
                let read = unsafe { &raw.Parameters.PipeRead };
                UsbRequestCompletionParamDetails::PipeRead {
                    buffer: unsafe { &*(read.Buffer.cast::<Memory>()) },
                    length: read.Length as usize,
                    offset: read.Offset as usize,
                }
            }
            UsbRequestType::PipeUrb => UsbRequestCompletionParamDetails::PipeUrb {
                buffer: unsafe { &*(raw.Parameters.PipeUrb.Buffer.cast::<Memory>()) },
            },
            _ => panic!("framework should not return other request types"),
        };

        Self {
            usbd_status: UsbdStatus::new(raw.UsbdStatus),
            request_type,
            parameters,
        }
    }
}

#[derive(Debug)]
pub enum UsbRequestCompletionParamDetails<'a> {
    DeviceString {
        buffer: &'a Memory,
        lang_id: u16,
        string_index: u8,
        required_size: u8,
    },
    DeviceControlTransfer {
        buffer: &'a Memory,
        setup_packet: UsbControlSetupPacket,
        length: u32,
    },
    DeviceUrb {
        buffer: &'a Memory,
    },
    PipeWrite {
        buffer: &'a Memory,
        length: usize,
        offset: usize,
    },
    PipeRead {
        buffer: &'a Memory,
        length: usize,
        offset: usize,
    },
    PipeUrb {
        buffer: &'a Memory,
    },
}

// repr(C) is need to keep the memory layout stable
// so that we can return the byte array representation
// of the struct safely from the `as_bytes` method below
#[repr(C)]
#[derive(Debug)]
pub struct UsbControlSetupPacket {
    bm: u8,
    request: u8,
    value: u16,
    index: u16,
    length: u16,
}

impl UsbControlSetupPacket {
    pub fn from_bytes(bytes: [u8; 8]) -> Self {
        Self {
            bm: bytes[0],
            request: bytes[1],
            value: (bytes[3] as u16) << 8 | (bytes[2] as u16),
            index: (bytes[5] as u16) << 8 | (bytes[4] as u16),
            length: (bytes[7] as u16) << 8 | (bytes[6] as u16),
        }
    }

    pub fn new_vendor(
        direction: UsbBmRequestDirection,
        recipient: UsbBmRequestRecipient,
        request: u8,
        value: u16,
        index: u16,
    ) -> Self {
        Self {
            bm: Self::to_bm(recipient, UsbBmRequestType::Vendor, direction),
            request,
            value,
            index,
            length: 0,
        }
    }

    pub fn as_bytes(&self) -> &[u8; mem::size_of::<Self>()] {
        unsafe { &*((self as *const Self).cast::<[u8; mem::size_of::<Self>()]>()) }
    }

    fn to_bm(
        recipient: UsbBmRequestRecipient,
        typ: UsbBmRequestType,
        dir: UsbBmRequestDirection,
    ) -> u8 {
        let mut bm = 0;
        bm |= (recipient as u8) & 0b11;
        bm |= ((typ as u8) & 0b11) << 5;
        bm |= ((dir as u8) & 0b1) << 7;
        bm
    }
}

impl From<&WDF_USB_CONTROL_SETUP_PACKET> for UsbControlSetupPacket {
    fn from(raw: &WDF_USB_CONTROL_SETUP_PACKET) -> Self {
        let packet = unsafe { &raw.Packet };
        Self {
            bm: unsafe { packet.bm.Byte },
            request: packet.bRequest,
            value: unsafe { packet.wValue.Value },
            index: unsafe { packet.wIndex.Value },
            length: packet.wLength,
        }
    }
}

impl Into<WDF_USB_CONTROL_SETUP_PACKET> for &UsbControlSetupPacket {
    fn into(self) -> WDF_USB_CONTROL_SETUP_PACKET {
        // WDF_USB_CONTROL_SETUP_PACKET does not have any Size field so
        // wdf_init_struct! macro is not used
        let mut raw = WDF_USB_CONTROL_SETUP_PACKET::default();
        raw.Packet.bm.Byte = self.bm;
        raw.Packet.bRequest = self.request;
        raw.Packet.wValue.Value = self.value;
        raw.Packet.wIndex.Value = self.index;
        raw.Packet.wLength = self.length;

        raw
    }
}

enum_mapping! {
    infallible;
    pub enum UsbBmRequestDirection: WDF_USB_BMREQUEST_DIRECTION {
        HostToDevice = BmRequestHostToDevice,
        DeviceToHost = BmRequestDeviceToHost
    }
}

enum_mapping! {
    infallible;
    pub enum UsbBmRequestRecipient: WDF_USB_BMREQUEST_RECIPIENT {
        Device = BmRequestToDevice,
        Interface = BmRequestToInterface,
        Endpoint = BmRequestToEndpoint,
        Other = BmRequestToOther
    }
}

enum_mapping! {
    infallible;
    pub enum UsbBmRequestType: WDF_USB_BMREQUEST_TYPE {
        Standard = BmRequestStandard,
        Class = BmRequestClass,
        Vendor = BmRequestVendor
    }
}

pub extern "C" fn __evt_usb_target_pipe_read_complete(
    pipe: WDFUSBPIPE,
    buffer: WDFMEMORY,
    num_bytes_transferred: usize,
    _context: WDFCONTEXT,
) {
    let pipe = unsafe { &*(pipe.cast::<UsbPipe>()) };

    if let Some(ctxt) = UsbPipeContinuousReaderContext::try_get(pipe) {
        if let Some(callback) = ctxt.read_complete_callback {
            let buffer: &Memory = unsafe { &*(buffer.cast::<Memory>()) };
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
    let pipe = unsafe { &*(pipe.cast::<UsbPipe>()) };

    if let Some(ctxt) = UsbPipeContinuousReaderContext::try_get(pipe) {
        if let Some(callback) = ctxt.readers_failed_callback {
            let nt_status: NtStatus = status.into();
            let usbd = UsbdStatus::new(usbd_status);
            let result = callback(pipe, nt_status, usbd);
            return if result { 1 } else { 0 };
        }
    }

    panic!("User did not provide callback readers_failed_callback but we subscribed to it");
}
