use core::mem::size_of;

use wdf::{
    Device,
    FILE_READ_ACCESS,
    FILE_WRITE_ACCESS,
    IoQueue,
    IoTargetSentIoAction,
    METHOD_BUFFERED,
    METHOD_OUT_DIRECT,
    MemoryDescriptor,
    MemoryDescriptorMut,
    NtResult,
    NtStatus,
    Request,
    Timeout,
    ctl_code,
    println,
    status_codes,
    usb::{
        UsbBmRequestDirection,
        UsbBmRequestRecipient,
        UsbControlTransfer,
        UsbMemoryDescriptorKind,
    },
};

use crate::DeviceContext;

const IOCTL_INDEX: u32 = 0x800;
const FILE_DEVICE_OSRUSBFX2: u32 = 65500;

const IOCTL_OSRUSBFX2_GET_CONFIG_DESCRIPTOR: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX,
    METHOD_BUFFERED,
    FILE_READ_ACCESS,
);

const IOCTL_OSRUSBFX2_RESET_DEVICE: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 1,
    METHOD_BUFFERED,
    FILE_WRITE_ACCESS,
);

const IOCTL_OSRUSBFX2_REENUMERATE_DEVICE: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 3,
    METHOD_BUFFERED,
    FILE_WRITE_ACCESS,
);

const IOCTL_OSRUSBFX2_GET_BAR_GRAPH_DISPLAY: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 4,
    METHOD_BUFFERED,
    FILE_READ_ACCESS,
);

const IOCTL_OSRUSBFX2_SET_BAR_GRAPH_DISPLAY: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 5,
    METHOD_BUFFERED,
    FILE_WRITE_ACCESS,
);

const IOCTL_OSRUSBFX2_READ_SWITCHES: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 6,
    METHOD_BUFFERED,
    FILE_READ_ACCESS,
);

const IOCTL_OSRUSBFX2_GET_7_SEGMENT_DISPLAY: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 7,
    METHOD_BUFFERED,
    FILE_READ_ACCESS,
);

const IOCTL_OSRUSBFX2_SET_7_SEGMENT_DISPLAY: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 8,
    METHOD_BUFFERED,
    FILE_WRITE_ACCESS,
);

const IOCTL_OSRUSBFX2_GET_INTERRUPT_MESSAGE: u32 = ctl_code(
    FILE_DEVICE_OSRUSBFX2,
    IOCTL_INDEX + 9,
    METHOD_OUT_DIRECT,
    FILE_READ_ACCESS,
);

// The vendor commands supported by our device

const USBFX2LK_READ_7SEGMENT_DISPLAY: u8 = 0xD4;
const USBFX2LK_READ_SWITCHES: u8 = 0xD6;
const USBFX2LK_READ_BARGRAPH_DISPLAY: u8 = 0xD7;
const USBFX2LK_SET_BARGRAPH_DISPLAY: u8 = 0xD8;
// const USBFX2LK_IS_HIGH_SPEED: u8 = 0xD9;
const USBFX2LK_REENUMERATE: u8 = 0xDA;
const USBFX2LK_SET_7SEGMENT_DISPLAY: u8 = 0xDB;

const DEFAULT_CONTROL_TRANSFER_TIMEOUT: Timeout = Timeout::relative_from_millis(5000);

bitflags::bitflags! {
    /// Represents the state of the bar graph on the FX2 device
    ///
    /// Individual bars starting from the top of the stack of bars.
    /// NOTE: There are actually 10 bars, but the very top two do not
    /// light and are not counted here
    #[repr(transparent)]
    pub struct BarGraphState: u8 {
        /// Bar 1 of bar graph on the FX2 device
        const BAR1 = 1 << 0;

        /// Bar 2 of bar graph on the FX2 device
        const BAR2 = 1 << 1;

        /// Bar 3 of bar graph on the FX2 device
        const BAR3 = 1 << 2;

        /// Bar 4 of bar graph on the FX2 device
        const BAR4 = 1 << 3;

        /// Bar 5 of bar graph on the FX2 device
        const BAR5 = 1 << 4;

        /// Bar 6 of bar graph on the FX2 device
        const BAR6 = 1 << 5;

        /// Bar 7 of bar graph on the FX2 device
        const BAR7 = 1 << 6;

        /// Bar 8 of bar graph on the FX2 device
        const BAR8 = 1 << 7;
    }
}

bitflags::bitflags! {
    /// Represents the state of the 8 switches on the FX2 device
    #[repr(transparent)]
    pub struct SwitchState: u8 {
        /// Switch 1 of the FX2 device
        const SWITCH1 = 1 << 0;

        /// Switch 2 of the FX2 device
        const SWITCH2 = 1 << 1;

        /// Switch 3 of the FX2 device
        const SWITCH3 = 1 << 2;

        /// Switch 4 of the FX2 device
        const SWITCH4 = 1 << 3;

        /// Switch 5 of the FX2 device
        const SWITCH5 = 1 << 4;

        /// Switch 6 of the FX2 device
        const SWITCH6 = 1 << 5;

        /// Switch 7 of the FX2 device
        const SWITCH7 = 1 << 6;

        /// Switch 8 of the FX2 device
        const SWITCH8 = 1 << 7;
    }
}

pub fn evt_io_device_control(
    queue: &IoQueue,
    mut request: Request,
    _output_buffer_length: usize,
    _input_buffer_length: usize,
    control_code: u32,
) {
    println!("I/O device control callback called");

    let device = queue.get_device();
    let device_context = DeviceContext::get(device);
    let mut request_pending = false;

    let result = match control_code {
        IOCTL_OSRUSBFX2_GET_CONFIG_DESCRIPTOR => {
            get_config_descriptor(&device_context, &mut request)
        }
        IOCTL_OSRUSBFX2_RESET_DEVICE => reset_device(device_context),
        IOCTL_OSRUSBFX2_REENUMERATE_DEVICE => reenumerate_device(device_context),
        IOCTL_OSRUSBFX2_GET_BAR_GRAPH_DISPLAY => {
            get_bar_graph_display(device_context, &mut request)
        }
        IOCTL_OSRUSBFX2_SET_BAR_GRAPH_DISPLAY => set_bar_graph_display(device_context, &request),
        IOCTL_OSRUSBFX2_GET_7_SEGMENT_DISPLAY => {
            get_seven_segment_display(device_context, &mut request)
        }
        IOCTL_OSRUSBFX2_SET_7_SEGMENT_DISPLAY => {
            set_seven_segment_display(device_context, &request)
        }
        IOCTL_OSRUSBFX2_READ_SWITCHES => get_switch_state(device_context, &mut request),
        IOCTL_OSRUSBFX2_GET_INTERRUPT_MESSAGE => {
            get_interrupt_message(device_context, &mut request, &mut request_pending)
        }
        _ => {
            println!("Unknown IOCTL: {:#X}", control_code);
            Err(status_codes::STATUS_INVALID_DEVICE_REQUEST.into())
        }
    };

    match result {
        Ok(bytes_returned) => {
            if !request_pending {
                println!(
                    "IOCTL succeeded, completing request with {} bytes",
                    bytes_returned
                );
                request
                    .complete_with_information(status_codes::STATUS_SUCCESS.into(), bytes_returned);
            }
        }
        Err(e) => {
            println!("IOCTL failed: {:?}", e);
            request.complete(e.code().into());
        }
    }
}

fn get_config_descriptor(device_context: &DeviceContext, request: &mut Request) -> NtResult<usize> {
    println!("Get config descriptor");

    let usb_device = device_context
        .usb_device
        .get()
        .expect("USB device should be set");

    let required_size = usb_device
        .retrieve_config_descriptor(None)
        .inspect_err(|e| println!("Failed to retrieve config descriptor size: {:?}", e))?;

    let request_buffer = request
        .retrieve_output_buffer(required_size as usize)
        .inspect_err(|e| println!("Failed to retrieve output buffer from request: {:?}", e))?;

    let bytes_written = usb_device.retrieve_config_descriptor(Some(request_buffer))?;

    Ok(bytes_written as usize)
}

fn reset_device(device_context: &DeviceContext) -> NtResult<usize> {
    println!("Reset device");

    stop_all_pipes(device_context);

    let usb_device = device_context
        .usb_device
        .get()
        .expect("USB device should be set");
    usb_device.reset_port_synchronously()?;

    start_all_pipes(device_context)?;

    Ok(0)
}

fn reenumerate_device(device_context: &DeviceContext) -> NtResult<usize> {
    println!("Re-enumerate device");

    send_vendor_command(
        UsbBmRequestDirection::HostToDevice,
        USBFX2LK_REENUMERATE,
        None,
        device_context,
    )
    .inspect_err(|e| println!("Failed to re-enumerate device: {:?}", e))?;

    Ok(0)
}

fn get_bar_graph_display(device_context: &DeviceContext, request: &mut Request) -> NtResult<usize> {
    println!("Get bar graph display");

    let buffer = request.retrieve_output_buffer(size_of::<BarGraphState>())?;
    if buffer.len() < size_of::<BarGraphState>() {
        println!(
            "Output buffer too small for getting bar graph state: {}",
            buffer.len()
        );
        return Err(status_codes::STATUS_BUFFER_TOO_SMALL.into());
    }

    let bytes_transferred = send_vendor_command(
        UsbBmRequestDirection::DeviceToHost,
        USBFX2LK_READ_BARGRAPH_DISPLAY,
        Some(buffer),
        device_context,
    )
    .inspect_err(|e| println!("Failed to get bar graph state: {:?}", e))?;

    println!("Bar graph state: {:#X}", buffer[0]);

    Ok(bytes_transferred)
}

fn set_bar_graph_display(device_context: &DeviceContext, request: &Request) -> NtResult<usize> {
    println!("Set bar graph display");

    let buffer = request.retrieve_input_buffer(size_of::<BarGraphState>())?;
    if buffer.len() < size_of::<BarGraphState>() {
        println!(
            "Input buffer too small for setting bar graph state: {}",
            buffer.len()
        );
        return Err(status_codes::STATUS_BUFFER_TOO_SMALL.into());
    }

    let buf_mut = &mut [buffer[0]; 1];

    let bytes_transferred = send_vendor_command(
        UsbBmRequestDirection::DeviceToHost,
        USBFX2LK_SET_BARGRAPH_DISPLAY,
        Some(buf_mut),
        device_context,
    )
    .inspect_err(|e| println!("Failed to set bar graph state: {:?}", e))?;

    println!("Bar graph state: {:#X}", buffer[0]);

    Ok(bytes_transferred)
}

fn get_seven_segment_display(
    device_context: &DeviceContext,
    request: &mut Request,
) -> NtResult<usize> {
    println!("Get 7-segment display");

    let buffer = request.retrieve_output_buffer(size_of::<u8>())?;
    if buffer.len() < size_of::<u8>() {
        println!(
            "Output buffer too small for getting 7-segment state: {}",
            buffer.len()
        );
        return Err(status_codes::STATUS_BUFFER_TOO_SMALL.into());
    }

    let bytes_transferred = send_vendor_command(
        UsbBmRequestDirection::DeviceToHost,
        USBFX2LK_READ_7SEGMENT_DISPLAY,
        Some(buffer),
        device_context,
    )
    .inspect_err(|e| println!("Failed to get 7-segment: {:?}", e))?;

    println!("7-segment state: {:#X}", buffer[0]);

    Ok(bytes_transferred)
}

fn set_seven_segment_display(device_context: &DeviceContext, request: &Request) -> NtResult<usize> {
    println!("Set 7-segment display");

    let buffer = request.retrieve_input_buffer(size_of::<u8>())?;
    if buffer.len() < size_of::<u8>() {
        println!(
            "Input buffer too small for setting 7-segment state: {}",
            buffer.len()
        );
        return Err(status_codes::STATUS_BUFFER_TOO_SMALL.into());
    }

    let buf_mut = &mut [buffer[0]; 1];

    let bytes_transferred = send_vendor_command(
        UsbBmRequestDirection::DeviceToHost,
        USBFX2LK_SET_7SEGMENT_DISPLAY,
        Some(buf_mut),
        device_context,
    )
    .inspect_err(|e| println!("Failed to set 7-segment: {:?}", e))?;

    println!("7-segment state: {:#X}", buffer[0]);

    Ok(bytes_transferred)
}

fn get_switch_state(device_context: &DeviceContext, request: &mut Request) -> NtResult<usize> {
    println!("Read switches");

    let buffer = request.retrieve_output_buffer(size_of::<SwitchState>())?;
    if buffer.len() < size_of::<SwitchState>() {
        println!(
            "Output buffer too small for getting switch state: {}",
            buffer.len()
        );
        return Err(status_codes::STATUS_BUFFER_TOO_SMALL.into());
    }

    let bytes_transferred = send_vendor_command(
        UsbBmRequestDirection::DeviceToHost,
        USBFX2LK_READ_SWITCHES,
        Some(buffer),
        device_context,
    )
    .inspect_err(|e| println!("Failed to get switch state: {:?}", e))?;

    println!("Switch state: {:#X}", buffer[0]);

    Ok(bytes_transferred)
}

fn get_interrupt_message(
    device_context: &DeviceContext,
    request: &mut Request,
    request_pending: &mut bool,
) -> NtResult<usize> {
    println!("Get interrupt message");

    *request_pending = false;

    let interrupt_queue = device_context
        .interrupt_msg_queue
        .get()
        .expect("Interrupt message queue should be set");

    request
        .forward_to_io_queue(&interrupt_queue)
        .inspect(|_| *request_pending = true)?;

    Ok(0)
}

pub fn usb_ioctl_get_interrupt_message(device: &Device, reader_status: NtStatus) {
    println!(
        "usb_ioctl_get_interrupt_message called with status: {:?}",
        reader_status
    );

    let device_context = DeviceContext::get(device);
    let interrupt_msg_queue = device_context
        .interrupt_msg_queue
        .get()
        .expect("Interrupt message queue should be set");

    // Complete all pending requests
    let mut bytes_returned;
    loop {
        match interrupt_msg_queue.retrieve_next_request() {
            Ok(mut request) => {
                let (request_status, bytes_returned) = match request
                    .retrieve_output_buffer(size_of::<SwitchState>())
                {
                    Ok(output_buffer) => {
                        if reader_status.is_success() {
                            bytes_returned = size_of::<SwitchState>();
                            output_buffer[0] = device_context.current_switch_state.lock().bits();
                            println!("Completing IOCTL with switch state: {}", output_buffer[0]);
                        } else {
                            bytes_returned = 0;
                        }

                        (reader_status, bytes_returned)
                    }
                    Err(e) => {
                        println!("Failed to retrieve output buffer from request: {:?}", e);
                        (e.code().into(), size_of::<SwitchState>())
                    }
                };

                request.complete_with_information(request_status, bytes_returned);
            }
            Err(e) if e.code() == status_codes::STATUS_NO_MORE_ENTRIES => {
                // No more requests to process
                break;
            }
            Err(e) => {
                println!("Failed to retrieve request: {:?}", e);
                continue;
            }
        }
    }
}

fn start_all_pipes(device_context: &DeviceContext) -> NtResult<()> {
    device_context
        .get_interrupt_pipe()
        .get_io_target()
        .start()?;
    device_context
        .get_bulk_read_pipe()
        .get_io_target()
        .start()?;
    device_context
        .get_bulk_write_pipe()
        .get_io_target()
        .start()?;

    Ok(())
}

fn stop_all_pipes(device_context: &DeviceContext) {
    device_context
        .get_interrupt_pipe()
        .get_io_target()
        .stop(IoTargetSentIoAction::CancelSentIo);
    device_context
        .get_bulk_read_pipe()
        .get_io_target()
        .stop(IoTargetSentIoAction::CancelSentIo);
    device_context
        .get_bulk_write_pipe()
        .get_io_target()
        .stop(IoTargetSentIoAction::CancelSentIo);
}

fn send_vendor_command(
    direction: UsbBmRequestDirection,
    request: u8,
    mut buffer: Option<&mut [u8]>,
    device_context: &DeviceContext,
) -> NtResult<usize> {
    let mem_desc: Option<MemoryDescriptor>;
    let mem_desc_mut: MemoryDescriptorMut;

    let memory_descriptor = match direction {
        UsbBmRequestDirection::HostToDevice => {
            mem_desc = buffer.map(|b| MemoryDescriptor::Buffer(&b[..]));
            UsbMemoryDescriptorKind::HostToDevice(mem_desc.as_ref())
        }
        UsbBmRequestDirection::DeviceToHost => {
            let buffer = buffer
                .as_mut()
                .ok_or(status_codes::STATUS_INVALID_PARAMETER)?;
            mem_desc_mut = MemoryDescriptorMut::Buffer(buffer);
            UsbMemoryDescriptorKind::DeviceToHost(&mem_desc_mut)
        }
    };

    let transfer = UsbControlTransfer::Vendor {
        recipient: UsbBmRequestRecipient::Device,
        request,
        value: 0,
        index: 0,
        memory_descriptor,
    };

    let usb_device = device_context
        .usb_device
        .get()
        .expect("USB device should be set");

    let bytes_transferred = usb_device.send_control_transfer_synchronously(
        None,
        &transfer,
        DEFAULT_CONTROL_TRANSFER_TIMEOUT,
    )?;

    Ok(bytes_transferred as usize)
}
