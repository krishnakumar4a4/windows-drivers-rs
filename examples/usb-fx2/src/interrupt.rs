
use wdf::{
    Memory,
    NtResult,
    NtStatus,
    println,
    status_codes,
    usb::{UsbContinuousReaderConfig, UsbDevice, UsbdStatus, UsbPipe},
};

use crate::{
    DeviceContext,
    ioctl::usb_ioctl_get_interrupt_message,
};

pub fn cont_reader_for_interrupt_endpoint(pipe: &mut UsbPipe) -> NtResult<()> {
    let mut config = UsbContinuousReaderConfig::new(64, Some(evt_usb_interrupt_pipe_read_complete));
    config.readers_failed_callback = Some(evt_usb_target_pipe_readers_failed);
    pipe.config_continuous_reader(&config)?;
    Ok(())
}

fn evt_usb_interrupt_pipe_read_complete(
    pipe: &UsbPipe,
    buffer: &Memory,
    num_bytes_transferred: usize,
) {
    println!("Interrupt read complete callback called");

    if num_bytes_transferred == 0 {
        println!("Zero length read occured on the Interrupt Pipe's Continuous Reader");
        return;
    } 


    let device = pipe.get_io_target().get_device();
    let device_context = DeviceContext::get(&device).expect("Device context should be set");
    *device_context.current_switch_state.lock() = buffer.get_buffer()[0];

    usb_ioctl_get_interrupt_message(&device, status_codes::STATUS_SUCCESS.into());
}

fn evt_usb_target_pipe_readers_failed(
    pipe: &UsbPipe,
    status: NtStatus,
    _usbd_status: UsbdStatus,
) -> bool {
    println!("Interrupt readers failed callback called");

    let device = pipe.get_io_target().get_device();
    let device_context = DeviceContext::get(&device).expect("Device context should be set");

    *device_context.current_switch_state.lock() = 0;

    usb_ioctl_get_interrupt_message(&device, status);

    true
}