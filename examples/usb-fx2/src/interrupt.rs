
use wdf::{
    Memory,
    NtResult,
    NtStatus,
    println,
    usb::{UsbContinuousReaderConfig, UsbDevice, UsbdStatus, UsbPipe},
};

use crate::DeviceContext;

pub fn cont_reader_for_interrupt_endpoint(pipe: &mut UsbPipe) -> NtResult<()> {
    let mut config = UsbContinuousReaderConfig::new(64, Some(evt_usb_interrupt_pipe_read_complete));
    config.readers_failed_callback = Some(evt_usb_target_pipe_readers_failed);
    pipe.config_continuous_reader(&config)?;
    Ok(())
}

fn evt_usb_interrupt_pipe_read_complete(
    _pipe: &UsbPipe,
    _buffer: &Memory,
    _num_bytes_transferred: usize,
) {
    println!("Interrupt read complete callback called");
}

fn evt_usb_target_pipe_readers_failed(
    pipe: &UsbPipe,
    _status: wdf::NtStatus,
    _usbd_status: UsbdStatus,
) -> bool {
    println!("Interrupt readers failed callback called");
    // let device = pipe.get_io_target().get_device();

    // let device_context = DeviceContext::get(&device);


    true
}