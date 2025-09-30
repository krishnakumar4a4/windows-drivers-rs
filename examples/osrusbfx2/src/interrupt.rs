//! This module has routines to configure a continuous reader on an
//! interrupt pipe to asynchronously read toggle switch states.

use wdf::{
    Memory,
    NtResult,
    NtStatus,
    println,
    status_codes,
    usb::{UsbContinuousReaderConfig, UsbPipe, UsbdStatus},
};

use crate::{DeviceContext, SwitchState, ioctl::usb_ioctl_get_interrupt_message};

/// This routine configures a continuous reader on the
/// interrupt endpoint. It's called from the PrepareHardware event
pub fn cont_reader_for_interrupt_endpoint(pipe: &mut UsbPipe) -> NtResult<()> {
    let mut config = UsbContinuousReaderConfig::new(64, Some(evt_usb_interrupt_pipe_read_complete));
    config.readers_failed_callback = Some(evt_usb_target_pipe_readers_failed);

    // Reader requests are not posted to the target automatically.
    // Driver must explicitly call `IoTarget::start()` to kickstart the
    // reader. In this sample, it's done in D0Entry. By default,
    // framework queues two requests to the target endpoint. Driver
    // can configure up to 10 requests with
    // `UsbContinuousReaderConfig::set_num_requests`.
    pipe.config_continuous_reader(&config)?;
    Ok(())
}

/// This is the completion routine of the continuous reader
///
/// # Arguments
///
/// * pipe - The pipe on which the read was performed
/// * buffer - This buffer is freed when this call returns.
/// If the driver wants to delay processing of the buffer, it
/// can take an additional referrence.
/// * num_bytes_transferred - Number of bytes read into the buffer
fn evt_usb_interrupt_pipe_read_complete(
    pipe: &UsbPipe,
    buffer: &Memory,
    num_bytes_transferred: usize,
) {
    println!("Interrupt read complete callback called");

    // Make sure that there is data in the read packet. Depending on the device
    // specification, it is possible for it to return a 0 length read in
    // certain conditions.
    if num_bytes_transferred == 0 {
        println!("Zero length read occurred on the Interrupt Pipe's Continuous Reader");
        return;
    }

    let device = pipe.get_io_target().get_device();
    let device_context = DeviceContext::get(device);
    *device_context.current_switch_state.lock() =
        SwitchState::from_bits_retain(buffer.get_buffer()[0]);

    // Handle any pending Interrupt Message IOCTLs. Note that the OSR USB device
    // will generate an interrupt message when the the device resumes from a low
    // power state. So if the Interrupt Message IOCTL was sent after the device
    // has gone to a low power state, the pending Interrupt Message IOCTL will
    // get completed in the function call below, before the user twiddles the
    // dip switches on the OSR USB device. If this is not the desired behavior
    // for your driver, then you could handle this condition by maintaining a
    // state variable in D0Entry to track interrupt messages caused by power up.
    usb_ioctl_get_interrupt_message(device, status_codes::STATUS_SUCCESS.into());
}

/// This routine is called when the continuous reader encounters an error
///
/// # Arguments
///
/// * pipe - The pipe on which the read was performed
/// * status - The NTSTATUS code for the error
/// * usbd_status - The USBD status code for the error
fn evt_usb_target_pipe_readers_failed(
    pipe: &UsbPipe,
    status: NtStatus,
    _usbd_status: UsbdStatus,
) -> bool {
    println!("Interrupt readers failed callback called");

    let device = pipe.get_io_target().get_device();
    let device_context = DeviceContext::get(device);

    // Clear the current switch state
    *device_context.current_switch_state.lock() = SwitchState::empty();

    // Service the pending interrupt switch change request
    usb_ioctl_get_interrupt_message(device, status);

    true
}
