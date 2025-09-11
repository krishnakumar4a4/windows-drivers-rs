use wdf::{Device, IoQueue, NtStatus, println, Request, status_codes};

use crate::{DeviceContext, SwitchState};


pub fn evt_io_device_control(
    _queue: &IoQueue,
    _request: Request,
    _output_buffer_length: usize,
    _input_buffer_length: usize,
    _control_code: u32,
) {
    println!("I/O device control callback called");
}


pub fn usb_ioctl_get_interrupt_message(device: &Device, reader_status: NtStatus) {
    println!(
        "usb_ioctl_get_interrupt_message called with status: {:?}",
        reader_status
    );

    let device_context = DeviceContext::get(device).expect("Device context should be set");
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

                        let request_status = if reader_status.is_success() {
                            reader_status
                        } else {
                            status_codes::STATUS_SUCCESS.into()
                        };

                        (request_status, bytes_returned)
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
