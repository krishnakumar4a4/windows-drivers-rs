use wdf::{
    IoQueue,
    IoTarget,
    Request,
    RequestCompletionParamDetails,
    RequestFormatBuffer,
    RequestId,
    RequestStopActionFlags,
    println,
    status_codes,
    usb::UsbRequestCompletionParamDetails,
};

use crate::DeviceContext;

const TEST_BOARD_TRANSFER_BUFFER_SIZE: usize = 64 * 1024;

pub fn evt_io_read(queue: &IoQueue, mut request: Request, length: usize) {
    println!("I/O read callback called");

    if length > TEST_BOARD_TRANSFER_BUFFER_SIZE {
        println!(
            "Transfer {} exceeds {}",
            length, TEST_BOARD_TRANSFER_BUFFER_SIZE
        );
        request.complete_with_information(status_codes::STATUS_INVALID_PARAMETER.into(), 0);
        return;
    }

    let device_context =
        DeviceContext::get(queue.get_device()).expect("Device context should be set");
    let pipe = device_context.get_bulk_read_pipe();

    if let Err(e) =
        pipe.format_request_for_read(&mut request, RequestFormatBuffer::RequestBuffer(None))
    {
        println!("Format request for read failed: {:?}", e);
        request.complete_with_information(e.code().into(), 0);
        return;
    }

    request.set_completion_routine(evt_request_read_completion_routine);

    let io_target = pipe.get_io_target();

    if let Err(request) = request.send_asynchronously(&io_target) {
        let status = request.get_status();
        println!("Request send failed: {:?}", status);
        request.complete_with_information(status, 0);
    }
}

fn evt_request_read_completion_routine(request: Request, _target: &IoTarget) {
    let completion_params = request.get_completion_params();
    let status = completion_params.io_status.status;

    let RequestCompletionParamDetails::Usb {
        completion: usb_completion_params,
        ..
    } = completion_params.parameters
    else {
        println!("Request completed with Non-USB completion params");
        request.complete_with_information(status_codes::STATUS_INVALID_DEVICE_REQUEST.into(), 0);
        return;
    };

    let UsbRequestCompletionParamDetails::PipeRead {
        length: bytes_read, ..
    } = usb_completion_params.parameters
    else {
        println!("Request completed with Non-USB pipe read completion params");
        request.complete_with_information(status_codes::STATUS_INVALID_DEVICE_REQUEST.into(), 0);
        return;
    };

    if status.is_success() {
        println!("Number of bytes read: {}", bytes_read);
    } else {
        println!(
            "Read failed - request status {:?} UsbdStatus {:?}",
            status, usb_completion_params.usbd_status
        );
    }

    request.complete_with_information(status.into(), bytes_read);
}

pub fn evt_io_write(queue: &IoQueue, mut request: Request, length: usize) {
    println!("I/O write callback called");

    if length > TEST_BOARD_TRANSFER_BUFFER_SIZE {
        println!(
            "Transfer {} exceeds {}",
            length, TEST_BOARD_TRANSFER_BUFFER_SIZE
        );
        request.complete_with_information(status_codes::STATUS_INVALID_PARAMETER.into(), 0);
        return;
    }

    let device_context =
        DeviceContext::get(queue.get_device()).expect("Device context should be set");
    let pipe = device_context.get_bulk_write_pipe();

    if let Err(e) =
        pipe.format_request_for_write(&mut request, RequestFormatBuffer::RequestBuffer(None))
    {
        println!("Format request for write failed: {:?}", e);
        request.complete_with_information(e.code().into(), 0);
        return;
    }

    request.set_completion_routine(evt_request_write_completion_routine);

    let io_target = pipe.get_io_target();

    if let Err(request) = request.send_asynchronously(&io_target) {
        let status = request.get_status();
        println!("Request send failed: {:?}", status);
        request.complete_with_information(status, 0);
    }
}

fn evt_request_write_completion_routine(request: Request, _target: &IoTarget) {
    let completion_params = request.get_completion_params();
    let status = completion_params.io_status.status;

    let RequestCompletionParamDetails::Usb {
        completion: usb_completion_params,
        ..
    } = completion_params.parameters
    else {
        println!("Request completed with Non-USB completion params");
        request.complete_with_information(status_codes::STATUS_INVALID_DEVICE_REQUEST.into(), 0);
        return;
    };

    let UsbRequestCompletionParamDetails::PipeWrite {
        length: bytes_written,
        ..
    } = usb_completion_params.parameters
    else {
        println!("Request completed with Non-USB pipe write completion params");
        request.complete_with_information(status_codes::STATUS_INVALID_DEVICE_REQUEST.into(), 0);
        return;
    };

    if status.is_success() {
        println!("Number of bytes written: {}", bytes_written);
    } else {
        println!(
            "Write failed - request status {:?} UsbdStatus {:?}",
            status, usb_completion_params.usbd_status
        );
    }

    request.complete_with_information(status.into(), bytes_written);
}

pub fn evt_io_stop(_queue: &IoQueue, request_id: RequestId, action_flags: RequestStopActionFlags) {
    println!("I/O stop callback called");

    if action_flags.contains(RequestStopActionFlags::SUSPEND) {
        Request::stop_acknowledge_no_requeue(request_id);
    } else if action_flags.contains(RequestStopActionFlags::PURGE) {
        // TODO: this is not safe as request might already be completed.
        // We need to design to enforce synchronization with completion here.
        Request::cancel_sent_request(request_id);
    }
}
