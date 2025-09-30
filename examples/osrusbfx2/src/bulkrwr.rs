//! This module has routines to perform reads and writes.
//! The read and writes are targeted to bulk endpoints.

use wdf::{
    IoQueue,
    IoTarget,
    Request,
    RequestCompletionParamDetails,
    RequestCompletionToken,
    RequestFormatMemory,
    RequestId,
    RequestStopActionFlags,
    println,
    status_codes,
    usb::UsbRequestCompletionParamDetails,
};

use crate::DeviceContext;

const TEST_BOARD_TRANSFER_BUFFER_SIZE: usize = 64 * 1024;

/// Called by the framework when it receives read requests.
///
/// # Arguments
///
/// * `queue` - The queue object which sent the request
/// * `request` - The request object
/// * `length` - Length of the data buffer associated with the request.
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

    let device_context = DeviceContext::get(queue.get_device());
    let pipe = device_context.get_bulk_read_pipe();

    // The format call validates to make sure that you are reading or
    // writing to the right pipe type, sets the appropriate transfer flags,
    // creates an URB and initializes the request.
    if let Err(e) =
        pipe.format_request_for_read(&mut request, RequestFormatMemory::RequestMemory(None))
    {
        println!("Format request for read failed: {:?}", e);
        request.complete_with_information(e.code().into(), 0);
        return;
    }

    if let Err(e) = request.set_completion_routine(evt_request_read_completion_routine) {
        println!("Setting completion routine failed: {:?}", e);
        request.complete_with_information(e.code().into(), 0);
        return;
    }

    let io_target = pipe.get_io_target();

    // Send the request asynchronously.
    match request.send_asynchronously(&io_target) {
        Ok(sent_request) => device_context.add_sent_request(sent_request),
        Err(request) => {
            let status = request.get_status();
            println!("Request send failed: {:?}", status);
            request.complete_with_information(status, 0);
        }
    }
}

/// This is the completion routine for reads
///
/// # Arguments
///
/// * `completion_token`` - The completion token for the request.
/// It allows you to get back the `Request` object from the
/// `SentRequest` object stored in the device context.
/// * `target` - The I/O target to which the request was sent.
fn evt_request_read_completion_routine(
    completion_token: RequestCompletionToken,
    _target: &IoTarget,
) {
    println!("Read completion routine called");

    // Redeem the completion token to get back the request object
    // stored in device conteext
    let Some(request) = get_request(completion_token) else {
        println!("Received completion token for unknown request");
        return;
    };

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
        println!("Number of bytes read: {bytes_read}");
    } else if status == status_codes::STATUS_CANCELLED.into() {
        println!("Request cancelled. Number of bytes read: {bytes_read}");
    } else {
        println!(
            "Request failed - request status {:?} UsbdStatus {:?}",
            status, usb_completion_params.usbd_status
        );
    }

    request.complete_with_information(status.into(), bytes_read);
}

/// Called by the framework when it receives write requests
///
/// # Arguments
///
/// * `queue`` - The queue object which sent the request
/// * `request` - The request object
/// * `length` - Length of the data buffer associated with the request.
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

    let device_context = DeviceContext::get(queue.get_device());
    let pipe = device_context.get_bulk_write_pipe();

    if let Err(e) =
        pipe.format_request_for_write(&mut request, RequestFormatMemory::RequestMemory(None))
    {
        println!("Format request for write failed: {:?}", e);
        request.complete_with_information(e.code().into(), 0);
        return;
    }

    if let Err(e) = request.set_completion_routine(evt_request_write_completion_routine) {
        println!("Setting completion routine failed: {:?}", e);
        request.complete_with_information(e.code().into(), 0);
        return;
    }

    let io_target = pipe.get_io_target();

    match request.send_asynchronously(&io_target) {
        Ok(sent_request) => device_context.add_sent_request(sent_request),
        Err(request) => {
            let status = request.get_status();
            println!("Request send failed: {:?}", status);
            request.complete_with_information(status, 0);
        }
    }
}

/// This is the completion routine for writes
///
/// # Arguments
///
/// * `completion_token` - The completion token for the request.
/// It allows you to get back the `Request` object from the
/// `SentRequest` object stored in the device context.
/// * `target` - The I/O target to which the request was sent.
fn evt_request_write_completion_routine(
    completion_token: RequestCompletionToken,
    _target: &IoTarget,
) {
    println!("Write completion routine called");

    let Some(request) = get_request(completion_token) else {
        println!("Received completion token for unknown request");
        return;
    };

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
        println!("Number of bytes written: {bytes_written}");
    } else if status == status_codes::STATUS_CANCELLED.into() {
        println!("Request cancelled. Number of bytes written: {bytes_written}");
    } else {
        println!(
            "Request failed - request status {:?} UsbdStatus {:?}",
            status, usb_completion_params.usbd_status
        );
    }

    request.complete_with_information(status.into(), bytes_written);
}

/// This callback is invoked for every inflight request when the device
/// is suspended or removed. Since our inflight read and write requests
/// are actually pending in the target device, we will just acknowledge
/// its presence. Until we acknowledge, complete, or requeue the requests
/// framework will wait before allowing the device suspend or remove to
/// proceed. When the underlying USB stack gets the request to suspend or
/// remove, it will fail all the pending requests.
///
/// # Arguments
///
/// * `queue` - Queue object that is associated with the I/O request
/// * `request` - Request object
/// * `action_flags` - Bitwise OR of one or more flags in
///   `RequestStopActionFlags`
pub fn evt_io_stop(queue: &IoQueue, request_id: RequestId, action_flags: RequestStopActionFlags) {
    println!("I/O stop callback called");

    if action_flags.contains(RequestStopActionFlags::SUSPEND) {
        Request::stop_acknowledge_no_requeue(request_id);
    } else if action_flags.contains(RequestStopActionFlags::PURGE) {
        let device_context = DeviceContext::get(queue.get_device());
        let Some(sent_request) = device_context.get_sent_request(request_id) else {
            println!(
                "evt_io_stop: request {:?} may have been already completed",
                request_id
            );
            return;
        };
        Request::cancel_sent_request(sent_request);
    }
}

fn get_request(token: RequestCompletionToken) -> Option<Request> {
    let queue = token.get_io_queue();
    let device_context = DeviceContext::get(queue.get_device());
    device_context
        .get_sent_request(token.request_id())
        .map(|s| s.into_request(token))
}
