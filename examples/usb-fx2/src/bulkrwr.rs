use wdf::{
    Device,
    IoQueue,
    println,
    Request,
    RequestFormatBuffer,
    RequestId,
    RequestStopActionFlags,
    status_codes,
};

use crate::DeviceContext;

const TEST_BOARD_TRANSFER_BUFFER_SIZE: usize = 64 * 1024;

pub fn evt_io_read(queue: &IoQueue, mut request: Request, length: usize) {
    println!("I/O read callback called");

    if length > TEST_BOARD_TRANSFER_BUFFER_SIZE {
        println!(
            "Requested read length {} exceeds maximum of {}",
            length, TEST_BOARD_TRANSFER_BUFFER_SIZE
        );
        request.complete_with_information(status_codes::STATUS_INVALID_PARAMETER.into(), 0);
        return;
    }

    let device_context = DeviceContext::get(&queue.get_device()).expect("Device context should be set");
    let pipe = device_context.get_bulk_read_pipe().expect("Bulk read pipe should be present");

    if let Err(e) = pipe.format_request_for_read(&mut request, RequestFormatBuffer::RequestBuffer(None)) {
        println!("Failed to initiate read on bulk read pipe: {:?}", e);
        request.complete_with_information(e.code().into(), 0);
        return;
    }



    

}

pub fn evt_io_write(queue: &IoQueue, request: Request, length: usize) {
    println!("I/O write callback called");

    if length > TEST_BOARD_TRANSFER_BUFFER_SIZE {
        println!(
            "Requested write length {} exceeds maximum of {}",
            length, TEST_BOARD_TRANSFER_BUFFER_SIZE
        );
        request.complete_with_information(status_codes::STATUS_INVALID_PARAMETER.into(), 0);
        return;
    }
}

pub fn evt_io_stop(_queue: &IoQueue, _request_id: RequestId, _action_flags: RequestStopActionFlags) {
    println!("I/O stop callback called");
}