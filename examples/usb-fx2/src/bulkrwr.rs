use wdf::{
    Device,
    IoQueue,
    println,
    Request,
    RequestId,
    RequestStopActionFlags,
};
pub fn evt_io_read(_queue: &IoQueue, _request: Request, _length: usize) {
    println!("I/O read callback called");
}

pub fn evt_io_write(_queue: &IoQueue, _request: Request, _length: usize) {
    println!("I/O write callback called");
}

pub fn evt_io_stop(_queue: &IoQueue, _request_id: RequestId, _action_flags: RequestStopActionFlags) {
    println!("I/O stop callback called");
}