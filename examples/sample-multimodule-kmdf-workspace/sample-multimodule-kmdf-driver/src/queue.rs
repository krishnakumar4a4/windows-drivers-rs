// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Queue initialization and I/O request handling module.

use wdf::{
    println, status_codes, trace, Device, IoQueue, IoQueueConfig, IoQueueDispatchType, NtResult,
    Request,
};

/// Creates and configures the default I/O queue for the device.
pub fn initialize_queues(device: &Device) -> NtResult<()> {
    trace!(
        FLAG_QUEUE,
        Information,
        "Trace: initialize_queues entered"
    );

    let mut queue_config = IoQueueConfig::new_default(IoQueueDispatchType::Sequential);
    queue_config.default_queue = true;
    queue_config.evt_io_write = Some(evt_io_write);

    let _queue = IoQueue::create(device, &queue_config)?;

    trace!(FLAG_QUEUE, "Trace: default queue created");

    // Use static lib helper in queue module
    let val = sample_static_lib::static_compute(10, 20);
    trace!(
        FLAG_QUEUE,
        Information,
        "Trace: static compute result {}",
        val: i32
    );

    Ok(())
}

fn evt_io_write(queue: &IoQueue, request: Request, length: usize) {
    println!("evt_io_write called, length {length}");
    trace!(
        FLAG_QUEUE,
        Information,
        "Trace: evt_io_write called with length {}",
        length: i32
    );

    // Use dynamic lib in write handler
    let result = sample_dynamic_lib::dynamic_compute(length as i32, 2);
    trace!(
        FLAG_QUEUE,
        "Trace: dynamic compute in write handler: {}",
        result: i32
    );

    request.complete_with_information(status_codes::STATUS_SUCCESS.into(), length);
}
