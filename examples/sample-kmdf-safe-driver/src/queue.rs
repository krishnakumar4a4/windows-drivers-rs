//! Queue initialization and I/O callbacks.

use wdf::{
    object_context, println, status_codes, Arc, CancellableRequest, Device, IoQueue,
    IoQueueConfig, IoQueueDispatchType, NtResult, Request, RequestCancellationToken, SpinLock,
    Timer, TimerConfig,
};

extern crate alloc;
use alloc::{vec, vec::Vec};

use crate::device::TimerContext;
use crate::MAX_WRITE_LENGTH;

/// Context object to be attached to a queue
#[object_context(IoQueue)]
pub(crate) struct QueueContext {
    pub request: SpinLock<Option<CancellableRequest>>,
    pub buffer: SpinLock<Option<Vec<u8>>>,
    pub timer: Arc<Timer>,
}

pub(crate) fn queue_initialize(device: &Device) -> NtResult<()> {
    let mut queue_config = IoQueueConfig::new(IoQueueDispatchType::Sequential);
    queue_config.default_queue = true;
    queue_config.evt_io_read = Some(evt_io_read);
    queue_config.evt_io_write = Some(evt_io_write);

    let queue = IoQueue::create(&device, &queue_config)?;

    let timer_config = TimerConfig::new_periodic(&queue, evt_timer, 9_000, 0, false);
    let timer = Timer::create(&timer_config)?;

    let timer_context = TimerContext {
        queue: queue.clone(),
    };
    TimerContext::attach(&timer, timer_context)?;

    let queue_context = QueueContext {
        request: SpinLock::create(None)?,
        buffer: SpinLock::create(None)?,
        timer,
    };
    QueueContext::attach(&queue, queue_context)?;

    trace!(VERBOSE, GENERAL, "Queue initialized");

    Ok(())
}

fn evt_io_read(queue: &IoQueue, mut request: Request, length: usize) {
    println!("evt_io_read called. Queue {queue:?}, Request {request:?} Length {length}");

    let context = QueueContext::get(queue);
    let memory = match request.retrieve_output_memory() {
        Ok(memory) => memory,
        Err(e) => {
            println!("evt_io_read could not get request memory buffer {e:?}");
            request.complete(e.into());
            return;
        }
    };

    let length = {
        let buffer = context.buffer.lock();
        let Some(buffer) = buffer.as_ref() else {
            println!("evt_io_read called but no request buffer is set");
            request.complete_with_information(status_codes::STATUS_SUCCESS.into(), 0);
            return;
        };

        let mut length = length;
        if buffer.len() < length {
            length = buffer.len();
        }

        if let Err(e) = memory.copy_from_buffer(0, buffer) {
            println!("evt_io_read failed to copy buffer: {e:?}");
            request.complete(e.into());
            return;
        }

        length
    };

    request.set_information(length);

    let mut req_opt = context.request.lock();
    match request.mark_cancellable(evt_request_cancel) {
        Ok(request) => {
            *req_opt = Some(request);
        }
        Err((e, request)) => {
            println!("evt_io_read failed to mark request cancellable: {e:?}");
            request.complete(status_codes::STATUS_UNSUCCESSFUL.into());
        }
    }
}

fn evt_io_write(queue: &IoQueue, request: Request, length: usize) {
    println!("evt_io_write called. Queue {queue:?}, Request {request:?} Length {length}");

    if length > MAX_WRITE_LENGTH {
        println!("evt_io_write buffer length too big {length}. Max is {MAX_WRITE_LENGTH}");
        request.complete_with_information(status_codes::STATUS_BUFFER_OVERFLOW.into(), 0);
        return;
    }

    let memory = match request.retrieve_input_memory() {
        Ok(memory) => memory,
        Err(e) => {
            println!("evt_io_write could not get request memory buffer {e:?}");
            request.complete(e.into());
            return;
        }
    };

    let mut buffer = vec![0_u8; length];

    if let Err(e) = memory.copy_to_buffer(0, &mut buffer) {
        println!("evt_io_write failed to copy buffer: {e:?}");
        request.complete(e.into());
        return;
    }

    let context = QueueContext::get(queue);
    *context.buffer.lock() = Some(buffer);
    request.set_information(length);

    let mut req_opt = context.request.lock();
    match request.mark_cancellable(evt_request_cancel) {
        Ok(request) => {
            *req_opt = Some(request);
        }
        Err((e, request)) => {
            println!("evt_io_write failed to mark request cancellable: {e:?}");
            request.complete(status_codes::STATUS_UNSUCCESSFUL.into());
        }
    }
}

fn evt_request_cancel(token: &RequestCancellationToken) {
    println!("evt_request_cancel called");

    let queue = token
        .get_io_queue()
        .expect("Queue must be available for this request");

    let context = QueueContext::get(queue);
    let mut req = context.request.lock();
    if let Some(req) = req.take() {
        req.complete(status_codes::STATUS_CANCELLED.into());
        println!("Request cancelled");
    } else {
        println!("Request already completed");
    }
}

fn evt_timer(timer: &Timer) {
    println!("evt_timer called");

    let queue = &TimerContext::get(timer).queue;
    let req = QueueContext::get(queue).request.lock().take();

    if let Some(req) = req {
        req.complete(status_codes::STATUS_SUCCESS.into());
        println!("Request completed");
    } else {
        println!("No request pending");
    }
}
