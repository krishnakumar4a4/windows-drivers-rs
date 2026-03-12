//! I/O request handlers: read, write, and cancellation callbacks.

use wdf::{println, status_codes, trace, IoQueue, Request, RequestCancellationToken};

use super::QueueContext;
use crate::MAX_WRITE_LENGTH;

use alloc::{vec, vec::Vec};

/// This callback is invoked when the framework receives IRP_MJ_READ request.
pub fn evt_io_read(queue: &IoQueue, mut request: Request, length: usize) {
    println!("evt_io_read called. Queue {queue:?}, Request {request:?} Length {length}");
    // trace!("Trace: evt_io_read called with values int - {}, str - {}", 50, "evt_io_read");
    // trace("<Trace: evt_io_read called>");

    trace!(FLAG_ONE, Information, "Trace: evt_io_read entry, length - {}", length: i32);

    let context = QueueContext::get(&queue);
    let memory = match request.retrieve_output_memory() {
        Ok(memory) => memory,
        Err(e) => {
            println!("evt_io_read could not get request memory buffer {e:?}");
            request.complete(e.into());
            return;
        }
    };

    // Nested scope to limit the lifetime of the lock
    let length = {
        // TODO: this lock is problematic because we call out into
        // the framework while holding it. Doing so is generally
        // considered a recipe for deadlocks although copy_from_buffer
        // specifically won't cause any. Still this is a bad pattern
        // in general and we have to find a way to avoid it.
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

    if let Err((e, request)) = request.mark_cancellable(evt_request_cancel, &context.request) {
        println!("evt_io_write failed to mark request cancellable: {e:?}");
        request.complete(status_codes::STATUS_UNSUCCESSFUL.into());
    }
}

/// This callback is invoked when the framework receives IRP_MJ_WRITE request.
pub fn evt_io_write(queue: &IoQueue, request: Request, length: usize) {
    println!("evt_io_write called. Queue {queue:?}, Request {request:?} Length {length}");
    // core::hint::codeview_annotation!("Trace: evt_io_write called");
    // trace("Trace: evt_io_write called");

    trace!(FLAG_TWO, "Trace: evt_io_write entry, length - {}", length: i32);

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

    let context = QueueContext::get(&queue);

    *context.buffer.lock() = Some(buffer);

    request.set_information(length);

    if let Err((e, request)) = request.mark_cancellable(evt_request_cancel, &context.request) {
        println!("evt_io_write failed to mark request cancellable: {e:?}");
        request.complete(status_codes::STATUS_UNSUCCESSFUL.into());
    }
}

/// Callback that is called when the request is cancelled.
pub fn evt_request_cancel(token: &RequestCancellationToken) {
    println!("evt_request_cancel called");
    // core::hint::codeview_annotation!("Trace: evt_request_cancel called");
    // trace("Trace: evt_request_cancel called");

    trace!("Trace: evt_request_cancel invoked");

    let queue = token.get_io_queue();

    let context = QueueContext::get(&queue);

    let mut req = context.request.lock();
    if let Some(req) = req.take() {
        req.complete(status_codes::STATUS_CANCELLED.into());
        println!("Request cancelled");
    } else {
        println!("Request already completed");
    }
}
