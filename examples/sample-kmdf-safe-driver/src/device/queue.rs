//! Queue initialisation, I/O callbacks, and timer logic.
//! Module depth 2: `device::queue`.

use alloc::{vec, vec::Vec};

use wdf::{
    object_context,
    println,
    status_codes,
    trace,
    Arc,
    CancellableRequest,
    Device,
    IoQueue,
    IoQueueConfig,
    IoQueueDispatchType,
    NtResult,
    Request,
    RequestCancellationToken,
    SpinLock,
    Timer,
    TimerConfig,
};

use core::time::Duration;

const MAX_WRITE_LENGTH: usize = 1024 * 40;

/// Context object to be attached to a queue
#[object_context(IoQueue)]
pub(crate) struct QueueContext {
    request: SpinLock<Option<CancellableRequest>>,
    buffer: SpinLock<Option<Vec<u8>>>,
    timer: Arc<Timer>,
}

/// Context object to be attached to a timer
#[object_context(Timer)]
struct TimerContext {
    queue: Arc<IoQueue>,
}

/// Creates and configures the I/O queue for the device.
pub fn queue_initialize(device: &Device) -> NtResult<()> {
    trace!(FLAG_IO, Information, "queue: initializing I/O queue for device");

    let mut queue_config = IoQueueConfig::new_default(IoQueueDispatchType::Sequential);
    queue_config.default_queue = true;
    queue_config.evt_io_read = Some(evt_io_read);
    queue_config.evt_io_write = Some(evt_io_write);

    let queue = IoQueue::create(device, &queue_config)?;

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

    trace!(FLAG_IO, "queue: I/O queue initialized successfully");
    Ok(())
}

/// Called when the device is started or restarted after suspend.
pub fn evt_device_self_managed_io_start(device: &Device) -> NtResult<()> {
    trace!(FLAG_STATE, Information, "queue: self-managed I/O start");

    let queue = device
        .get_default_queue()
        .expect("Failed to get default queue");

    queue.start();
    let context = QueueContext::get(&queue);
    let _ = context.timer.start(&Duration::from_millis(100));

    trace!(FLAG_STATE, "queue: timer started, period = %d ms", 100i32);
    Ok(())
}

/// Called when the device is stopped for rebalance or entering Sx.
pub fn evt_device_self_managed_io_suspend(device: &Device) -> NtResult<()> {
    trace!(FLAG_STATE, Information, "queue: self-managed I/O suspend");

    let queue = device
        .get_default_queue()
        .expect("Failed to get default queue");

    queue.stop_synchronously();
    let context = QueueContext::get(&queue);
    context.timer.stop(false);

    trace!(FLAG_STATE, "queue: timer stopped");
    Ok(())
}

fn evt_io_read(queue: &IoQueue, mut request: Request, length: usize) {
    trace!(FLAG_IO, "queue: evt_io_read, length = %Iu", length);

    let context = QueueContext::get(queue);
    let memory = match request.retrieve_output_memory() {
        Ok(memory) => memory,
        Err(e) => {
            trace!(FLAG_IO, Error, "queue: read failed to get memory, status = %d", e.code());
            request.complete(e.into());
            return;
        }
    };

    let length = {
        let buffer = context.buffer.lock();
        let Some(buffer) = buffer.as_ref() else {
            trace!(FLAG_IO, "queue: read with no buffer set, completing with 0 bytes");
            request.complete_with_information(status_codes::STATUS_SUCCESS.into(), 0);
            return;
        };

        let mut length = length;
        if buffer.len() < length {
            length = buffer.len();
        }

        if let Err(e) = memory.copy_from_buffer(0, buffer) {
            trace!(FLAG_IO, Error, "queue: read copy failed, status = %d", e.code());
            request.complete(e.into());
            return;
        }

        length
    };

    request.set_information(length);

    if let Err((e, request)) = request.mark_cancellable(evt_request_cancel, &context.request) {
        trace!(FLAG_IO, Error, "queue: read mark_cancellable failed, status = %d", e.code());
        request.complete(status_codes::STATUS_UNSUCCESSFUL.into());
    }
}

fn evt_io_write(queue: &IoQueue, request: Request, length: usize) {
    trace!(FLAG_IO, "queue: evt_io_write, length = %Iu", length);

    if length > MAX_WRITE_LENGTH {
        trace!(FLAG_IO, Warning, "queue: write too large: %Iu > max %Iu", length, MAX_WRITE_LENGTH);
        request.complete_with_information(status_codes::STATUS_BUFFER_OVERFLOW.into(), 0);
        return;
    }

    let memory = match request.retrieve_input_memory() {
        Ok(memory) => memory,
        Err(e) => {
            trace!(FLAG_IO, Error, "queue: write failed to get memory, status = %d", e.code());
            request.complete(e.into());
            return;
        }
    };

    let mut buffer = vec![0_u8; length];
    if let Err(e) = memory.copy_to_buffer(0, &mut buffer) {
        trace!(FLAG_IO, Error, "queue: write copy failed, status = %d", e.code());
        request.complete(e.into());
        return;
    }

    let context = QueueContext::get(queue);
    *context.buffer.lock() = Some(buffer);
    request.set_information(length);

    if let Err((e, request)) = request.mark_cancellable(evt_request_cancel, &context.request) {
        trace!(FLAG_IO, Error, "queue: write mark_cancellable failed, status = %d", e.code());
        request.complete(status_codes::STATUS_UNSUCCESSFUL.into());
    }
}

fn evt_request_cancel(token: &RequestCancellationToken) {
    trace!(FLAG_IO, "queue: request cancel callback");

    let queue = token.get_io_queue();
    let context = QueueContext::get(&queue);

    let mut req = context.request.lock();
    if let Some(req) = req.take() {
        req.complete(status_codes::STATUS_CANCELLED.into());
        trace!(FLAG_IO, "queue: request cancelled");
    } else {
        trace!(FLAG_IO, "queue: cancel called but request already completed");
    }
}

fn evt_timer(timer: &Timer) {
    trace!(FLAG_PERF, Verbose, "queue: timer fired");

    let queue = &TimerContext::get(timer).queue;
    let req = QueueContext::get(queue).request.lock().take();

    if let Some(req) = req {
        req.complete(status_codes::STATUS_SUCCESS.into());
        trace!(FLAG_PERF, "queue: request completed by timer");
    }
}
