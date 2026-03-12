//! I/O subsystem: queue initialization, read/write handlers, and timer.

pub mod handlers;
pub mod timer;

use wdf::{
    object_context, println, trace, Arc, CancellableRequest, Device, IoQueue, IoQueueConfig,
    IoQueueDispatchType, NtResult, SpinLock, Timer, TimerConfig,
};

use alloc::vec::Vec;

/// Context object to be attached to a queue
#[object_context(IoQueue)]
pub struct QueueContext {
    /// Field that stores the in-flight request.
    pub request: SpinLock<Option<CancellableRequest>>,

    /// Buffer where data from incoming write request is stored
    pub buffer: SpinLock<Option<Vec<u8>>>,

    /// The timer that is used to complete the request
    pub timer: Arc<Timer>,
}

// The `object_context` macro generates private `get`/`attach` methods.
// Expose `get` as `pub(crate)` so callers in other modules (e.g. lib.rs)
// can retrieve the context.
impl QueueContext {
    /// Returns a reference to the queue context attached to the given queue.
    pub(crate) fn context(queue: &IoQueue) -> &QueueContext {
        Self::get(queue)
    }
}

/// Context object to be attached to a timer
#[object_context(Timer)]
pub struct TimerContext {
    /// The queue associated with this timer
    pub queue: Arc<IoQueue>,
}

/// The I/O dispatch callbacks for the frameworks device object
/// are configured in this function.
///
/// A single default I/O Queue is configured for serial request
/// processing, and queue context is set up.
pub fn queue_initialize(device: &Device) -> NtResult<()> {
    // Create queue
    let mut queue_config = IoQueueConfig::new_default(IoQueueDispatchType::Sequential);
    queue_config.default_queue = true;
    queue_config.evt_io_read = Some(handlers::evt_io_read);
    queue_config.evt_io_write = Some(handlers::evt_io_write);

    let queue = IoQueue::create(&device, &queue_config)?;

    trace!(Information, "Trace: queue_initialize - queue created for device {}", 1);

    // Create timer
    let timer_config = TimerConfig::new_periodic(&queue, timer::evt_timer, 9_000, 0, false);

    let timer = Timer::create(&timer_config)?;

    // Attach context to the timer
    let timer_context = TimerContext {
        queue: queue.clone(),
    };

    TimerContext::attach(&timer, timer_context)?;

    // Attach context to the queue
    let queue_context = QueueContext {
        request: SpinLock::create(None)?,
        buffer: SpinLock::create(None)?,
        timer,
    };

    QueueContext::attach(&queue, queue_context)?;

    // core::hint::codeview_annotation!("Trace: Queue initialized");
    // trace!("Trace: Queue initialized {}", 43);
    Ok(())
}
