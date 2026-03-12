//! Timer callback for deferred request completion.

use wdf::{println, status_codes, trace, Timer};

use super::{QueueContext, TimerContext};

/// Callback that is called when the timer fires.
/// It fetches the request stored in the context and completes it.
pub fn evt_timer(timer: &Timer) {
    println!("evt_timer called");
    // core::hint::codeview_annotation!("Trace: evt_timer called");

    trace!(Verbose, "Trace: evt_timer fired");

    let queue = &TimerContext::get(timer).queue;

    let req = QueueContext::get(queue).request.lock().take();

    if let Some(req) = req {
        req.complete(status_codes::STATUS_SUCCESS.into());
        println!("Request completed");
    } else {
        println!("No request pending");
    }
}

