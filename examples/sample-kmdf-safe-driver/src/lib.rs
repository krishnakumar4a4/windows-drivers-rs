//! A sample driver written in 100% safe Rust.
//! Demonstrates request processing and cancellation.
//! 
//! When a write request arrives it stores the request
//! in context object and starts a timer. When the timer
//! fires it completes the request. This simulates I/O
//! processing on real hardware. At any time before its
//! completion the request can be cancelled. Cancellation is
//! supported through the request cancellation callback.
//! 
//! This driver uses safe Rust abstractions provided by the
//! `wdf` crate located at the path `../../crates/wdf` relative
//! to this directory.
//! 
//! The design of everything over here and in the `wdf` crate is
//! at a very early stage. Some parts may appear subotimal or even
//! wrong. That is likely to change and improve over time.

#![no_std]

use wdf::{
    Arc, driver_entry, object_context, println, trace, CancellableMarkedRequest, Request,
    RequestCancellationToken, Device, DeviceInit, Driver, Guid, IoQueue,
    IoQueueConfig, NtError, NtResult, NtStatus, PnpPowerEventCallbacks, SpinLock, Timer,
    TimerConfig
};

use core::time::Duration;

extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;

const MAX_WRITE_LENGTH: usize = 1024*40;

/// Context object to be attached to a queue
#[object_context(IoQueue)]
struct QueueContext {
    // Field that stores the in-flight request.
    // The spin lock prevents concurrency issues
    // between request completion and cancellation.
    // The lock is enforced at compile time (i.e. the
    // code will fail to compile if you do not use
    // the lock).
    request: SpinLock<Option<CancellableMarkedRequest>>,

    // Buffer where data from incoming write request is stored
    buffer: SpinLock<Option<Vec<u8>>>,

    // The timer that is used to complete the request
    timer: Arc<Timer>,
}

/// Context object to be attached to a timer
#[object_context(Timer)]
struct TimerContext {
    queue: Arc<IoQueue>
}

/// The entry point for the driver. It initializes the driver and is the first
/// routine called by the system after the driver is loaded. `driver_entry`
/// specifies the other entry points in the function driver, such as
/// `evt_device_add` and `driver_unload`.
/// 
/// The #[driver_entry] attribute is used to mark the entry point.
/// It is a proc macro that generates the shim code which enables WDF
/// to call this driver
///
/// # Arguments
/// 
/// * `driver` - Represents the instance of the function driver that is loaded
/// into memory. `driver` object is allocated by the system before the
/// driver is loaded, and it is released by the system after the system unloads
/// the function driver from memory.
/// 
/// * `registry_path` - Represents the driver specific path in the Registry.
/// The function driver can use the path to store driver related data between
/// reboots. The path does not store hardware instance specific data.
#[driver_entry]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> Result<(), NtError> {
    if cfg!(debug_assertions) {
        print_driver_version(driver)?;
    }

    // Set up the device add callback
    driver.on_evt_device_add(evt_device_add);

    // Enable tracing
    let control_guid = Guid::parse("cb94defb-592a-4509-8f2e-54f204929669").expect("GUID is valid");
    driver.enable_tracing(control_guid);

    trace("Trace: Safe Rust driver entry complete");

    Ok(())
}

/// `evt_device_add` is called by the framework in response to AddDevice
/// call from the PnP manager. We create and initialize a device object to
/// represent a new instance of the device.
///
/// # Arguments
///
/// * `device_init` - Reference to a framework-allocated `DeviceInit` structure.
fn evt_device_add(device_init: &mut DeviceInit) -> Result<(), NtError> {
    println!("Enter evt_device_add");

    device_create(device_init)
}


/// Worker routine called to create a device and its software resources.
///
/// # Arguments
///
/// * `device_init` - Pointer to an opaque init structure. Memory for
/// this structure will be freed by the framework when the
/// WdfDeviceCreate succeeds. So don't access the structure after
/// that point.
fn device_create(device_init: &mut DeviceInit) -> NtResult<()> {
    // Register pnp/power callbacks so that we can start and stop the
    // timer as the device gets started and stopped.
    let mut pnp_power_callbacks = PnpPowerEventCallbacks::default();
    pnp_power_callbacks.evt_device_self_managed_io_init = Some(evt_device_self_managed_io_start);
    pnp_power_callbacks.evt_device_self_managed_io_suspend = Some(evt_device_self_managed_io_suspend);
    pnp_power_callbacks.evt_device_self_managed_io_restart = Some(evt_device_self_managed_io_start);

    let device = Device::create(device_init, Some(pnp_power_callbacks))?;

    // Create a device interface so that application can find and talk
    // to us.
    let _ = device.create_interface(
        &Guid::parse("2aa02ab1-c26e-431b-8efe-85ee8de102e4").expect("GUID is valid"),
        None
    )?; 

    queue_initialize(&device)
}

/// Callback for starting self-managed I/O
fn evt_device_self_managed_io_start(device: &Device) -> NtResult<()>{
    println!("Self-managed I/O start called: {:?}", device);

    let queue = device.get_default_queue().
        expect("Failed to get default queue");

    queue.start();

    let context = QueueContext::get(&queue)
        .expect("Failed to get queue context"); 

    let _ = context.timer.start(&Duration::from_millis(100));

    Ok(())
}

/// Callback for stopping self-managed I/O
fn evt_device_self_managed_io_suspend(device: &Device) -> NtResult<()> {
    println!("Self-managed I/O suspend called: {:?}", device);

    let queue = device.get_default_queue().
        expect("Failed to get default queue");

    queue.stop_synchronously();

    let context = QueueContext::get(&queue)
        .expect("Failed to get queue context"); 

    context.timer.stop(false);

    Ok(())
}

/// The I/O dispatch callbacks for the frameworks device object
/// are configured in this function.
/// 
/// A single default I/O Queue is configured for serial request
/// processing, and queue context is set up. The lifetime ofthe
/// context is tied to the lifetime of the I/O Queue object.
/// 
/// # Arguments
/// 
/// * `device`` - Handle to a framework device object.
fn queue_initialize(device: &Device) -> NtResult<()> {
    // Create queue
    let mut queue_config = IoQueueConfig::default();

    queue_config.default_queue = true;
    queue_config.evt_io_write = Some(evt_io_write);
    queue_config.evt_io_write = Some(evt_io_read);

    let queue = IoQueue::create(&device, &queue_config)?; // The `?` operator is used to propagate errors to the caller

    // Create timer
    let timer_config = TimerConfig::new_periodic(&queue, evt_timer, 9_000, 0, false);

    let timer = Timer::create(&timer_config)?;

    // Attach context to the timer
    let timer_context = TimerContext {
        queue: queue.clone()
    };

    TimerContext::attach(&timer, timer_context)?;

    // Attach context to the queue
    let queue_context = QueueContext {
        request: SpinLock::create(None)?,
        buffer: SpinLock::create(None)?,
        timer,
    };

    QueueContext::attach(&queue, queue_context)?;
    
    Ok(())
}

/// This callback is invoked when the framework receives IRP_MJ_WRITE request.
/// It copies the data from the request into a buffer stored in the queue context.
/// The actual completion of the request is deferred to the periodic timer.
/// 
/// # Arguments
/// 
/// * `queue` - Handle to the framework queue object that is associated with the
///             I/O request.
/// * `Request` - Handle to a framework request object.
/// 
/// * `Length`  - number of bytes to be read.
///             The default property of the queue is to not dispatch
///             zero lenght read & write requests to the driver and
///             complete is with status success. So we will never get
///             a zero length request.
fn evt_io_write(queue: &IoQueue, request: Request, length: usize) {
    println!("evt_io_write called. Queue {queue:?}, Request {request:?} Length {length}");

    if length > MAX_WRITE_LENGTH {
        println!("evt_io_write buffer length too big {length}. Max is {MAX_WRITE_LENGTH}");
        request.complete_with_information(NtStatus::buffer_overflow(), 0);
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

    let Some(context) = QueueContext::get(&queue) else {
        println!("evt_io_write failed to get queue context");
        request.complete(NtStatus::Error(NtError::from(1))); // TODO: decide on the status code here
        return;
    };

    *context.buffer.lock() = Some(buffer);

    request.set_information(length);

    let request = match request.mark_cancellable(evt_request_cancel) {
        Ok(request) => request,
        Err((e, request)) => {
            println!("evt_io_write failed to mark request cancellable: {e:?}");
            request.complete(NtStatus::Error(NtError::from(1))); // TODO: decide on the status code here
            return;
        }
    };

    *context.request.lock() = Some(request);
}

/// This callback is invoked when the framework receives IRP_MJ_READ request.
/// It copies the data from the queue context buffer to the request buffer.
/// If the driver hasn't received any write request earlier, it returns 0.
/// The actual completion of the request is deferred to the periodic timer.
///
/// # Arguments
/// 
/// * `queue` - Handle to the framework queue object that is associated with the
///             I/O request.
/// * `Request` - Handle to a framework request object.
/// 
/// * `Length`  - number of bytes to be read.
///             The default property of the queue is to not dispatch
///             zero lenght read & write requests to the driver and
///             complete is with status success. So we will never get
///             a zero length request.
fn evt_io_read(queue: &IoQueue, mut request: Request, length: usize) {
    println!("evt_io_read called. Queue {queue:?}, Request {request:?} Length {length}");

    let Some(context) = QueueContext::get(&queue) else {
        println!("evt_io_write failed to get queue context");
        request.complete(NtStatus::Error(NtError::from(1))); // TODO: decide on the status code here
        return;
    };

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
            request.complete_with_information(NtStatus::Success, 0);
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

    let request = match request.mark_cancellable(evt_request_cancel) {
        Ok(request) => request,
        Err((e, request)) => {
            println!("evt_io_write failed to mark request cancellable: {e:?}");
            request.complete(NtStatus::Error(NtError::from(1))); // TODO: decide on the status code here
            return;
        }
    };

    *context.request.lock() = Some(request);
}

/// Callback that is called when the request is cancelled.
/// It cancels the request identified by the `token` parameter
/// if it is found in the context.
fn evt_request_cancel(token: &RequestCancellationToken) {
    println!("evt_request_cancel called");

    let queue = token.get_io_queue();

    if let Some(context) = QueueContext::get(&queue) {
        let mut req = context.request.lock();
        if let Some(req) = req.take() {
            req.complete(NtStatus::cancelled());
            println!("Request cancelled");
        } else {
            println!("Request already completed");
        }
    } else {
        println!("Could not cancel request. Failed to get queue context");
    }
}

/// Callback that is called when the timer fires.
/// It fetches the request stored in the context
/// and completes it
fn evt_timer(timer: &Timer) {
    println!("evt_timer called");

    let queue = &TimerContext::get(timer)
        .expect("Failed to get timer context")
        .queue;

    let req = QueueContext::get(queue)
        .and_then(|context| context.request.lock().take());

    if let Some(req) = req {
        req.complete(NtStatus::Success);
        println!("Request completed");
    } else {
        println!("No request pending");
    }
}

/// This routine shows how to retrieve framework version string and
/// also how to find out to which version of framework library the
/// client driver is bound to.
fn print_driver_version(driver: &Driver) -> NtResult<()> {
    let driver_version = driver.retrieve_version_string()?;
    println!("Echo Sample {driver_version}");

    if driver.is_version_available(1, 0) {
        println!("Yes, framework version is 1.0");
    } else {
        println!("No, framework verison is not 1.0");
    }

    Ok(())
}