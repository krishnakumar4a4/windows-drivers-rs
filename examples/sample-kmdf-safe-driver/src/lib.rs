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
// #![feature(codeview_annotation)]
// #![feature(core_intrinsics)]

extern crate alloc;

use core::time::Duration;

// use core::intrinsics::codeview_annotation;

mod io;
mod utils;

use wdf::{
    driver_entry,
    println,
    status_codes,
    trace,
    Device,
    DeviceInit,
    Driver,
    Guid,
    NtResult,
    PnpPowerEventCallbacks,
    tracing::TraceWriter,
    get_trace_writer,
};

use io::QueueContext;

pub(crate) const MAX_WRITE_LENGTH: usize = 1024 * 40;

/// The entry point for the driver. It initializes the driver and is the first
/// routine called by the system after the driver is loaded. `driver_entry`
/// specifies the other entry points in the function driver such as
/// `evt_device_add`.
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
#[driver_entry(trace_control = ("cb94defb-592a-4509-8f2e-54f204929669", [FLAG_ONE, FLAG_TWO]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    if cfg!(debug_assertions) {
        utils::print_driver_version(driver)?;
    }

    // Set up the device add callback
    driver.set_evt_device_add(evt_device_add);

    // core::hint::codeview_annotation!("TMF:", "e7602a7b-5034-321b-d450-a986113fc2e1 sample_kmdf_safe_driver // SRC=lib.rs MJ= MN=",
    // "#typev sample_kmdf_safe_driver_109 10 \"%0Trace: Safe Rust driver entry complete %10!d!\"", "{", "test, ItemLong -- 10", "}");

    for i in 0..1000 {
        // Use the new type annotation syntax: `variable: Type`
        trace!("Trace: Safe Rust driver entry trace, int - {}, int - {}, int - {}, int - {}, int - {}, int - {}", i: i32, i: i32, i: i32, i: i32, i: i32, i: i32);
    }

    trace!(FLAG_ONE, "1 Trace: Safe Rust driver entry complete, int - {}", 1001);

    trace!(FLAG_TWO, "2 Trace: Safe Rust driver entry complete, int - {}", 1002);

    // Trace with level only (no flag)
    trace!(Information, "3 Trace: Safe Rust driver entry complete, int - {}", 1003);

    // Trace with level only (no flag)
    trace!(Verbose, "4 Trace: Safe Rust driver entry complete, int - {}", 1004);

    trace!(FLAG_TWO, Information, "5 Trace: Safe Rust driver entry complete, int - {}", 1005);

    trace!("Trace: Safe Rust driver entry with basic data, int - {}, str - {}", 9999, "hello");

    trace!(FLAG_TWO, Information, "Trace: Safe Rust driver entry complete, int - {}, str - {}", 1006, "examplestring!@#$%^&*()_+-=1234567890`~[]{}|;:'\"<>,./?  E");

    Ok(())
}

/// `evt_device_add` is called by the framework in response to AddDevice
/// call from the PNP manager.
fn evt_device_add(device_init: &mut DeviceInit) -> NtResult<()> {
    println!("Enter evt_device_add");

    device_create(device_init)
}

/// Worker routine called to create a device and its software resources.
fn device_create(device_init: &mut DeviceInit) -> NtResult<()> {
    let mut pnp_power_callbacks = PnpPowerEventCallbacks::default();
    pnp_power_callbacks.evt_device_self_managed_io_init = Some(evt_device_self_managed_io_start);
    pnp_power_callbacks.evt_device_self_managed_io_suspend =
        Some(evt_device_self_managed_io_suspend);
    pnp_power_callbacks.evt_device_self_managed_io_restart = Some(evt_device_self_managed_io_start);

    let device = Device::create(device_init, Some(pnp_power_callbacks))?;

    let _ = device.create_device_interface(
        &Guid::parse("2aa02ab1-c26e-431b-8efe-85ee8de102e4").expect("GUID is valid"),
        None,
    )?;

    io::queue_initialize(&device)
}

/// This callback is called by the Framework when the device is started
/// or restarted after a suspend operation.
fn evt_device_self_managed_io_start(device: &Device) -> NtResult<()> {
    println!("Self-managed I/O start called: {:?}", device);

    let queue = device
        .get_default_queue()
        .expect("Failed to get default queue");

    queue.start();

    let context = QueueContext::context(&queue);

    let _ = context.timer.start(&Duration::from_millis(100));

    Ok(())
}

/// This callback is called by the Framework when the device is stopped
/// for resource rebalance or suspended when the system is entering
/// Sx state.
fn evt_device_self_managed_io_suspend(device: &Device) -> NtResult<()> {
    println!("Self-managed I/O suspend called: {:?}", device);

    let queue = device
        .get_default_queue()
        .expect("Failed to get default queue");

    queue.stop_synchronously();

    let context = QueueContext::context(&queue);

    context.timer.stop(false);

    Ok(())
}
