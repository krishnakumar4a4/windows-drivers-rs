// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! # Sample Multi-Module KMDF Driver
//!
//! This driver demonstrates:
//! - Multiple modules within a single driver crate (device, queue, power)
//! - Consuming a statically linked internal library (`sample-static-lib`)
//! - Consuming a dynamically linked internal library (`sample-dynamic-lib`)
//! - Consuming an external path dependency (`sample-external-lib`)
//! - Trace macros across all modules and library boundaries

#![no_std]

extern crate alloc;

mod device;
mod power;
mod queue;

use wdf::{driver_entry, println, trace, DeviceInit, Driver, NtResult};

/// The entry point for the multi-module driver.
#[driver_entry(trace_control = ("a1b2c3d4-e5f6-7890-abcd-ef1234567890", [FLAG_DEVICE, FLAG_QUEUE, FLAG_POWER]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    println!("Multi-module KMDF driver entry");

    trace!("Trace: Driver entry started");
    trace!(
        FLAG_DEVICE,
        Information,
        "Trace: Initializing device subsystem, version {}",
        1
    );

    // Use statically linked internal library
    let result = sample_static_lib::static_helper(42);
    trace!(
        FLAG_DEVICE,
        "Trace: Static lib helper returned {}",
        result: i32
    );

    // Use dynamically linked internal library
    let result = sample_dynamic_lib::dynamic_helper(100);
    trace!(
        FLAG_QUEUE,
        "Trace: Dynamic lib helper returned {}",
        result: i32
    );

    // Use external path dependency (statically linked)
    let result = sample_external_lib::external_helper(200);
    trace!(
        FLAG_POWER,
        "Trace: External lib helper returned {}",
        result: i32
    );

    driver.set_evt_device_add(evt_device_add);

    trace!(Information, "Trace: Driver entry complete");
    Ok(())
}

fn evt_device_add(device_init: &mut DeviceInit) -> NtResult<()> {
    println!("Enter evt_device_add");
    trace!(
        FLAG_DEVICE,
        Information,
        "Trace: evt_device_add called"
    );
    device::create_device(device_init)
}
