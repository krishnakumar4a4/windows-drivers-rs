// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Power management callbacks module.

use wdf::{println, trace, Device, NtResult};

/// Called when the device's self-managed I/O starts or restarts.
pub fn evt_self_managed_io_start(device: &Device) -> NtResult<()> {
    println!("Self-managed I/O start: {:?}", device);
    trace!(
        FLAG_POWER,
        Information,
        "Trace: self-managed I/O start"
    );

    // Use dynamic lib from power module
    let val = sample_dynamic_lib::dynamic_compute(5, 3);
    trace!(
        FLAG_POWER,
        "Trace: dynamic compute in power module: {}",
        val: i32
    );

    // Use external lib from power module
    let config = sample_external_lib::external_get_config();
    trace!(
        FLAG_POWER,
        Verbose,
        "Trace: external config value: {}",
        config: i32
    );

    Ok(())
}

/// Called when the device's self-managed I/O is suspended.
pub fn evt_self_managed_io_suspend(device: &Device) -> NtResult<()> {
    println!("Self-managed I/O suspend: {:?}", device);
    trace!(
        FLAG_POWER,
        Information,
        "Trace: self-managed I/O suspend"
    );

    // Use static lib from power module
    let val = sample_static_lib::static_helper(99);
    trace!(
        FLAG_POWER,
        "Trace: static helper in suspend: {}",
        val: i32
    );

    // Incremental compilation test: add new trace in existing module
    trace!(
        FLAG_POWER,
        Verbose,
        "Trace: power suspend complete with val {}",
        val: i32
    );

    Ok(())
}
