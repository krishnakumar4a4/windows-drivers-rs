// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Device creation and configuration module.

use wdf::{println, trace, Device, DeviceInit, Guid, NtResult, PnpPowerEventCallbacks};

use crate::power;
use crate::queue;

/// Creates and configures the WDF device object.
pub fn create_device(device_init: &mut DeviceInit) -> NtResult<()> {
    trace!(
        FLAG_DEVICE,
        Information,
        "Trace: create_device entered"
    );

    let mut pnp_power_callbacks = PnpPowerEventCallbacks::default();
    pnp_power_callbacks.evt_device_self_managed_io_init = Some(power::evt_self_managed_io_start);
    pnp_power_callbacks.evt_device_self_managed_io_suspend =
        Some(power::evt_self_managed_io_suspend);
    pnp_power_callbacks.evt_device_self_managed_io_restart =
        Some(power::evt_self_managed_io_start);

    let device = Device::create(device_init, Some(pnp_power_callbacks))?;

    let _ = device.create_device_interface(
        &Guid::parse("3bb03bc2-d37f-542c-9f0f-96ff224103f5").expect("GUID is valid"),
        None,
    )?;

    trace!(FLAG_DEVICE, "Trace: device interface created");

    queue::initialize_queues(&device)?;

    // Use external lib from device module
    let val = sample_external_lib::external_compute(10, 20);
    trace!(
        FLAG_DEVICE,
        Information,
        "Trace: external compute in device module: {}",
        val: i32
    );

    trace!(
        FLAG_DEVICE,
        Information,
        "Trace: create_device complete"
    );
    Ok(())
}
