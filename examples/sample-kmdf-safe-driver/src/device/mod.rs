//! Device creation, interface, and PnP power callbacks.
//! Module depth 1: `device`. Imports from `device::queue` at depth 2.

pub mod queue;

use wdf::{
    println,
    trace,
    Device,
    DeviceInit,
    Guid,
    NtResult,
    NtStatus,
    PnpPowerEventCallbacks,
};

/// Called by the framework in response to AddDevice from the PNP manager.
pub fn evt_device_add(device_init: &mut DeviceInit) -> NtResult<()> {
    trace!(FLAG_ONE, Information, "device: evt_device_add entered");
    device_create(device_init)
}

/// Creates the device, registers PnP callbacks, and initializes the queue.
fn device_create(device_init: &mut DeviceInit) -> NtResult<()> {
    trace!(FLAG_ONE, "device: creating device object");

    let mut pnp_power_callbacks = PnpPowerEventCallbacks::default();
    pnp_power_callbacks.evt_device_self_managed_io_init =
        Some(queue::evt_device_self_managed_io_start);
    pnp_power_callbacks.evt_device_self_managed_io_suspend =
        Some(queue::evt_device_self_managed_io_suspend);
    pnp_power_callbacks.evt_device_self_managed_io_restart =
        Some(queue::evt_device_self_managed_io_start);

    let device = Device::create(device_init, Some(pnp_power_callbacks))?;

    let _ = device.create_device_interface(
        &Guid::parse("2aa02ab1-c26e-431b-8efe-85ee8de102e4").expect("GUID is valid"),
        None,
    )?;

    trace!(FLAG_ONE, "device: device interface created");

    queue::queue_initialize(&device)?;

    trace!(FLAG_ONE, "device: device_create complete, status = %!STATUS!", NtStatus::from(0));
    Ok(())
}
