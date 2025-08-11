//! USB FX2 sample driver

#![no_std]

use wdf::{Driver, NtResult, driver_entry, trace};

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
#[driver_entry(tracing_control_guid = "cb94defb-592a-4509-8f2e-54f204929669")]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    // Set up the device add callback
    driver.on_evt_device_add(evt_device_add);

    trace("Trace: Safe Rust driver entry complete");

    Ok(())
}

fn evt_device_add(device_init: &mut wdf::DeviceInit) -> NtResult<()> {
    // Create the device
    let _device = wdf::Device::create(device_init, None)?;

    trace("Trace: Device created successfully");

    Ok(())
}
