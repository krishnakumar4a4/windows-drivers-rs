//! A Sample KMDF driver implemented in safe Rust

#![no_std]

use wdf::{driver_entry, Guid, println, DeviceInit, Driver, trace, NtError};

#[driver_entry]
fn driver_entry(driver: &mut Driver, registry_path: &str) -> Result<(), i32> {
    println!("Safe Rust driver entry called. Registry path: {registry_path}");

    driver.on_evt_device_add(device_add);

    let control_guid = Guid::parse("cb94defb-592a-4509-8f2e-54f204929669").expect("GUID is valid");
    driver.enable_tracing(control_guid);

    trace("Trace: Safe Rust driver entry complete");

    Ok(())
}

fn device_add(_: &mut DeviceInit) -> Result<(), NtError> {
    println!("Safe Rust device add called");
    trace("Trace: Safe Rust device add complete");
    Ok(())
}


// Alternative programming model using a struct
// and an impl instead of free functions

// use wdf::driver_impl;
// // Struct representing the driver
// struct MyDriver;

// // Impl containing the driver's callbacks/logic
// #[driver_impl]
// // #[trace_config(control_guid="cb94defb-592a-4509-8f2e-54f204929669", ifr_enabled=true, ...)]
// impl MyDriver {
//     fn driver_entry(&self) -> Result<(), i32> {
//         Ok(())
//      }

//     fn evt_driver_device_add(&self) -> Result<(), i32> {
//         Ok(())
//     }
// }
