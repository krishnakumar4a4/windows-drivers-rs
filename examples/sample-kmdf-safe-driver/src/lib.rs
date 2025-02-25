//! A Sample KMDF driver implemented in safe Rust

#![no_std]

use wdf::{driver_entry, object_context, Guid, println, Device, DeviceInit, Driver, IoQueue, IoQueueConfig, trace, NtError, NtStatus};

#[object_context(Device)]
struct DeviceState {
    ver: i32
}

#[driver_entry]
fn driver_entry(driver: &mut Driver, registry_path: &str) -> Result<(), i32> {
    println!("Safe Rust driver entry called. Registry path: {registry_path}");

    driver.on_evt_device_add(device_add);

    let control_guid = Guid::parse("cb94defb-592a-4509-8f2e-54f204929669").expect("GUID is valid");
    driver.enable_tracing(control_guid);

    trace("Trace: Safe Rust driver entry complete");

    Ok(())
}

fn device_add(device_init: &mut DeviceInit) -> Result<(), NtError> {
    println!("Safe Rust device add called");

    let device = Device::create(device_init)?;

    let mut queue_config = IoQueueConfig::default();

    queue_config.evt_io_read = Some(|_queue, mut request, _| {
        println!("Safe Rust evt_io_read called");
        request.complete(NtStatus::Success);
    });

    queue_config.evt_io_write = Some(|_queue, mut request, _| {
        println!("Safe Rust evt_io_write called");
        request.complete(NtStatus::Success);
    });

    queue_config.evt_io_default = Some(|_queue, mut request| {
        println!("Safe Rust evt_io_default called");
        request.complete(NtStatus::Success);
    });

    let _ = IoQueue::create(&device, &queue_config)?;

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
