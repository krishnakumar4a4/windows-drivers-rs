// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! # Sample UMDF Safe Driver
//!
//! A sample UMDF driver written in 100% safe Rust. Demonstrates request
//! processing and cancellation, split across modules, along with WPP tracing
//! that is also recorded into the WPP in-flight recorder (IFR).

#![feature(codeview_annotation)]
#![feature(core_os)]

use wdf::{
    driver_entry, println, status_codes, wpp_control_guids, DeviceInit, Driver, DriverConfig,
    DriverObject, NtResult, NtStatus, UnicodeString,
};

const MAX_WRITE_LENGTH: usize = 1024 * 40;

/// Driver lifecycle states, traced as a WPP enum.
#[allow(dead_code)]
#[repr(u8)]
#[derive(Clone, Copy, wpp::WppField)]
enum SampleState {
    Unknown = 0,
    Starting = 1,
    Ready = 2,
}

wpp_control_guids!(
    SampleUmdfDriver 67d93aeb-cbe5-49a0-a5f8-ab26f55ec81e {
        GENERAL,
        PNP,
        POWER,
        IO,
    }
);

// Modules declared after wpp_control_guids! so trace macros are in scope
mod device;
mod queue;

#[driver_entry(trace_providers(SampleUmdfDriver))]
fn driver_entry(driver_object: &mut DriverObject, registry_path: &UnicodeString) -> NtResult<()> {
    trace!(INFO, GENERAL, "UMDF safe driver entry, state={}", SampleState::Starting);

    let config = DriverConfig::new(evt_device_add);
    let driver = Driver::create(driver_object, registry_path, config)?;

    if cfg!(debug_assertions) {
        print_driver_version(driver)?;
    }

    trace!(
        INFO, GENERAL,
        "Safe Rust UMDF driver entry complete. Int: {}, Str: {}, state={}",
        42, "rust for drivers", SampleState::Ready
    );
    trace!(
        INFO, GENERAL,
        "NTSTATUS value: success={}",
        NtStatus::from(status_codes::STATUS_SUCCESS)
    );
    trace!(WARNING, IO, "IO path ready, max write: {}", MAX_WRITE_LENGTH);

    Ok(())
}

fn evt_device_add(device_init: &mut DeviceInit) -> NtResult<()> {
    println!("Enter evt_device_add");
    device::device_create(device_init)
}

fn print_driver_version(driver: &Driver) -> NtResult<()> {
    let driver_version = driver.retrieve_version_string()?;
    println!("Echo Sample {driver_version}");

    if driver.is_version_available(2, 0) {
        println!("Yes, framework version is 2.0");
    } else {
        println!("No, framework version is not 2.0");
    }

    Ok(())
}
