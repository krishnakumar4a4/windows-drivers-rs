//! A sample driver written in 100% safe Rust.
//! Demonstrates request processing and cancellation, split across modules.

#![no_std]
#![feature(codeview_annotation)]

use wdf::{
    driver_entry, println, wpp_control_guids, DeviceInit, Driver, DriverConfig, DriverObject,
    NtResult, UnicodeString,
};

extern crate alloc;

const MAX_WRITE_LENGTH: usize = 1024 * 40;

wpp_control_guids!(
    SampleDriver cb94defb-592a-4509-8f2e-54f204929669 {
        GENERAL,
    }
    PnpTracer a1b2c3d4-e5f6-7890-abcd-ef1234567890 {
        PNP,
        POWER,
        IO,
    }
);

// Modules declared after wpp_control_guids! so trace macros are in scope
mod device;
mod queue;

#[driver_entry(trace_providers(
    SampleDriver,
    PnpTracer,
    sample_kmdf_safe_driver_wpp_dependency::DependencyProvider
))]
fn driver_entry(driver_object: &mut DriverObject, registry_path: &UnicodeString) -> NtResult<()> {
    let config = DriverConfig::new(evt_device_add);
    let driver = Driver::create(driver_object, registry_path, config)?;

    if cfg!(debug_assertions) {
        print_driver_version(driver)?;
    }

    let msg = "rust for drivers";
    trace!(INFO, GENERAL, "Safe Rust driver entry complete. Int: {=i32}, Str: {=str}", 42, msg);

    // Signed integer width type specifiers.
    trace!(
        INFO, GENERAL,
        "Signed widths: i8={=i8}, i16={=i16}, i32={=i32}, i64={=i64}, isize={=isize}",
        -8_i8, -1600_i16, -320_000_i32, -64_000_000_000_i64, -42_isize
    );

    // Unsigned integer width type specifiers.
    trace!(
        INFO, GENERAL,
        "Unsigned widths: u8={=u8}, u16={=u16}, u32={=u32}, u64={=u64}, usize={=usize}",
        200_u8, 60_000_u16, 4_000_000_000_u32, 18_000_000_000_000_000_000_u64, MAX_WRITE_LENGTH
    );

    // Boolean and C-string type specifiers.
    trace!(
        INFO, GENERAL,
        "Flags: initialized={=bool}, safe_mode={=bool}, build={=cstr}",
        true, false, c"safe-rust"
    );

    trace!(VERBOSE, PNP, "PnP subsystem initialized");
    trace!(WARNING, IO, "IO path ready, max write: {=usize}", MAX_WRITE_LENGTH);
    sample_kmdf_safe_driver_wpp_dependency::emit_dependency_trace();

    // Default trace: no keyword, routes to first provider
    trace!(INFO, "Driver entry complete, no keyword");

    Ok(())
}

fn evt_device_add(device_init: &mut DeviceInit) -> NtResult<()> {
    println!("Enter evt_device_add");
    device::device_create(device_init)
}

fn print_driver_version(driver: &Driver) -> NtResult<()> {
    let driver_version = driver.retrieve_version_string()?;
    println!("Echo Sample {driver_version}");

    if driver.is_version_available(1, 0) {
        println!("Yes, framework version is 1.0");
    } else {
        println!("No, framework version is not 1.0");
    }

    Ok(())
}
