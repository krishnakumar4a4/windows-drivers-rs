//! A sample driver written in 100% safe Rust.
//! Demonstrates request processing and cancellation, split across modules.

#![no_std]
#![feature(codeview_annotation)]

use wdf::{
    driver_entry, println, wpp_control_guids, DeviceInit, Driver, DriverConfig, DriverObject,
    HResult, NtResult, NtStatus, UnicodeString,
};

use core::fmt;
extern crate alloc;
use alloc::{boxed::Box, rc::Rc, sync::Arc, vec};

const MAX_WRITE_LENGTH: usize = 1024 * 40;

/// Example custom struct with a `Display` implementation.
/// The `trace!` macro automatically serializes it via `TraceFmtBuf`.
struct DeviceInfo {
    vendor_id: u16,
    device_id: u16,
    revision: u8,
}

impl fmt::Display for DeviceInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{vendor_id=0x{:04X}, device_id=0x{:04X}, revision={}}}",
            self.vendor_id, self.device_id, self.revision
        )
    }
}

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

/// Example struct with only `Debug` (no `Display`).
/// The `trace!` macro formats it via `{:?}` using `debug_to_trace_buf`.
#[derive(Debug)]
struct DriverState {
    initialized: bool,
    irql: u8,
}

// Modules declared after wpp_control_guids! so trace macros are in scope
mod device;
mod queue;

#[driver_entry(trace_providers(SampleDriver, PnpTracer))]
fn driver_entry(driver_object: &mut DriverObject, registry_path: &UnicodeString) -> NtResult<()> {
    let config = DriverConfig::new(evt_device_add);
    let driver = Driver::create(driver_object, registry_path, config)?;

    if cfg!(debug_assertions) {
        print_driver_version(driver)?;
    }

    let msg = "rust for drivers";
    trace!(INFO, GENERAL, "Safe Rust driver entry complete. Int: {}, Str: {}", 42, msg);

    let nt_status = NtStatus::from(0); // STATUS_SUCCESS
    trace!(INFO, GENERAL, "NtStatus check: {}", nt_status);

    let hr = HResult::from(0); // S_OK
    trace!(INFO, GENERAL, "HResult check: {}", hr);

    let dev = DeviceInfo { vendor_id: 0x8086, device_id: 0x1234, revision: 3 };
    trace!(INFO, GENERAL, "Device info: {}", dev);

    let state = DriverState { initialized: true, irql: 2 };
    trace!(INFO, GENERAL, "Driver state: {:?}", state);

    // --- Standard library container types via {:?} Debug ---
    let items = vec![1, 2, 3];
    trace!(INFO, GENERAL, "Vec: {:?}", items);

    let boxed = Box::new(42);
    trace!(INFO, GENERAL, "Box: {:?}", boxed);

    let arc_val = Arc::new(99);
    trace!(INFO, GENERAL, "Arc: {:?}", arc_val);

    let rc_val = Rc::new(77);
    trace!(INFO, GENERAL, "Rc: {:?}", rc_val);

    // Note: Mutex and RwLock are std-only, not available in no_std kernel drivers.
    // Use wdf::SpinLock for kernel synchronization instead.

    // --- Option: both variants ---
    let some_val: Option<i32> = Some(123);
    let none_val: Option<i32> = None;
    trace!(INFO, GENERAL, "Option Some: {:?}", some_val);
    trace!(INFO, GENERAL, "Option None: {:?}", none_val);

    // --- Result: both variants ---
    let ok_val: Result<i32, &str> = Ok(200);
    let err_val: Result<i32, &str> = Err("something failed");
    trace!(INFO, GENERAL, "Result Ok: {:?}", ok_val);
    trace!(INFO, GENERAL, "Result Err: {:?}", err_val);

    trace!(VERBOSE, PNP, "PnP subsystem initialized");
    trace!(WARNING, IO, "IO path ready, max write: {}", MAX_WRITE_LENGTH);

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
