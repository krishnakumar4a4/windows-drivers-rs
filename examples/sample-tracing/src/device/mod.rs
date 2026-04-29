//! Device-level module for sample-tracing.
//! Demonstrates `trace!` at module depth 1 (device) and imports from
//! a nested submodule (device::diagnostics) at depth 2.

pub mod diagnostics;

use wdf::{println, trace, Device, DeviceInit, NtResult};

/// Called by the framework in response to AddDevice from the PNP manager.
pub fn evt_device_add(_device_init: &mut DeviceInit) -> NtResult<()> {
    println!("evt_device_add called");

    // Trace from module depth 1
    trace!(FLAG_ONE, Information, "device: evt_device_add entered");

    // Run the exhaustive format-spec diagnostics from depth 2
    diagnostics::trace_all_format_specs();

    // Run second-provider diagnostics from depth 2
    diagnostics::trace_provider2_diagnostics();

    trace!(FLAG_TWO, "device: evt_device_add complete, status = %d", 0i32);

    Ok(())
}
