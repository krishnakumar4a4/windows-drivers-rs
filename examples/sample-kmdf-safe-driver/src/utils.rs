//! Utility functions for the sample driver.

use wdf::{println, trace, Driver, NtResult};

/// This routine shows how to retrieve framework version string and
/// also how to find out to which version of framework library the
/// client driver is bound to.
pub fn print_driver_version(driver: &Driver) -> NtResult<()> {
    let driver_version = driver.retrieve_version_string()?;
    println!("Echo Sample {driver_version}");

    trace!(Information, "Trace: driver version retrieved - {}", 1);

    if driver.is_version_available(1, 0) {
        println!("Yes, framework version is 1.0");
    } else {
        println!("No, framework version is not 1.0");
    }

    Ok(())
}
