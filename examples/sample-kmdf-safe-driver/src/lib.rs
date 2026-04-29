//! A sample driver written in safe Rust.
//! Demonstrates request processing, cancellation, and multi-module tracing.
//!
//! Module structure (2 levels of nesting):
//!   - `lib.rs` — driver entry, format-spec coverage, version check
//!   - `device/mod.rs` — device creation, PnP callbacks
//!   - `device/queue.rs` — queue init, I/O read/write, timer, cancellation

#![no_std]
#![feature(codeview_annotation)]
#![feature(core_intrinsics)]

mod device;

use wdf::{
    driver_entry,
    println,
    trace,
    Driver,
    HResult,
    NtResult,
    NtStatus,
};

extern crate alloc;

/// Two trace providers:
///   - SampleKmdfSafe  (FLAG_ONE, FLAG_TWO): general driver lifecycle
///   - SampleKmdfDiag  (FLAG_PERF, FLAG_IO, FLAG_STATE): diagnostics
#[driver_entry(trace_control = ("SampleKmdfSafe", "cb94defb-592a-4509-8f2e-54f204929669", [FLAG_ONE, FLAG_TWO]), ("SampleKmdfDiag", "a1b2c3d4-e5f6-7890-abcd-ef1234567890", [FLAG_PERF, FLAG_IO, FLAG_STATE]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    if cfg!(debug_assertions) {
        print_driver_version(driver)?;
    }

    driver.set_evt_device_add(device::evt_device_add);

    // =====================================================================
    // Comprehensive format-spec coverage (easy to observe on driver load)
    // =====================================================================

    // --- No-argument trace -----------------------------------------------
    trace!("sample-kmdf-safe: driver entry started");

    // --- Signed integers (short-form) ------------------------------------
    let val_i32: i32 = -100_000;
    trace!(FLAG_ONE, "i32 %d", val_i32);
    trace!(FLAG_ONE, "i32 %i", val_i32);

    let val_i16: i16 = -256;
    trace!(FLAG_ONE, "i16 %hd", val_i16);
    trace!(FLAG_ONE, "i16 %hi", val_i16);

    let val_i64: i64 = -9_999_999_999;
    trace!(FLAG_ONE, "i64 %I64d", val_i64);
    trace!(FLAG_ONE, "i64 %lld", val_i64);

    trace!(FLAG_ONE, "i32 long %ld", val_i32);
    trace!(FLAG_ONE, "i32 long %li", val_i32);

    // --- Unsigned integers (short-form) ----------------------------------
    let val_u32: u32 = 4_000_000_000;
    trace!(FLAG_ONE, "u32 %u", val_u32);

    let val_u16: u16 = 65535;
    trace!(FLAG_ONE, "u16 %hu", val_u16);

    let val_u64: u64 = 18_446_744_073_709_551_615;
    trace!(FLAG_ONE, "u64 %I64u", val_u64);
    trace!(FLAG_ONE, "u64 %llu", val_u64);

    trace!(FLAG_ONE, "u32 %lu", val_u32);

    // --- Char / byte -----------------------------------------------------
    let val_u8: u8 = 65;
    trace!(FLAG_ONE, "u8 char %c", val_u8);
    trace!(FLAG_ONE, "u8 hc %hc", val_u8);

    // --- Hex / octal (short-form) ----------------------------------------
    let hex_val: u32 = 0xDEAD_BEEF;
    trace!(FLAG_ONE, "u32 hex %x", hex_val);
    trace!(FLAG_ONE, "u32 HEX %X", hex_val);
    trace!(FLAG_ONE, "u32 oct %o", hex_val);
    trace!(FLAG_ONE, "u16 hex %hx", val_u16);
    trace!(FLAG_ONE, "u16 HEX %hX", val_u16);
    trace!(FLAG_ONE, "u16 oct %ho", val_u16);
    trace!(FLAG_ONE, "u32 lx %lx", hex_val);
    trace!(FLAG_ONE, "u32 lX %lX", hex_val);
    trace!(FLAG_ONE, "u32 lo %lo", hex_val);
    trace!(FLAG_ONE, "i64 I64x %I64x", val_i64);
    trace!(FLAG_ONE, "i64 I64X %I64X", val_i64);
    trace!(FLAG_ONE, "i64 I64o %I64o", val_i64);
    trace!(FLAG_ONE, "i64 llx %llx", val_i64);
    trace!(FLAG_ONE, "i64 llX %llX", val_i64);
    trace!(FLAG_ONE, "i64 llo %llo", val_i64);

    // --- Pointer-sized integers ------------------------------------------
    let val_isize: isize = -42;
    let val_usize: usize = 0xFFFF_8000_0000_0000;
    trace!(FLAG_TWO, "isize %Id", val_isize);
    trace!(FLAG_TWO, "usize %Iu", val_usize);
    trace!(FLAG_TWO, "usize hex %Ix", val_usize);
    trace!(FLAG_TWO, "usize HEX %IX", val_usize);
    trace!(FLAG_TWO, "usize oct %Io", val_usize);
    trace!(FLAG_TWO, "pointer %p", val_usize);

    // --- Floating point --------------------------------------------------
    let temperature: f64 = 98.6;
    trace!(FLAG_TWO, "f64 %f", temperature);
    trace!(FLAG_TWO, "f64 %e", temperature);
    trace!(FLAG_TWO, "f64 %E", temperature);
    trace!(FLAG_TWO, "f64 %g", temperature);
    trace!(FLAG_TWO, "f64 %G", temperature);

    // --- Strings ---------------------------------------------------------
    let msg: &str = "hello from driver_entry";
    trace!(FLAG_TWO, "string %s", msg);
    trace!(FLAG_TWO, "string %hs", msg);

    // --- Long-form integer types (%!NAME!) -------------------------------
    let val_i8: i8 = -1;
    trace!(FLAG_ONE, "SBYTE %!SBYTE!", val_i8);
    trace!(FLAG_ONE, "UBYTE %!UBYTE!", val_u8);
    trace!(FLAG_ONE, "SSHORT %!SSHORT!", val_i16);
    trace!(FLAG_ONE, "USHORT %!USHORT!", val_u16);
    trace!(FLAG_ONE, "SINT %!SINT!", val_i32);
    trace!(FLAG_ONE, "UINT %!UINT!", val_u32);
    trace!(FLAG_ONE, "SLONG %!SLONG!", val_i32);
    trace!(FLAG_ONE, "ULONG %!ULONG!", val_u32);
    trace!(FLAG_ONE, "SINT64 %!SINT64!", val_i64);
    trace!(FLAG_ONE, "UINT64 %!UINT64!", val_u64);
    trace!(FLAG_ONE, "DOUBLE %!DOUBLE!", temperature);

    // --- Long-form hex display -------------------------------------------
    let xval: i32 = 0x0ABC;
    let xshort: i16 = 0x00FF;
    trace!(FLAG_ONE, "XINT %!XINT!", xval);
    trace!(FLAG_ONE, "OINT %!OINT!", xval);
    trace!(FLAG_ONE, "XLONG %!XLONG!", xval);
    trace!(FLAG_ONE, "OLONG %!OLONG!", xval);
    trace!(FLAG_ONE, "XSHORT %!XSHORT!", xshort);
    trace!(FLAG_ONE, "OSHORT %!OSHORT!", xshort);
    trace!(FLAG_ONE, "XBYTE %!XBYTE!", val_i8);
    trace!(FLAG_ONE, "OBYTE %!OBYTE!", val_i8);
    trace!(FLAG_ONE, "XINT64 %!XINT64!", val_i64);
    trace!(FLAG_ONE, "XXINT64 %!XXINT64!", val_i64);
    trace!(FLAG_ONE, "OINT64 %!OINT64!", val_i64);

    // --- Long-form pointer types -----------------------------------------
    trace!(FLAG_TWO, "PTR %!PTR!", val_usize);
    trace!(FLAG_TWO, "HANDLE %!HANDLE!", val_usize);
    trace!(FLAG_TWO, "SLONGPTR %!SLONGPTR!", val_isize);
    trace!(FLAG_TWO, "ULONGPTR %!ULONGPTR!", val_usize);
    trace!(FLAG_TWO, "XLONGPTR %!XLONGPTR!", val_isize);
    trace!(FLAG_TWO, "OLONGPTR %!OLONGPTR!", val_isize);

    // --- Special decoded types -------------------------------------------
    let status = NtStatus::from(0);
    let hr = HResult::from(0);
    trace!(FLAG_ONE, "STATUS %!STATUS!", status);
    trace!(FLAG_ONE, "status %!status!", status);
    trace!(FLAG_ONE, "HRESULT %!HRESULT!", hr);
    trace!(FLAG_ONE, "hresult %!hresult!", hr);

    let winerr: u32 = 5;
    trace!(FLAG_ONE, "WINERROR %!WINERROR!", winerr);
    trace!(FLAG_ONE, "winerr %!winerr!", winerr);

    let ndis: i32 = 0;
    trace!(FLAG_ONE, "NDIS_STATUS %!NDIS_STATUS!", ndis);

    // --- Boolean types ---------------------------------------------------
    let flag_on: bool = true;
    let flag_off: bool = false;
    trace!(FLAG_TWO, "bool true %!bool!", flag_on);
    trace!(FLAG_TWO, "bool false %!bool!", flag_off);
    trace!(FLAG_TWO, "BOOLEAN %!BOOLEAN!", flag_on);

    // --- Network types ---------------------------------------------------
    let ip: u32 = 0x0A000001;
    let port: u16 = 8080;
    trace!(FLAG_ONE, "IPADDR %!IPADDR!", ip);
    trace!(FLAG_ONE, "ipaddr %!ipaddr!", ip);
    trace!(FLAG_ONE, "PORT %!PORT!", port);
    trace!(FLAG_ONE, "port %!port!", port);

    // --- Time types ------------------------------------------------------
    let ts: i64 = 133_500_000_000_000_000;
    trace!(FLAG_ONE, "TIMESTAMP %!TIMESTAMP!", ts);
    trace!(FLAG_ONE, "TIME %!TIME!", ts);
    trace!(FLAG_ONE, "DATE %!DATE!", ts);
    trace!(FLAG_ONE, "WAITTIME %!WAITTIME!", ts);

    // --- String long-form ------------------------------------------------
    let astr: &str = "ansi string test";
    trace!(FLAG_TWO, "ASTR %!ASTR!", astr);

    // --- Flag + Level combinations ---------------------------------------
    trace!(FLAG_ONE, Information, "info level %d", val_i32);
    trace!(FLAG_TWO, Verbose, "verbose level %!bool!", flag_on);
    trace!(Warning, "warning (no flag) %d", val_i32);
    trace!(Error, "error (no flag) %d", val_i32);
    trace!(Critical, "critical (no flag) %d", val_i32);

    // --- Mixed types in single trace -------------------------------------
    trace!(FLAG_ONE, "mixed: i32=%d u64=%I64u bool=%!bool! str=%s",
        val_i32, val_u64, flag_on, msg);
    trace!(FLAG_TWO, Information, "mixed: status=%!STATUS! u32=%u i16=%hd ptr=%p",
        status, val_u32, val_i16, val_usize);

    // --- Second trace provider (SampleKmdfDiag) --------------------------
    trace!(FLAG_PERF, "perf: init latency = %d us", 200i32);
    trace!(FLAG_PERF, Information, "perf: throughput = %I64u bytes/sec", 500_000_000u64);
    trace!(FLAG_IO, "io: max write length = %Iu bytes", MAX_WRITE_LENGTH);
    trace!(FLAG_IO, Verbose, "io: queue config = sequential, default = %!bool!", flag_on);
    trace!(FLAG_STATE, "state: driver initialized, code = %d", 0i32);
    trace!(FLAG_STATE, Warning, "state: debug build = %!BOOLEAN!", flag_on);

    trace!("sample-kmdf-safe: driver entry complete");
    Ok(())
}

const MAX_WRITE_LENGTH: usize = 1024 * 40;

fn print_driver_version(driver: &Driver) -> NtResult<()> {
    let driver_version = driver.retrieve_version_string()?;
    println!("Echo Sample {driver_version}");

    trace!(FLAG_ONE, "version: %s", "1.0");

    if driver.is_version_available(1, 0) {
        println!("Yes, framework version is 1.0");
    } else {
        println!("No, framework version is not 1.0");
    }

    Ok(())
}
