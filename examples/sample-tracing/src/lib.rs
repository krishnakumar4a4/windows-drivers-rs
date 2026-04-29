#![no_std]
#![feature(codeview_annotation)]
#![feature(core_intrinsics)]
#![allow(incomplete_features)]

mod device;

use wdf::{
    driver_entry, println, trace, Driver, HResult, NtResult, NtStatus,
};

use core::fmt;

/// Example custom struct with a manual `Display` implementation.
/// The `trace!` macro serializes it via `%!DISPLAY!` using `TraceFmtBuf`.
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

/// The entry point for the driver.
///
/// Demonstrates the `trace!` macro with every category of C-style format
/// specifier supported by defaultwpp.ini. Uses two trace providers:
///   - SampleTracing: general tracing (FLAG_ONE, FLAG_TWO)
///   - SampleTracingDiag: diagnostics (FLAG_PERF, FLAG_IO, FLAG_STATE)
///
/// The `device` module (depth 1) and `device::diagnostics` (depth 2)
/// contain additional trace statements exercising the full format-spec set.
#[driver_entry(trace_control = ("SampleTracing", "cb94defb-592a-4509-8f2e-54f204929669", [FLAG_ONE, FLAG_TWO]), ("SampleTracingDiag", "d3e4f5a6-b7c8-9012-3456-789abcdef012", [FLAG_PERF, FLAG_IO, FLAG_STATE]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    println!("Sample tracing driver entry");

    // === No-argument trace ================================================
    trace!("Hello from sample-tracing driver!");

    // === Signed integers (short-form) =====================================
    let val_i32: i32 = -100_000;
    trace!(FLAG_ONE, "i32 decimal: %d", val_i32);

    let val_i16: i16 = -256;
    trace!(FLAG_ONE, "i16 short: %hd", val_i16);

    let val_i64: i64 = -9_999_999_999;
    trace!(FLAG_ONE, "i64 long-long: %I64d", val_i64);

    trace!(FLAG_ONE, "i32 long: %ld", val_i32);

    // === Unsigned integers (short-form) ===================================
    let val_u32: u32 = 4_000_000_000;
    trace!(FLAG_ONE, "u32 decimal: %u", val_u32);

    let val_u16: u16 = 65535;
    trace!(FLAG_ONE, "u16 short: %hu", val_u16);

    let val_u64: u64 = 18_446_744_073_709_551_615;
    trace!(FLAG_ONE, "u64 unsigned: %I64u", val_u64);

    trace!(FLAG_ONE, "u32 long unsigned: %lu", val_u32);

    // === Char / byte (short-form) =========================================
    let val_u8: u8 = 255;
    trace!(FLAG_ONE, "u8 char: %c", val_u8);

    // === Hex / octal display (short-form) =================================
    let hex_val: u32 = 0xDEAD_BEEF;
    trace!(FLAG_ONE, "u32 hex lower: %x", hex_val);
    trace!(FLAG_ONE, "u32 hex upper: %X", hex_val);

    let oct_val: u32 = 0o777;
    trace!(FLAG_ONE, "u32 octal: %o", oct_val);
    trace!(FLAG_ONE, "u16 hex: %hx", val_u16);
    trace!(FLAG_ONE, "i64 hex: %I64x", val_i64);

    // === Pointer-sized integers (short-form) ==============================
    let val_isize: isize = -42;
    trace!(FLAG_ONE, "isize: %Id", val_isize);

    let val_usize: usize = 42;
    trace!(FLAG_ONE, "usize: %Iu", val_usize);
    trace!(FLAG_ONE, "usize hex: %Ix", val_usize);

    let ptr_val: usize = 0xFFFF_8000_0000_0000;
    trace!(FLAG_ONE, "pointer: %p", ptr_val);

    // === Floating point (short-form) ======================================
    let temperature: f64 = 98.6;
    trace!(FLAG_ONE, "f64 float: %f", temperature);
    trace!(FLAG_ONE, "f64 scientific: %e", temperature);
    trace!(FLAG_ONE, "f64 general: %g", temperature);

    // === String (short-form) ==============================================
    let msg: &str = "hello world";
    trace!(FLAG_ONE, "string: %s", msg);

    // === Long-form integer types (%!NAME!) ================================
    let val_i8: i8 = -1;
    trace!(FLAG_ONE, "SBYTE: %!SBYTE!", val_i8);
    trace!(FLAG_ONE, "UBYTE: %!UBYTE!", val_u8);
    trace!(FLAG_ONE, "SSHORT: %!SSHORT!", val_i16);
    trace!(FLAG_ONE, "USHORT: %!USHORT!", val_u16);
    trace!(FLAG_ONE, "SINT: %!SINT!", val_i32);
    trace!(FLAG_ONE, "UINT: %!UINT!", val_u32);
    trace!(FLAG_ONE, "SLONG: %!SLONG!", val_i32);
    trace!(FLAG_ONE, "ULONG: %!ULONG!", val_u32);
    trace!(FLAG_ONE, "SINT64: %!SINT64!", val_i64);
    trace!(FLAG_ONE, "UINT64: %!UINT64!", val_u64);
    trace!(FLAG_ONE, "DOUBLE: %!DOUBLE!", temperature);

    // === Long-form hex display variants ===================================
    let xval: i32 = 0x0ABC;
    trace!(FLAG_ONE, "XINT: %!XINT!", xval);

    let xshort: i16 = 0x00FF;
    trace!(FLAG_ONE, "XSHORT: %!XSHORT!", xshort);
    trace!(FLAG_ONE, "XBYTE: %!XBYTE!", val_i8);
    trace!(FLAG_ONE, "XINT64: %!XINT64!", val_i64);

    // === Long-form pointer types ==========================================
    trace!(FLAG_ONE, "PTR: %!PTR!", ptr_val);

    let handle: usize = 0x1234;
    trace!(FLAG_ONE, "HANDLE: %!HANDLE!", handle);
    trace!(FLAG_ONE, "SLONGPTR: %!SLONGPTR!", val_isize);
    trace!(FLAG_ONE, "ULONGPTR: %!ULONGPTR!", val_usize);

    // === Special decoded types ============================================
    let status = NtStatus::from(0);
    trace!(FLAG_ONE, "STATUS: %!STATUS!", status);

    let hr = HResult::from(0);
    trace!(FLAG_ONE, "HRESULT: %!HRESULT!", hr);

    let winerr: u32 = 5;
    trace!(FLAG_ONE, "WINERROR: %!WINERROR!", winerr);

    let ndis: i32 = 0;
    trace!(FLAG_ONE, "NDIS_STATUS: %!NDIS_STATUS!", ndis);

    // === Boolean types ====================================================
    let flag_on: bool = true;
    trace!(FLAG_TWO, "bool: %!bool!", flag_on);

    let flag_off: bool = false;
    trace!(FLAG_TWO, "BOOLEAN: %!BOOLEAN!", flag_off);

    // === Network types ====================================================
    let ip: u32 = 0x0A000001;
    trace!(FLAG_ONE, "IPADDR: %!IPADDR!", ip);

    let port: u16 = 8080;
    trace!(FLAG_ONE, "PORT: %!PORT!", port);

    // === Time types =======================================================
    let ts: i64 = 133_500_000_000_000_000;
    trace!(FLAG_ONE, "TIMESTAMP: %!TIMESTAMP!", ts);

    // === String long-form =================================================
    let astr_val: &str = "ansi string";
    trace!(FLAG_ONE, "ASTR: %!ASTR!", astr_val);

    // === Flag + Level combinations ========================================
    trace!(FLAG_ONE, Information, "info level: %!STATUS!", status);
    trace!(FLAG_TWO, Verbose, "verbose bool: %!bool!", flag_on);
    trace!(Warning, "warning (no flag): %d", val_i32);

    // === Mixed types in single trace ======================================
    trace!(FLAG_ONE, "mixed: i32=%d, u64=%I64u, bool=%!bool!, str=%s",
        val_i32, val_u64, flag_on, msg);

    // === Custom Display type (%!DISPLAY!) =================================
    let dev = DeviceInfo { vendor_id: 0x8086, device_id: 0x1234, revision: 3 };
    trace!(FLAG_ONE, "device: %!DISPLAY!", dev);
    trace!(FLAG_ONE, "status=%!STATUS!, device=%!DISPLAY!", status, dev);

    // === Second trace provider (SampleTracingDiag) ========================
    trace!(FLAG_PERF, "perf: init latency = %d us", 250i32);
    trace!(FLAG_IO, Information, "io: buffer pool size = %u", 16384u32);
    trace!(FLAG_STATE, Verbose, "state: driver ready, version = %d", 1i32);

    driver.set_evt_device_add(device::evt_device_add);
    Ok(())
}
