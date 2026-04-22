#![no_std]
#![feature(codeview_annotation)]
#![feature(core_intrinsics)]
#![allow(incomplete_features)]

// Nightly features for compile time reflection (kept for future exploration,
// no longer required by the trace! macro)
// #![feature(type_info)]
// #![feature(const_cmp)]
// #![feature(const_trait_impl)]

use wdf::{
    driver_entry, println, trace, Driver, DeviceInit, HResult, NtResult, NtStatus,
};

/// The entry point for the driver.
///
/// Demonstrates the `trace!` macro with every category of C-style format
/// specifier supported by defaultwpp.ini. Each trace statement uses a
/// variable argument (not a bare literal) to exercise compile-time type
/// assertion.
#[driver_entry(trace_control = ("cb94defb-592a-4509-8f2e-54f204929669", [FLAG_ONE, FLAG_TWO]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    println!("Sample tracing driver entry");

    // === No-argument trace ================================================
    trace!("Hello from sample-tracing driver!");

    // === Signed integers (short-form) =====================================
    // %d / %i  → i32  (ItemLong)
    let val_i32: i32 = -100_000;
    trace!(FLAG_ONE, "i32 decimal: %d", val_i32);

    // %hd / %hi → i16 (ItemShort)
    let val_i16: i16 = -256;
    trace!(FLAG_ONE, "i16 short: %hd", val_i16);

    // %I64d / %lld → i64 (ItemLongLong)
    let val_i64: i64 = -9_999_999_999;
    trace!(FLAG_ONE, "i64 long-long: %I64d", val_i64);

    // %ld / %li → i32  (ItemLong, same as %d on Windows)
    trace!(FLAG_ONE, "i32 long: %ld", val_i32);

    // === Unsigned integers (short-form) ===================================
    // %u → u32 (ItemLong)
    let val_u32: u32 = 4_000_000_000;
    trace!(FLAG_ONE, "u32 decimal: %u", val_u32);

    // %hu → u16 (ItemShort)
    let val_u16: u16 = 65535;
    trace!(FLAG_ONE, "u16 short: %hu", val_u16);

    // %I64u / %llu → u64 (ItemULongLong)
    let val_u64: u64 = 18_446_744_073_709_551_615;
    trace!(FLAG_ONE, "u64 unsigned: %I64u", val_u64);

    // %lu → u32 (ItemLong)
    trace!(FLAG_ONE, "u32 long unsigned: %lu", val_u32);

    // === Char / byte (short-form) =========================================
    // %c → u8 (ItemChar)
    let val_u8: u8 = 255;
    trace!(FLAG_ONE, "u8 char: %c", val_u8);

    // === Hex / octal display (short-form) =================================
    // %x, %X → u32 hex (ItemLong)
    let hex_val: u32 = 0xDEAD_BEEF;
    trace!(FLAG_ONE, "u32 hex lower: %x", hex_val);
    trace!(FLAG_ONE, "u32 hex upper: %X", hex_val);

    // %o → u32 octal
    let oct_val: u32 = 0o777;
    trace!(FLAG_ONE, "u32 octal: %o", oct_val);

    // %hx → u16 hex (ItemShort)
    trace!(FLAG_ONE, "u16 hex: %hx", val_u16);

    // %I64x → i64 hex (ItemLongLongX)
    trace!(FLAG_ONE, "i64 hex: %I64x", val_i64);

    // === Pointer-sized integers (short-form) ==============================
    // %Id → isize (ItemPtr, signed)
    let val_isize: isize = -42;
    trace!(FLAG_ONE, "isize: %Id", val_isize);

    // %Iu → usize (ItemPtr, unsigned)
    let val_usize: usize = 42;
    trace!(FLAG_ONE, "usize: %Iu", val_usize);

    // %Ix → usize hex
    trace!(FLAG_ONE, "usize hex: %Ix", val_usize);

    // %p → usize pointer
    let ptr_val: usize = 0xFFFF_8000_0000_0000;
    trace!(FLAG_ONE, "pointer: %p", ptr_val);

    // === Floating point (short-form) ======================================
    // %f → f64 (ItemDouble)
    let temperature: f64 = 98.6;
    trace!(FLAG_ONE, "f64 float: %f", temperature);

    // %e → f64 scientific
    trace!(FLAG_ONE, "f64 scientific: %e", temperature);

    // %g → f64 general
    trace!(FLAG_ONE, "f64 general: %g", temperature);

    // === String (short-form) ==============================================
    // %s → &str converted to CString (ItemString)
    let msg: &str = "hello world";
    trace!(FLAG_ONE, "string: %s", msg);

    // === Long-form integer types (%!NAME!) ================================
    // %!SBYTE! → i8 (ItemChar)
    let val_i8: i8 = -1;
    trace!(FLAG_ONE, "SBYTE: %!SBYTE!", val_i8);

    // %!UBYTE! → u8 (ItemChar)
    trace!(FLAG_ONE, "UBYTE: %!UBYTE!", val_u8);

    // %!SSHORT! → i16
    trace!(FLAG_ONE, "SSHORT: %!SSHORT!", val_i16);

    // %!USHORT! → u16
    trace!(FLAG_ONE, "USHORT: %!USHORT!", val_u16);

    // %!SINT! → i32
    trace!(FLAG_ONE, "SINT: %!SINT!", val_i32);

    // %!UINT! → u32
    trace!(FLAG_ONE, "UINT: %!UINT!", val_u32);

    // %!SLONG! → i32
    trace!(FLAG_ONE, "SLONG: %!SLONG!", val_i32);

    // %!ULONG! → u32
    trace!(FLAG_ONE, "ULONG: %!ULONG!", val_u32);

    // %!SINT64! → i64
    trace!(FLAG_ONE, "SINT64: %!SINT64!", val_i64);

    // %!UINT64! → u64
    trace!(FLAG_ONE, "UINT64: %!UINT64!", val_u64);

    // %!DOUBLE! → f64
    trace!(FLAG_ONE, "DOUBLE: %!DOUBLE!", temperature);

    // === Long-form hex display variants ===================================
    // %!XINT! → i32 (ItemLong, 08x format)
    let xval: i32 = 0x0ABC;
    trace!(FLAG_ONE, "XINT: %!XINT!", xval);

    // %!XSHORT! → i16 (ItemShort, 04hX format)
    let xshort: i16 = 0x00FF;
    trace!(FLAG_ONE, "XSHORT: %!XSHORT!", xshort);

    // %!XBYTE! → i8 (ItemChar, 02x format)
    trace!(FLAG_ONE, "XBYTE: %!XBYTE!", val_i8);

    // %!XINT64! → i64 (ItemLongLongX)
    trace!(FLAG_ONE, "XINT64: %!XINT64!", val_i64);

    // === Long-form pointer types ==========================================
    // %!PTR! → usize
    trace!(FLAG_ONE, "PTR: %!PTR!", ptr_val);

    // %!HANDLE! → usize
    let handle: usize = 0x1234;
    trace!(FLAG_ONE, "HANDLE: %!HANDLE!", handle);

    // %!SLONGPTR! → isize
    trace!(FLAG_ONE, "SLONGPTR: %!SLONGPTR!", val_isize);

    // %!ULONGPTR! → usize
    trace!(FLAG_ONE, "ULONGPTR: %!ULONGPTR!", val_usize);

    // === Special decoded types ============================================
    // %!STATUS! → NtStatus (ItemNTSTATUS)
    let status = NtStatus::from(0);
    trace!(FLAG_ONE, "STATUS: %!STATUS!", status);

    // %!HRESULT! → HResult (ItemHRESULT)
    let hr = HResult::from(0);
    trace!(FLAG_ONE, "HRESULT: %!HRESULT!", hr);

    // %!WINERROR! → u32 (ItemWINERROR)
    let winerr: u32 = 5; // ERROR_ACCESS_DENIED
    trace!(FLAG_ONE, "WINERROR: %!WINERROR!", winerr);

    // %!NDIS_STATUS! → i32 (ItemNDIS_STATUS)
    let ndis: i32 = 0;
    trace!(FLAG_ONE, "NDIS_STATUS: %!NDIS_STATUS!", ndis);

    // === Boolean types ====================================================
    // %!bool! → bool (ItemListLong)
    let flag_on: bool = true;
    trace!(FLAG_TWO, "bool: %!bool!", flag_on);

    // %!BOOLEAN! → bool (ItemListByte)
    let flag_off: bool = false;
    trace!(FLAG_TWO, "BOOLEAN: %!BOOLEAN!", flag_off);

    // === Network types ====================================================
    // %!IPADDR! → u32 (ItemIPAddr)
    let ip: u32 = 0x0A000001; // 10.0.0.1
    trace!(FLAG_ONE, "IPADDR: %!IPADDR!", ip);

    // %!PORT! → u16 (ItemPort)
    let port: u16 = 8080;
    trace!(FLAG_ONE, "PORT: %!PORT!", port);

    // === Time types =======================================================
    // %!TIMESTAMP! → i64 (ItemTimestamp)
    let ts: i64 = 133_500_000_000_000_000;
    trace!(FLAG_ONE, "TIMESTAMP: %!TIMESTAMP!", ts);

    // === String long-form =================================================
    // %!ASTR! → &str (ItemString, same as %s)
    let astr_val: &str = "ansi string";
    trace!(FLAG_ONE, "ASTR: %!ASTR!", astr_val);

    // === Flag + Level combinations ========================================
    trace!(FLAG_ONE, Information, "info level: %!STATUS!", status);
    trace!(FLAG_TWO, Verbose, "verbose bool: %!bool!", flag_on);
    trace!(Warning, "warning (no flag): %d", val_i32);

    // === Mixed types in single trace ======================================
    trace!(FLAG_ONE, "mixed: i32=%d, u64=%I64u, bool=%!bool!, str=%s",
        val_i32, val_u64, flag_on, msg);

    driver.set_evt_device_add(evt_device_add);
    Ok(())
}

fn evt_device_add(_device_init: &mut DeviceInit) -> NtResult<()> {
    println!("evt_device_add called");
    Ok(())
}
