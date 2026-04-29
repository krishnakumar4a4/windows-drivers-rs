//! Deep-nested diagnostics module (2 levels: device::diagnostics).
//! Demonstrates that `trace!` works from any module depth.

use wdf::{println, trace, NtResult, NtStatus, HResult};

/// Runs a comprehensive set of trace statements covering all supported
/// C-style format specifiers. Called from `device::run_device_diagnostics`.
pub fn trace_all_format_specs() {
    println!("Running format spec diagnostics");

    // === No-argument trace ================================================
    trace!("diagnostics: no-arg trace from nested module");

    // === Signed integers (short-form) =====================================
    let val_i32: i32 = -100_000;
    trace!(FLAG_ONE, "diag: i32 decimal %d", val_i32);

    let val_i16: i16 = -256;
    trace!(FLAG_ONE, "diag: i16 short %hd", val_i16);

    let val_i64: i64 = -9_999_999_999;
    trace!(FLAG_ONE, "diag: i64 long-long %I64d", val_i64);

    trace!(FLAG_ONE, "diag: i32 long %ld", val_i32);
    trace!(FLAG_ONE, "diag: i32 %i", val_i32);
    trace!(FLAG_ONE, "diag: i16 %hi", val_i16);
    trace!(FLAG_ONE, "diag: i64 %lld", val_i64);
    trace!(FLAG_ONE, "diag: i32 %li", val_i32);

    // === Unsigned integers (short-form) ===================================
    let val_u32: u32 = 4_000_000_000;
    trace!(FLAG_ONE, "diag: u32 decimal %u", val_u32);

    let val_u16: u16 = 65535;
    trace!(FLAG_ONE, "diag: u16 short %hu", val_u16);

    let val_u64: u64 = 18_446_744_073_709_551_615;
    trace!(FLAG_ONE, "diag: u64 unsigned %I64u", val_u64);

    trace!(FLAG_ONE, "diag: u32 long unsigned %lu", val_u32);
    trace!(FLAG_ONE, "diag: u64 %llu", val_u64);

    // === Char / byte ======================================================
    let val_u8: u8 = 65; // 'A'
    trace!(FLAG_ONE, "diag: u8 char %c", val_u8);
    trace!(FLAG_ONE, "diag: u8 hc %hc", val_u8);

    // === Hex / octal (short-form) =========================================
    let hex_val: u32 = 0xDEAD_BEEF;
    trace!(FLAG_ONE, "diag: u32 hex lower %x", hex_val);
    trace!(FLAG_ONE, "diag: u32 hex upper %X", hex_val);
    trace!(FLAG_ONE, "diag: u32 octal %o", hex_val);

    trace!(FLAG_ONE, "diag: u16 hex %hx", val_u16);
    trace!(FLAG_ONE, "diag: u16 hex upper %hX", val_u16);
    trace!(FLAG_ONE, "diag: u16 octal %ho", val_u16);

    trace!(FLAG_ONE, "diag: u32 long hex %lx", hex_val);
    trace!(FLAG_ONE, "diag: u32 long hex upper %lX", hex_val);
    trace!(FLAG_ONE, "diag: u32 long octal %lo", hex_val);

    trace!(FLAG_ONE, "diag: i64 hex %I64x", val_i64);
    trace!(FLAG_ONE, "diag: i64 hex upper %I64X", val_i64);
    trace!(FLAG_ONE, "diag: i64 octal %I64o", val_i64);
    trace!(FLAG_ONE, "diag: i64 hex ll %llx", val_i64);
    trace!(FLAG_ONE, "diag: i64 hex upper ll %llX", val_i64);
    trace!(FLAG_ONE, "diag: i64 octal ll %llo", val_i64);

    // === Pointer-sized integers ===========================================
    let val_isize: isize = -42;
    let val_usize: usize = 0xFFFF_8000_0000_0000;
    trace!(FLAG_TWO, "diag: isize %Id", val_isize);
    trace!(FLAG_TWO, "diag: usize %Iu", val_usize);
    trace!(FLAG_TWO, "diag: usize hex %Ix", val_usize);
    trace!(FLAG_TWO, "diag: usize hex upper %IX", val_usize);
    trace!(FLAG_TWO, "diag: usize octal %Io", val_usize);
    trace!(FLAG_TWO, "diag: pointer %p", val_usize);

    // === Floating point ===================================================
    let temperature: f64 = 98.6;
    trace!(FLAG_TWO, "diag: f64 float %f", temperature);
    trace!(FLAG_TWO, "diag: f64 scientific %e", temperature);
    trace!(FLAG_TWO, "diag: f64 scientific upper %E", temperature);
    trace!(FLAG_TWO, "diag: f64 general %g", temperature);
    trace!(FLAG_TWO, "diag: f64 general upper %G", temperature);

    // === Strings ==========================================================
    let msg: &str = "hello from diagnostics";
    trace!(FLAG_TWO, "diag: string %s", msg);
    trace!(FLAG_TWO, "diag: string hs %hs", msg);

    // === Long-form integer types (%!NAME!) ================================
    let val_i8: i8 = -1;
    trace!(FLAG_ONE, "diag: SBYTE %!SBYTE!", val_i8);
    trace!(FLAG_ONE, "diag: UBYTE %!UBYTE!", val_u8);
    trace!(FLAG_ONE, "diag: SSHORT %!SSHORT!", val_i16);
    trace!(FLAG_ONE, "diag: USHORT %!USHORT!", val_u16);
    trace!(FLAG_ONE, "diag: SINT %!SINT!", val_i32);
    trace!(FLAG_ONE, "diag: UINT %!UINT!", val_u32);
    trace!(FLAG_ONE, "diag: SLONG %!SLONG!", val_i32);
    trace!(FLAG_ONE, "diag: ULONG %!ULONG!", val_u32);
    trace!(FLAG_ONE, "diag: SINT64 %!SINT64!", val_i64);
    trace!(FLAG_ONE, "diag: UINT64 %!UINT64!", val_u64);
    trace!(FLAG_ONE, "diag: DOUBLE %!DOUBLE!", temperature);

    // === Long-form hex display ============================================
    let xval: i32 = 0x0ABC;
    let xshort: i16 = 0x00FF;
    trace!(FLAG_ONE, "diag: XINT %!XINT!", xval);
    trace!(FLAG_ONE, "diag: OINT %!OINT!", xval);
    trace!(FLAG_ONE, "diag: XLONG %!XLONG!", xval);
    trace!(FLAG_ONE, "diag: OLONG %!OLONG!", xval);
    trace!(FLAG_ONE, "diag: XSHORT %!XSHORT!", xshort);
    trace!(FLAG_ONE, "diag: OSHORT %!OSHORT!", xshort);
    trace!(FLAG_ONE, "diag: XBYTE %!XBYTE!", val_i8);
    trace!(FLAG_ONE, "diag: OBYTE %!OBYTE!", val_i8);
    trace!(FLAG_ONE, "diag: XINT64 %!XINT64!", val_i64);
    trace!(FLAG_ONE, "diag: XXINT64 %!XXINT64!", val_i64);
    trace!(FLAG_ONE, "diag: OINT64 %!OINT64!", val_i64);

    // === Long-form pointer types ==========================================
    trace!(FLAG_TWO, "diag: PTR %!PTR!", val_usize);
    trace!(FLAG_TWO, "diag: HANDLE %!HANDLE!", val_usize);
    trace!(FLAG_TWO, "diag: SLONGPTR %!SLONGPTR!", val_isize);
    trace!(FLAG_TWO, "diag: ULONGPTR %!ULONGPTR!", val_usize);
    trace!(FLAG_TWO, "diag: XLONGPTR %!XLONGPTR!", val_isize);
    trace!(FLAG_TWO, "diag: OLONGPTR %!OLONGPTR!", val_isize);

    // === Special decoded types ============================================
    let status = NtStatus::from(0);
    let hr = HResult::from(0);
    trace!(FLAG_ONE, "diag: STATUS %!STATUS!", status);
    trace!(FLAG_ONE, "diag: status (lower) %!status!", status);
    trace!(FLAG_ONE, "diag: HRESULT %!HRESULT!", hr);
    trace!(FLAG_ONE, "diag: hresult (lower) %!hresult!", hr);

    let winerr: u32 = 5;
    trace!(FLAG_ONE, "diag: WINERROR %!WINERROR!", winerr);
    trace!(FLAG_ONE, "diag: winerr %!winerr!", winerr);

    let ndis: i32 = 0;
    trace!(FLAG_ONE, "diag: NDIS_STATUS %!NDIS_STATUS!", ndis);

    // === Boolean types ====================================================
    let flag_on: bool = true;
    let flag_off: bool = false;
    trace!(FLAG_TWO, "diag: bool true %!bool!", flag_on);
    trace!(FLAG_TWO, "diag: bool false %!bool!", flag_off);
    trace!(FLAG_TWO, "diag: BOOLEAN %!BOOLEAN!", flag_on);

    // === Network types ====================================================
    let ip: u32 = 0x0A000001; // 10.0.0.1
    let port: u16 = 8080;
    trace!(FLAG_ONE, "diag: IPADDR %!IPADDR!", ip);
    trace!(FLAG_ONE, "diag: ipaddr (lower) %!ipaddr!", ip);
    trace!(FLAG_ONE, "diag: PORT %!PORT!", port);
    trace!(FLAG_ONE, "diag: port (lower) %!port!", port);

    // === Time types =======================================================
    let ts: i64 = 133_500_000_000_000_000;
    trace!(FLAG_ONE, "diag: TIMESTAMP %!TIMESTAMP!", ts);
    trace!(FLAG_ONE, "diag: TIME %!TIME!", ts);
    trace!(FLAG_ONE, "diag: DATE %!DATE!", ts);
    trace!(FLAG_ONE, "diag: WAITTIME %!WAITTIME!", ts);

    // === String long-form =================================================
    let astr: &str = "ansi string test";
    trace!(FLAG_TWO, "diag: ASTR %!ASTR!", astr);

    // === Flag + Level combinations ========================================
    trace!(FLAG_ONE, Information, "diag: info level %d", val_i32);
    trace!(FLAG_TWO, Verbose, "diag: verbose level %!bool!", flag_on);
    trace!(Warning, "diag: warning (no flag) %d", val_i32);
    trace!(Error, "diag: error (no flag) %d", val_i32);
    trace!(Critical, "diag: critical (no flag) %d", val_i32);

    // === Mixed types in single trace ======================================
    trace!(FLAG_ONE, "diag: mixed i32=%d u64=%I64u bool=%!bool! str=%s",
        val_i32, val_u64, flag_on, msg);
    trace!(FLAG_TWO, Information, "diag: mixed status=%!STATUS! u32=%u i16=%hd ptr=%p",
        status, val_u32, val_i16, val_usize);
}

/// Traces a diagnostic summary using the second trace provider's flags.
pub fn trace_provider2_diagnostics() {
    trace!(FLAG_PERF, "diag2: performance check, latency = %d us", 150i32);
    trace!(FLAG_PERF, Information, "diag2: throughput = %I64u bytes/sec", 1_000_000_000u64);
    trace!(FLAG_IO, "diag2: I/O queue depth = %d", 32i32);
    trace!(FLAG_IO, Verbose, "diag2: I/O buffer size = %u bytes", 4096u32);
    trace!(FLAG_STATE, "diag2: state = %d (ready)", 1i32);
    trace!(FLAG_STATE, Warning, "diag2: state transition warning, code = %!STATUS!", NtStatus::from(0));
}
