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
/// Demonstrates the `trace!` macro with C-style format specifiers
/// as supported by the WPP tracewpp tool (defaultwpp.ini).
/// Both short-form (`%d`, `%I64u`) and long-form (`%!STATUS!`, `%!HRESULT!`)
/// format specifiers are supported.
#[driver_entry(trace_control = ("cb94defb-592a-4509-8f2e-54f204929669", [FLAG_ONE, FLAG_TWO]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    println!("Sample tracing driver entry");

    // -----------------------------------------------------------------------
    // Literal-only trace (no arguments)
    // -----------------------------------------------------------------------
    trace!("Hello from sample-tracing driver!");

    // -----------------------------------------------------------------------
    // Integer literals with C-style format specifiers
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, "Literal i32: %d", 42i32);
    trace!(FLAG_ONE, "Literal u64: %I64u", 12345u64);

    // -----------------------------------------------------------------------
    // All integer primitive types with appropriate C format specifiers
    // -----------------------------------------------------------------------
    let val_i8: i8 = -1;
    let val_i16: i16 = -256;
    let val_i32: i32 = -100000;
    let val_i64: i64 = -9_999_999_999;
    let val_u8: u8 = 255;
    let val_u16: u16 = 65535;
    let val_u32: u32 = 4_000_000_000;
    let val_u64: u64 = 18_446_744_073_709_551_615;
    let val_isize: isize = -42;
    let val_usize: usize = 42;

    trace!(FLAG_ONE, "i8=%!SBYTE!, i16=%hd", val_i8, val_i16);
    trace!(FLAG_ONE, "i32=%d, i64=%I64d", val_i32, val_i64);
    trace!(FLAG_ONE, "u8=%c, u16=%hu", val_u8, val_u16);
    trace!(FLAG_ONE, "u32=%u, u64=%I64u", val_u32, val_u64);
    trace!(FLAG_ONE, "isize=%Id, usize=%Iu", val_isize, val_usize);

    // -----------------------------------------------------------------------
    // Boolean — uses long-form %!bool!
    // -----------------------------------------------------------------------
    let verbose_mode: bool = true;
    trace!(FLAG_TWO, "verbose=%!bool!", verbose_mode);

    // -----------------------------------------------------------------------
    // NTSTATUS — long-form %!STATUS!
    // -----------------------------------------------------------------------
    let status = NtStatus::from(0); // STATUS_SUCCESS
    trace!(FLAG_ONE, "NTSTATUS: %!STATUS!", status);

    // -----------------------------------------------------------------------
    // HRESULT — long-form %!HRESULT!
    // -----------------------------------------------------------------------
    let hr = HResult::from(0); // S_OK
    trace!(FLAG_ONE, "HRESULT: %!HRESULT!", hr);

    // -----------------------------------------------------------------------
    // String literal — short-form %s
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, "message: %s", "hello world");

    // -----------------------------------------------------------------------
    // Hex display format
    // -----------------------------------------------------------------------
    let flags: u32 = 0xFF;
    trace!(FLAG_ONE, "flags hex: %x", flags);

    let code: i32 = 42;
    trace!(FLAG_ONE, "code decimal: %d", code);

    // -----------------------------------------------------------------------
    // Double/float — %f maps to f64 (ItemDouble)
    // -----------------------------------------------------------------------
    let temperature: f64 = 98.6;
    trace!(FLAG_ONE, "temperature: %f", temperature);

    // -----------------------------------------------------------------------
    // With flag and level
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, Information, "info-level status: %!STATUS!", status);

    // -----------------------------------------------------------------------
    // Mixed types in a single trace call
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, "mixed: i32=%d, u64=%I64u, bool=%!bool!", val_i32, val_u64, verbose_mode);

    driver.set_evt_device_add(evt_device_add);

    Ok(())
}

fn evt_device_add(_device_init: &mut DeviceInit) -> NtResult<()> {
    println!("evt_device_add called");
    Ok(())
}

// ==========================================================================
// Previous experimental code (kept for reference)
// ==========================================================================
//
// The code below was used to experiment with different approaches for
// resolving ETW type metadata at compile time before settling on the
// generic function pattern with T::ETW_TYPE associated constants.
//
// fn codeview_call<T0: ::wdf::__internal::TraceData, T1: ::wdf::__internal::TraceData>(_: &T0, _: &T1) {
//     unsafe {
//         core::intrinsics::codeview_annotation(
//             &[
//                 "TMF:",
//                 "e7602a7b-5034-321b-d450-a986113fc2e1 sample_tracing // SRC=lib.rs MJ= MN=",
//                 "#typev sample_tracing_18 11 \"%0Literal int trace: %10!d!, string: %11!s!\"",
//                 "{",
//                 "literal0, ",
//                 T0::ETW_TYPE,
//                 " -- 10",
//                 "literal1, ",
//                 T1::ETW_TYPE,
//                 " -- 11",
//                 "}",
//             ],
//         );
//     }
// }
//
// The above pattern was generalized into the trace! macro expansion:
// - A generic function is generated per trace! call site
// - Type parameters are bounded by TraceData
// - T::ETW_TYPE provides the ETW type string at monomorphization
// - No transmute needed — all annotation entries are &'static str
