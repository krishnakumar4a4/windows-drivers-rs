#![no_std]
#![feature(codeview_annotation)]
#![feature(core_intrinsics)]

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
/// Demonstrates the `trace!` macro with all supported Rust primitive types,
/// NTSTATUS, HRESULT, and GUID. Variables no longer require explicit format
/// specifiers — the ETW type is resolved via `TraceData::ETW_TYPE` at compile
/// time through generic monomorphization.
#[driver_entry(trace_control = ("cb94defb-592a-4509-8f2e-54f204929669", [FLAG_ONE, FLAG_TWO]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    println!("Sample tracing driver entry");

    // -----------------------------------------------------------------------
    // Literal-only trace (no arguments)
    // -----------------------------------------------------------------------
    trace!("Hello from sample-tracing driver!");

    // -----------------------------------------------------------------------
    // Integer literals — auto-detected (bare {} works)
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, "Literal i32: {}", 42);
    trace!(FLAG_ONE, "Literal u64: {}", 12345u64);

    // -----------------------------------------------------------------------
    // All integer primitive types — no format specifier needed!
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

    trace!(FLAG_ONE, "i8={}, i16={}", val_i8, val_i16);
    trace!(FLAG_ONE, "i32={}, i64={}", val_i32, val_i64);
   //  trace!(FLAG_ONE, "u8={}, u16={}", val_u8, val_u16);

   {
        let __trace_arg0 = val_u8;
        let __trace_arg1 = val_u16;
        #[inline(always)]
        fn __trace_codeview_15<
            T0: ::wdf::__internal::TraceData,
            T1: ::wdf::__internal::TraceData,
        >(_arg0: &T0, _arg1: &T1) {
               core::intrinsics::codeview_annotation(
                  &[
                     "TMF:",
                     "e7602a7b-5034-321b-d450-a986113fc2e1 sample_tracing // SRC=lib.rs MJ= MN=",
                     "#typev sample_tracing_52 15 \"%0u8=%10!d!, u16=%11!d!\"",
                     "{",
                     "val_u8, ",
                     T0::ETW_TYPE,
                     " -- 10",
                     "val_u16, ",
                     T1::ETW_TYPE,
                     " -- 11",
                     "}",
                  ],
               );
        }
        __trace_codeview_15(&__trace_arg0, &__trace_arg1);
        let __trace_bytes0 = ::wdf::__internal::TraceData::as_bytes(&__trace_arg0);
        let __trace_bytes1 = ::wdf::__internal::TraceData::as_bytes(&__trace_arg1);
        let __trace_level: u8 = TraceLevel::None as u8;
        let __trace_flags: u32 = {
            let __flag = WppFlag::by_name("FLAG_ONE").expect("Unknown WppFlag");
            (__flag.flag_index() + 1) as u32
        };
        let __control_index: usize = {
            let __flag = WppFlag::by_name("FLAG_ONE").expect("Unknown WppFlag");
            __flag.control_index()
        };
        if let Some(__trace_writer) = ::wdf::get_trace_writer() {
            let __should_trace_wpp = (__trace_flags == 0
                || __trace_writer.is_flag_enabled(__control_index, __trace_flags))
                && __trace_writer.is_level_enabled(__control_index, __trace_level);
            let __should_auto_log = __trace_level < TraceLevel::Verbose as u8
                || __trace_writer.is_auto_log_verbose_enabled(__control_index);
            unsafe {
                if __should_trace_wpp {
                    let __logger = ::wdf::__internal::get_wpp_logger().unwrap();
                    let _ = ::wdf::__internal::get_wpp_trace_message()
                        .unwrap()(
                        __logger,
                        ::wdf::__internal::WPP_TRACE_OPTIONS,
                        &::wdf::__internal::TRACE_GUID,
                        15,
                        __trace_bytes0.as_ptr() as *const core::ffi::c_void,
                        __trace_bytes0.len(),
                        __trace_bytes1.as_ptr() as *const core::ffi::c_void,
                        __trace_bytes1.len(),
                        core::ptr::null::<core::ffi::c_void>(),
                    );
                }
                if __should_auto_log {
                    let __auto_log_context = ::wdf::__internal::get_auto_log_context()
                        .unwrap();
                    let _ = ::wdf::__internal::WppAutoLogTrace(
                        __auto_log_context,
                        __trace_level,
                        __trace_flags,
                        &::wdf::__internal::TRACE_GUID as *const _ as *mut _,
                        15,
                        __trace_bytes0.as_ptr() as *const core::ffi::c_void,
                        __trace_bytes0.len(),
                        __trace_bytes1.as_ptr() as *const core::ffi::c_void,
                        __trace_bytes1.len(),
                        core::ptr::null::<core::ffi::c_void>(),
                    );
                }
            }
        }
    };

    trace!(FLAG_ONE, "u32={}, u64={}", val_u32, val_u64);
    trace!(FLAG_ONE, "isize={}, usize={}", val_isize, val_usize);

    // -----------------------------------------------------------------------
    // Boolean — TraceData resolves to ItemListLong(false,true)
    // -----------------------------------------------------------------------
    let verbose_mode: bool = true;
    trace!(FLAG_TWO, "verbose={}", verbose_mode);

    // -----------------------------------------------------------------------
    // NTSTATUS — TraceData resolves to ItemNTSTATUS
    // -----------------------------------------------------------------------
    let status = NtStatus::from(0); // STATUS_SUCCESS
    trace!(FLAG_ONE, "NTSTATUS: {}", status);

    // -----------------------------------------------------------------------
    // HRESULT — TraceData resolves to ItemHRESULT
    // -----------------------------------------------------------------------
    let hr = HResult::from(0); // S_OK
    trace!(FLAG_ONE, "HRESULT: {}", hr);

    // -----------------------------------------------------------------------
    // String literal — auto-wrapped in CString by macro
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, "message: {}", "hello world");

    // -----------------------------------------------------------------------
    // Optional display format overrides (hex, decimal)
    // -----------------------------------------------------------------------
    let flags: u32 = 0xFF;
    trace!(FLAG_ONE, "flags hex: {:x}", flags);

    let code: i32 = 42;
    trace!(FLAG_ONE, "code decimal: {:d}", code);

    // -----------------------------------------------------------------------
    // With flag and level
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, Information, "info-level status: {}", status);

    // -----------------------------------------------------------------------
    // Mixed types in a single trace call
    // -----------------------------------------------------------------------
    trace!(FLAG_ONE, "mixed: i32={}, u64={}, bool={}", val_i32, val_u64, verbose_mode);

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
