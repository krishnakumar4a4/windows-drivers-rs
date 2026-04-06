#![no_std]
#![feature(codeview_annotation)]
#![feature(core_intrinsics)]

// Nightly features to get compile time reflection
#![feature(type_info)]
#![feature(const_cmp)]
#![feature(const_trait_impl)]

use wdf::{
    driver_entry, println, trace, Driver, DeviceInit, NtResult,
};

fn test() -> i32 {
42
}

/// The entry point for the driver.
#[driver_entry(trace_control = ("cb94defb-592a-4509-8f2e-54f204929669", [FLAG_ONE, FLAG_TWO]))]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    println!("Sample tracing driver entry");

    // Trace with a literal only
    trace!("Hello from sample-tracing driver!");

    // Trace with a literal integer argument (auto-detected)
   //  trace!(FLAG_ONE, "Literal int trace: {}", 42);

   {
        let __trace_arg0: i32 = core::hint::black_box(42_i32);
      //   let __trace_arg0_etw_type = ::wdf::__internal::format_spec_of(&__trace_arg0);
      //   unsafe {
      //       let annotation: &'static [&'static str] = core::mem::transmute::<
      //               &[&str],
      //               &'static [&'static str],
      //           >(
      //               &[
      //                   "TMF:",
      //                   "e7602a7b-5034-321b-d450-a986113fc2e1 sample_tracing // SRC=lib.rs MJ= MN=",
      //                   "#typev sample_tracing_18 11 \"%0Literal int trace: %10!d!\"",
      //                   "{",
      //                   "literal0, ",
      //                   __trace_arg0_etw_type,
      //                   " -- 10",
      //                   "}",
      //               ],
      //           );
      //       core::intrinsics::codeview_annotation(annotation);
      //    }


      //   unsafe {
      //       // let __trace_arg0_etw_type: &'static str = ::wdf::__internal::format_spec_of(&__trace_arg0);
      //       // const __trace_arg0_etw_type: &'static str = const_format::formatcp!("{}", ::wdf::__internal::format_spec_of(&__trace_arg0));
      //       core::intrinsics::codeview_annotation(
      //          // core::mem::transmute::<
      //          //      &[&str],
      //          //      &'static [&'static str],
      //          //  >(
      //               &[
      //                   "TMF:",
      //                   "e7602a7b-5034-321b-d450-a986113fc2e1 sample_tracing // SRC=lib.rs MJ= MN=",
      //                   "#typev sample_tracing_18 11 \"%0Literal int trace: %10!d!\"",
      //                   "{",
      //                   "literal0, ",
      //                   // ::wdf::__internal::format_spec_of(&__trace_arg0),
      //                   ::wdf::__internal::primitive_name_of_val(&__trace_arg0),
      //                   " -- 10",
      //                   "}",
      //               ],
      //          //  )
      //       );
      //    }
   
         // Compile time reflection using nightly rust features
         use core::any::TypeId;
         use core::mem::type_info::{Type, TypeKind};


         #[inline(always)]
         pub fn codeview_annotation_with_type<T: 'static>(_: &T) {
            // const {     
               let ty: Type = TypeId::of::<T>().info();
               let k = match ty.kind {
                     TypeKind::Bool(_) => "bool",
                     TypeKind::Char(_) => "char",
                     TypeKind::Float(f) => match f.bits {
                        16 => "f16",
                        32 => "f32",
                        64 => "f64",
                        128 => "f128",
                        _ => "unknown-float",
                     },
                     TypeKind::Int(_) => {
                        // Current nightly docs clearly expose TypeKind::Int(Int),
                        // but the fetched docs here don't expose Int's exact public fields.
                        // So for a compileable sample, dispatch by concrete type parameter.
                        if TypeId::of::<T>() == TypeId::of::<u8>() {
                           "u8"
                        } else if TypeId::of::<T>() == TypeId::of::<u16>() {
                           "u16"
                        } else if TypeId::of::<T>() == TypeId::of::<u32>() {
                           "u32"
                        } else if TypeId::of::<T>() == TypeId::of::<u64>() {
                           "u64"
                        } else if TypeId::of::<T>() == TypeId::of::<u128>() {
                           "u128"
                        } else if TypeId::of::<T>() == TypeId::of::<usize>() {
                           "usize"
                        } else if TypeId::of::<T>() == TypeId::of::<i8>() {
                           "i8"
                        } else if TypeId::of::<T>() == TypeId::of::<i16>() {
                           "i16"
                        } else if TypeId::of::<T>() == TypeId::of::<i32>() {
                           "i32"
                        } else if TypeId::of::<T>() == TypeId::of::<i64>() {
                           "i64"
                        } else if TypeId::of::<T>() == TypeId::of::<i128>() {
                           "i128"
                        } else if TypeId::of::<T>() == TypeId::of::<isize>() {
                           "isize"
                        } else {
                           "unknown-int"
                        }
                     },
                     _ => "non-primitive",
               };

               unsafe {
                  core::intrinsics::codeview_annotation(
                     core::mem::transmute::<
                        &[&str],
                        &'static [&'static str],
                     >(
                        &[
                              "TMF:",
                              "e7602a7b-5034-321b-d450-a986113fc2e1 sample_tracing // SRC=lib.rs MJ= MN=",
                              "#typev sample_tracing_18 11 \"%0Literal int trace: %10!d!\"",
                              "{",
                              "literal0, ",
                              // ::wdf::__internal::format_spec_of(&__trace_arg0),
                              k,
                              " -- 10",
                              "}",
                        ],
                     )
                  );
               // }
            }
         };

         codeview_annotation_with_type(&__trace_arg0);
        
        let __trace_bytes0 = ::wdf::__internal::TraceData::as_bytes(&__trace_arg0);
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
                        11,
                        __trace_bytes0.as_ptr() as *const core::ffi::c_void,
                        __trace_bytes0.len(),
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
                        11,
                        __trace_bytes0.as_ptr() as *const core::ffi::c_void,
                        __trace_bytes0.len(),
                        core::ptr::null::<core::ffi::c_void>(),
                    );
                }
            }
        }
    };

    // Trace with variables requiring explicit format specifiers
    let status_code: i32 = 0;
    let iteration: u64 = 12345;
    trace!(FLAG_ONE, "Driver loaded with status {:d} iteration {:u64}", status_code, iteration);

    // Trace with a boolean variable
    let verbose_mode: bool = true;
    trace!(FLAG_TWO, "Verbose mode enabled: {:bool}", verbose_mode);

    driver.set_evt_device_add(evt_device_add);

    Ok(())
}

fn evt_device_add(_device_init: &mut DeviceInit) -> NtResult<()> {
    println!("evt_device_add called");
    Ok(())
}
