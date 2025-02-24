// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! A collection of macros used for writing WDF-based drivers in safe Rust

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Error, Ident, ItemFn, ItemImpl, ItemStruct};

/// A procedural macro that when placed on a safe Rust impl of a driver
/// generates the relevant FFI wrappers
#[proc_macro_attribute]
pub fn driver_impl(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_clone = input.clone();
    let item_impl = parse_macro_input!(input_clone as ItemImpl);

    let driver_ty = item_impl.self_ty;

    let mut wrappers: TokenStream = quote! {
        use wdf::{nt_success, println, WdkAllocator};
        use wdf::ffi::{
            ntddk::KeGetCurrentIrql,
            APC_LEVEL,
            DRIVER_OBJECT,
            NTSTATUS,
            PCUNICODE_STRING,
            PDRIVER_OBJECT,
            PWDFDEVICE_INIT,
            STATUS_SUCCESS,
            ULONG,
            UNICODE_STRING,
            WDFDRIVER,
            WDFOBJECT,
            WDFSTRING,
            WDFDEVICE,
            WDFDEVICE_INIT,
            WDF_DRIVER_CONFIG,
            WDF_DRIVER_VERSION_AVAILABLE_PARAMS,
            WDF_NO_HANDLE,
            WDF_NO_OBJECT_ATTRIBUTES,
        };

        extern crate alloc;

        use alloc::{slice, string::String};

        #[global_allocator]
        static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;

        static SAFE_DRIVER: #driver_ty = #driver_ty;

        #[link_section = "INIT"]
        #[export_name = "DriverEntry"] // WDF expects a symbol with the name DriverEntry
        extern "system" fn driver_entry(driver: &mut DRIVER_OBJECT, registry_path: PCUNICODE_STRING,) -> NTSTATUS {
            driver.DriverUnload = Some(driver_exit);

            let mut driver_config = WDF_DRIVER_CONFIG {
                Size: core::mem::size_of::<WDF_DRIVER_CONFIG>() as ULONG,
                EvtDriverDeviceAdd: Some(evt_driver_device_add),
                ..WDF_DRIVER_CONFIG::default()
            };

            let driver_handle_output = WDF_NO_HANDLE.cast::<WDFDRIVER>();

            let nt_status = unsafe {
                wdk_sys::call_unsafe_wdf_function_binding!(
                    WdfDriverCreate,
                    driver as PDRIVER_OBJECT,
                    registry_path,
                    WDF_NO_OBJECT_ATTRIBUTES,
                    &mut driver_config,
                    driver_handle_output,
                )
            };

            if !nt_success(nt_status) {
                return nt_status;
            }

            match SAFE_DRIVER.driver_entry() {
                Ok(_) => 0,
                Err(nt_status) => nt_status,
            }
        }

        #[link_section = "PAGE"]
        extern "C" fn evt_driver_device_add(
            _driver: WDFDRIVER,
            mut device_init: *mut WDFDEVICE_INIT,
        ) -> NTSTATUS {
            // println!("EvtDriverDeviceAdd Entered!");

            let mut device_handle_output: WDFDEVICE = WDF_NO_HANDLE.cast();

            let ntstatus;
            // SAFETY: This is safe because:
            //       1. `device_init` is provided by `EvtDriverDeviceAdd` and is never null
            //       2. the argument receiving `WDF_NO_OBJECT_ATTRIBUTES` is allowed to be
            //          null
            //       3. `device_handle_output` is expected to be null
            unsafe {
                ntstatus = wdk_sys::call_unsafe_wdf_function_binding!(
                    WdfDeviceCreate,
                    &mut device_init,
                    WDF_NO_OBJECT_ATTRIBUTES,
                    &mut device_handle_output,
                );
            }

            // println!("WdfDeviceCreate NTSTATUS: {ntstatus:#02x}");

            match SAFE_DRIVER.evt_driver_device_add() {
                Ok(_) => 0,
                Err(nt_status) => nt_status,
            }
        }

        extern "C" fn driver_exit(_driver: *mut DRIVER_OBJECT) {
            // println!("Goodbye World!");
            // println!("Driver Exit Complete!");
        }
    }
    .into();

    wrappers.extend(input);

    wrappers
}

/// A procedural macro used to mark the entry point of a WDF driver
#[proc_macro_attribute]
pub fn driver_entry(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_clone = input.clone();
    let item_fn = parse_macro_input!(input_clone as ItemFn);
    let safe_driver_entry = item_fn.sig.ident;

    let mut wrappers: TokenStream = quote! {
        use wdf::{
            call_safe_driver_entry,
            DRIVER_OBJECT,
            NTSTATUS,
            PCUNICODE_STRING,
        };

        #[link_section = "INIT"]
        #[export_name = "DriverEntry"] // WDF expects a symbol with the name DriverEntry
        extern "system" fn __driver_entry(driver: &mut DRIVER_OBJECT, registry_path: PCUNICODE_STRING,) -> NTSTATUS {
            call_safe_driver_entry(driver, registry_path, #safe_driver_entry)
        }
    }
    .into();

    wrappers.extend(input);

    wrappers
}

// /// A procedural macro used to mark the entry point of a WDF driver
// #[proc_macro_attribute]
// pub fn driver_entry(_args: TokenStream, input: TokenStream) -> TokenStream {
//     let input_clone = input.clone();
//     let item_fn = parse_macro_input!(input_clone as ItemFn);
//     let safe_driver_entry = item_fn.sig.ident;

//     let mut wrappers: TokenStream = quote! {
//         use wdf::{Driver, nt_success, println, WdkAllocator};
//         use wdf::ffi::{
//             call_unsafe_wdf_function_binding,
//             ntddk::KeGetCurrentIrql,
//             APC_LEVEL,
//             DRIVER_OBJECT,
//             NTSTATUS,
//             PCUNICODE_STRING,
//             PDRIVER_OBJECT,
//             PWDF_DRIVER_CONFIG,
//             PWDF_OBJECT_ATTRIBUTES,
//             PWDFDEVICE_INIT,
//             STATUS_SUCCESS,
//             ULONG,
//             UNICODE_STRING,
//             WDFDRIVER,
//             WDFOBJECT,
//             WDFSTRING,
//             WDFDEVICE,
//             WDFDEVICE_INIT,
//             WDF_DRIVER_CONFIG,
//             WDF_DRIVER_VERSION_AVAILABLE_PARAMS,
//             WDF_NO_HANDLE,
//             WDF_NO_OBJECT_ATTRIBUTES,
//         };

//         extern crate alloc;

//         use alloc::{slice, string::String};

//         #[global_allocator]
//         static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;

//         #[link_section = "INIT"]
//         #[export_name = "DriverEntry"] // WDF expects a symbol with the name DriverEntry
//         extern "system" fn __driver_entry(driver: &mut DRIVER_OBJECT, registry_path: PCUNICODE_STRING,) -> NTSTATUS {
//             driver.DriverUnload = Some(driver_unload);

//             let mut driver_config = WDF_DRIVER_CONFIG {
//                 Size: core::mem::size_of::<WDF_DRIVER_CONFIG>() as ULONG,
//                 EvtDriverDeviceAdd: Some(evt_driver_device_add),
//                 ..WDF_DRIVER_CONFIG::default()
//             };

//             let driver_handle_output = WDF_NO_HANDLE.cast::<WDFDRIVER>();

//             let nt_status = unsafe {
//                 call_unsafe_wdf_function_binding!(
//                     WdfDriverCreate,
//                     driver as PDRIVER_OBJECT,
//                     registry_path,
//                     WDF_NO_OBJECT_ATTRIBUTES,
//                     &mut driver_config,
//                     driver_handle_output
//                 )
//             };

//             if !nt_success(nt_status) {
//                 return nt_status;
//             }

//             let driver = Driver;

//             match #safe_driver_entry(&driver, "") {
//                 Ok(_) => 0,
//                 Err(nt_status) => nt_status,
//             }
//         }

//         #[link_section = "PAGE"]
//         extern "C" fn evt_driver_device_add(
//             _driver: WDFDRIVER,
//             mut _device_init: *mut WDFDEVICE_INIT,
//         ) -> NTSTATUS {
//             match EVT_DEVICE_ADD() {
//                 Ok(_) => 0,
//                 Err(nt_status) => nt_status,
//             }

//             // println!("EvtDriverDeviceAdd Entered!");

//         //     let mut device_handle_output: WDFDEVICE = WDF_NO_HANDLE.cast();

//         //     let ntstatus;
//         //     // SAFETY: This is safe because:
//         //     //       1. `device_init` is provided by `EvtDriverDeviceAdd` and is never null
//         //     //       2. the argument receiving `WDF_NO_OBJECT_ATTRIBUTES` is allowed to be
//         //     //          null
//         //     //       3. `device_handle_output` is expected to be null
//         //     unsafe {
//         //         ntstatus = call_unsafe_wdf_function_binding!(
//         //             WdfDeviceCreate,
//         //             &mut device_init,
//         //             WDF_NO_OBJECT_ATTRIBUTES,
//         //             &mut device_handle_output,
//         //         );
//         //     }

//         //     // println!("WdfDeviceCreate NTSTATUS: {ntstatus:#02x}");

//         //     match SAFE_DRIVER.evt_driver_device_add() {
//         //         Ok(_) => 0,
//         //         Err(nt_status) => nt_status,
//         //     }
//         }

//         extern "C" fn driver_unload(_driver: *mut DRIVER_OBJECT) {
//             // println!("Goodbye World!");
//             // println!("Driver Exit Complete!");
//         }
//     }
//     .into();

//     wrappers.extend(input);

//     wrappers
// }


/// The attribute used to mark a struct as a WDF object context
#[proc_macro_attribute]
pub fn object_context(attr: TokenStream, item: TokenStream) -> TokenStream {
    let wdf_obj_type_name = parse_macro_input!(attr as Ident);
    let context_struct = parse_macro_input!(item as ItemStruct);


    // Check if the struct is generic
    if !context_struct.generics.params.is_empty() {
        return Error::new_spanned(context_struct, "The `object_context` attribute cannot be applied to generic structs.")
            .to_compile_error()
            .into();
    }

    // Establish wdf crate's path to use 
    let wdf_crate_path = if std::env::var("CARGO_PKG_NAME").ok() == Some("wdf".to_string()) {
        quote!(crate) // Inside the `wdf` crate itself
    } else {
        quote!(::wdf) // Outside of `wdf`, use the global path
    };


    let struct_name = &context_struct.ident;
    let static_name = Ident::new(&format!("__WDF_{}_TYPE_INFO", struct_name), struct_name.span());
    let cleanup_callback_name = Ident::new(&format!("__evt_{}_cleanup", struct_name), struct_name.span());

    
    let expanded = quote! {
        #context_struct

        #[allow(non_upper_case_globals)]
        #[link_section = ".data"]
        static #static_name: #wdf_crate_path::WdfObjectContextTypeInfo = #wdf_crate_path::WdfObjectContextTypeInfo::new(#wdf_crate_path::WDF_OBJECT_CONTEXT_TYPE_INFO {
            Size: core::mem::size_of::<#wdf_crate_path::WdfObjectContextTypeInfo>() as u32,
            ContextName: concat!(stringify!(#struct_name),'\0').as_bytes().as_ptr().cast(),
            ContextSize: core::mem::size_of::<#struct_name>(),
            UniqueType: core::ptr::addr_of!(#static_name) as *const #wdf_crate_path::WDF_OBJECT_CONTEXT_TYPE_INFO,
            EvtDriverGetUniqueContextType: None,
        });

        impl #struct_name {
            fn attach(wdf_obj: &mut #wdf_obj_type_name, context: #struct_name) -> Result<(), NtError> where Self: Sync {
                unsafe {
                    #wdf_crate_path::ObjectContext::attach(wdf_obj, context, &#static_name, #cleanup_callback_name)
                }
            }

            fn get(wdf_obj: &#wdf_obj_type_name) -> Option<&#struct_name> where Self: Sync {
                unsafe {
                    #wdf_crate_path::ObjectContext::get(wdf_obj, &#static_name)
                }
            }
        }

        #[allow(non_snake_case)]
        extern "C" fn #cleanup_callback_name(wdf_obj: #wdf_crate_path::WDFOBJECT) {
            unsafe {
                #wdf_crate_path::ObjectContext::drop::<#struct_name>(wdf_obj, &#static_name);
            }
        }
    };

    TokenStream::from(expanded)
}
