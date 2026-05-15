// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of the `define_trace_writer_methods!` proc macro.
//!
//! Generates `trace_0`..`trace_8` methods that use modern ETW APIs
//! (`EtwWriteTransfer` via `crate::etw::write`) instead of legacy
//! `WmiTraceMessage`.

use proc_macro::TokenStream;
use quote::quote;

/// Core implementation: emits inherent `trace_0`..`trace_8` methods on
/// `wpp::writer::WppProvider`.
///
/// The WPP trace path now builds an `EVENT_DESCRIPTOR` and
/// `EVENT_DATA_DESCRIPTOR` array and calls `crate::etw::write()` instead
/// of the legacy variadic `WmiTraceMessage`.
pub(crate) fn define_trace_writer_methods_impl() -> TokenStream {
    const MAX_ARITY: usize = 8;
    let mut methods: Vec<proc_macro2::TokenStream> = Vec::with_capacity(MAX_ARITY + 1);

    for n in 0..=MAX_ARITY {
        let method_ident = syn::Ident::new(&format!("trace_{n}"), proc_macro2::Span::call_site());

        let type_params: Vec<syn::Ident> = (0..n)
            .map(|i| syn::Ident::new(&format!("A{}", i + 1), proc_macro2::Span::call_site()))
            .collect();
        let arg_idents: Vec<syn::Ident> = (0..n)
            .map(|i| syn::Ident::new(&format!("a{}", i + 1), proc_macro2::Span::call_site()))
            .collect();
        let bytes_idents: Vec<syn::Ident> = (0..n)
            .map(|i| syn::Ident::new(&format!("__b{}", i + 1), proc_macro2::Span::call_site()))
            .collect();

        let generics = if n == 0 {
            quote! {}
        } else {
            quote! { < #(#type_params : crate::wpp_arg::WppArgData),* > }
        };

        let arg_params = arg_idents.iter().zip(type_params.iter()).map(|(a, t)| {
            quote! { #a: & #t }
        });

        let byte_bindings = arg_idents.iter().zip(bytes_idents.iter()).map(|(a, b)| {
            quote! { let #b = crate::wpp_arg::WppArgData::as_bytes(#a); }
        });

        // Build EVENT_DATA_DESCRIPTOR array for EtwWriteTransfer
        let data_descriptor_entries: Vec<proc_macro2::TokenStream> = bytes_idents.iter().map(|b| {
            quote! {
                crate::etw::EVENT_DATA_DESCRIPTOR {
                    Ptr: #b.as_ptr() as u64,
                    Size: #b.len() as u32,
                    Reserved: 0,
                }
            }
        }).collect();

        let n_u32 = n as u32;
        let n_lit = proc_macro2::Literal::u32_unsuffixed(n_u32);

        let etw_write_block = if n > 0 {
            quote! {
                let __wpp_data: [crate::etw::EVENT_DATA_DESCRIPTOR; #n_lit as usize] = [
                    #(#data_descriptor_entries),*
                ];
                let _ = crate::etw::write(
                    self.reg_handle(),
                    &__wpp_evt_desc as *const crate::etw::EVENT_DESCRIPTOR,
                    #n_lit,
                    __wpp_data.as_ptr(),
                );
            }
        } else {
            quote! {
                let _ = crate::etw::write(
                    self.reg_handle(),
                    &__wpp_evt_desc as *const crate::etw::EVENT_DESCRIPTOR,
                    0,
                    core::ptr::null(),
                );
            }
        };

        // Auto-log path still uses variadic WppAutoLogTrace
        let arg_pairs: Vec<proc_macro2::TokenStream> = bytes_idents.iter().map(|b| {
            quote! { #b.as_ptr(), #b.len(), }
        }).collect();

        methods.push(quote! {
            #[doc(hidden)]
            #[inline]
            pub fn #method_ident #generics (
                &self,
                level: ::wdk_sys::UCHAR,
                flags: ::wdk_sys::ULONG,
                id: ::wdk_sys::USHORT,
                trace_guid: ::wdk_sys::LPCGUID,
                should_trace_wpp: bool,
                should_auto_log: bool,
                #(#arg_params),*
            ) {
                #(#byte_bindings)*
                unsafe {
                    if should_trace_wpp {
                        let __keyword: u64 = if flags > 0 {
                            1u64 << ((flags as u64 - 1) & 31)
                        } else {
                            0u64
                        };
                        let __wpp_evt_desc = crate::etw::EVENT_DESCRIPTOR {
                            Id: id,
                            Version: 0,
                            Channel: 0,
                            Level: level,
                            Opcode: 0,
                            Task: 0,
                            Keyword: __keyword,
                        };
                        #etw_write_block
                    }

                    if should_auto_log {
                        let auto_log_context = self.get_auto_log_context();
                        let _ = crate::writer::WppAutoLogTrace(
                            auto_log_context,
                            level,
                            flags,
                            trace_guid.cast_mut().cast(),
                            id,
                            #(#arg_pairs)*
                            core::ptr::null::<core::ffi::c_void>(),
                        );
                    }
                }
            }
        });
    }

    let expanded = quote! { #(#methods)* };
    expanded.into()
}
