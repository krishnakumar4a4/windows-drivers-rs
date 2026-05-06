// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of the `define_trace_writer_methods!` proc macro.

use proc_macro::TokenStream;
use quote::quote;

/// Core implementation: emits inherent `trace_0`..`trace_8` methods on
/// `wpp::writer::WppProvider`.
pub(crate) fn define_trace_writer_methods_impl() -> TokenStream {
    const MAX_ARITY: usize = 8;
    let mut methods: Vec<proc_macro2::TokenStream> = Vec::with_capacity(MAX_ARITY + 1);

    for n in 0..=MAX_ARITY {
        let method_ident = syn::Ident::new(&format!("trace_{}", n), proc_macro2::Span::call_site());

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
                        let logger = self.get_wpp_logger();
                        if let Some(trace_message) = self.get_wpp_trace_message() {
                            let _ = trace_message(
                                logger,
                                crate::writer::WPP_TRACE_OPTIONS,
                                trace_guid,
                                id,
                                #(#arg_pairs)*
                                core::ptr::null::<core::ffi::c_void>(),
                            );
                        }
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
