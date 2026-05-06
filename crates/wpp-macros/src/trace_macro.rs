// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of the `trace!` proc macro.

use proc_macro::TokenStream;
use quote::quote;
use syn::Error;

use crate::format_spec::{generate_wpp_format_string, rust_assert_type_tokens};
use crate::guid::{TRACE_MESSAGE_ID, format_guid, generate_deterministic_guid, guid_to_tokens};
use crate::trace_args::{ParsedTraceArgs, TraceArg, parse_trace_args};

/// Core implementation of the `trace!` proc macro.
pub(crate) fn trace_impl(item: TokenStream) -> TokenStream {
    let span = proc_macro::Span::call_site();

    let source_file = span.file();
    let file_name = std::path::Path::new(&source_file)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown.rs");
    let line_number = span.line();

    let parsed = match parse_trace_args(item) {
        Ok(parsed) => parsed,
        Err(e) => return e.to_compile_error().into(),
    };

    let ParsedTraceArgs { flag, level, format_str, format_specs, args } = parsed;

    if args.len() > 8 {
        return Error::new(
            proc_macro2::Span::call_site(),
            format!("trace! supports at most 8 arguments (got {}); add a new pre-generated trace_N method on WppWriter to raise the limit", args.len()),
        ).to_compile_error().into();
    }

    let message_id = TRACE_MESSAGE_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

    let driver_name = std::env::var("CARGO_PKG_NAME")
        .unwrap_or_else(|_| "unknown_driver".to_string())
        .replace('-', "_");

    let trace_guid_bytes = generate_deterministic_guid(&source_file);
    let trace_guid_str = format_guid(&trace_guid_bytes);
    let param2 = format!("{} {} // SRC={} MJ= MN=", trace_guid_str, driver_name, file_name);

    let wpp_format = generate_wpp_format_string(&format_str, &format_specs);
    let typev_name = format!("{}_{}", driver_name, line_number);
    let param3 = format!("#typev {} {} \"%0{}\"", typev_name, message_id, wpp_format);

    let arg_descriptors: Vec<String> = args.iter().enumerate().map(|(idx, arg)| {
        let param_num = 10 + idx;
        format!("{}, {} -- {}", arg.name, arg.spec.etw_type, param_num)
    }).collect();

    let mut annotation_args = vec![
        quote! { "TMF:" },
        quote! { #param2 },
        quote! { #param3 },
        quote! { "{" },
    ];
    for desc in &arg_descriptors {
        annotation_args.push(quote! { #desc });
    }
    annotation_args.push(quote! { "}" });

    let method_name = format!("trace_{}", args.len());
    let method_ident = syn::Ident::new(&method_name, proc_macro2::Span::call_site());

    let method_call = generate_trace_method_call(
        &method_ident, &trace_guid_bytes, message_id as u16,
        &args, flag.as_ref(), level.as_ref(),
    );

    let expanded = quote! {
        core::hint::codeview_annotation!(#(#annotation_args),*);
        {
            #method_call
        }
    };

    expanded.into()
}

/// Generates the call site that invokes one of the pre-generated
/// `WppProvider::trace_N` inherent methods.
fn generate_trace_method_call(
    method_ident: &syn::Ident,
    trace_guid_bytes: &[u8; 16],
    message_id: u16,
    args: &[TraceArg],
    flag: Option<&syn::Ident>,
    level: Option<&syn::Ident>,
) -> proc_macro2::TokenStream {
    let guid_literal = guid_to_tokens(trace_guid_bytes);
    let arg_bindings: Vec<proc_macro2::TokenStream> = args.iter().enumerate().map(|(idx, arg)| {
        let arg_var = syn::Ident::new(&format!("__trace_arg_{}", idx), proc_macro2::Span::call_site());
        let expr = &arg.expr;
        let rust_type = &arg.spec.rust_assert_type;

        if arg.spec.is_complex && rust_type == "&str" {
            quote! {
                let #arg_var = ::wdf::__internal::CString::new(#expr).unwrap();
            }
        } else if arg.spec.is_complex && rust_type == "Display" {
            let assert_fn = syn::Ident::new(&format!("__assert_display_{}", idx), proc_macro2::Span::call_site());
            quote! {
                fn #assert_fn(_: &impl core::fmt::Display) {}
                let __display_value = &#expr;
                #assert_fn(__display_value);
                let mut #arg_var = ::wdf::__internal::TraceFmtBuf::new();
                core::fmt::write(&mut #arg_var, format_args!("{}", __display_value)).unwrap();
                #arg_var.finalize();
            }
        } else {
            let assert_type = rust_assert_type_tokens(rust_type);
            quote! {
                let #arg_var: #assert_type = #expr;
            }
        }
    }).collect();

    let arg_refs: Vec<proc_macro2::TokenStream> = args.iter().enumerate().map(|(idx, _)| {
        let arg_var = syn::Ident::new(&format!("__trace_arg_{}", idx), proc_macro2::Span::call_site());
        quote! { &#arg_var }
    }).collect();

    let message_id_lit = proc_macro2::Literal::u16_unsuffixed(message_id);

    let level_value = if let Some(level_ident) = level {
        quote! { crate::TraceLevel::#level_ident as u8 }
    } else {
        quote! { crate::TraceLevel::None as u8 }
    };

    let (flags_value, control_index) = if let Some(flag_ident) = flag {
        let flag_name = flag_ident.to_string();
        (
            quote! { { let __flag = crate::WppFlag::by_name(#flag_name).expect("Unknown WppFlag"); (__flag.flag_index() + 1) as u32 } },
            quote! { { let __flag = crate::WppFlag::by_name(#flag_name).expect("Unknown WppFlag"); __flag.control_index() } }
        )
    } else {
        (quote! { 0u32 }, quote! { 0usize })
    };

    quote! {
        {
            #(#arg_bindings)*

            let __trace_level: u8 = #level_value;
            let __trace_flags: u32 = #flags_value;
            let __control_index: usize = #control_index;

            if let Some(__provider) = ::wdf::get_provider(__control_index) {
                let __should_trace_wpp = (__trace_flags == 0 || __provider.is_flag_enabled(__trace_flags))
                                            && __provider.is_level_enabled(__trace_level);
                let __should_auto_log = __trace_level < crate::TraceLevel::Verbose as u8 || __provider.is_auto_log_verbose_enabled();

                __provider.#method_ident(
                    __trace_level,
                    __trace_flags,
                    #message_id_lit,
                    &#guid_literal,
                    __should_trace_wpp,
                    __should_auto_log,
                    #(#arg_refs),*
                );
            }
        }
    }
}
