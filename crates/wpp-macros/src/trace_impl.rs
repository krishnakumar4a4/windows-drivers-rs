// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of `__wpp_trace_impl!` proc macro.
//!
//! Generates a monomorphized generic function where `T::TYPE_NAME` resolves
//! to concrete type strings during monomorphization, embedding them in the PDB
//! via `codeview_annotation`.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Expr, Ident, LitStr, Token, Result};

// ─── AST ────────────────────────────────────────────────────────────────────

struct TraceImplInput {
    provider_mod: TokenStream,
    ifr_state: TokenStream,
    provider_name: String,
    guid: String,
    level_expr: Expr,
    level_name: String,
    keyword_ident: Ident,
    format_string: String,
    args: Vec<Expr>,
}

// ─── Parsing ────────────────────────────────────────────────────────────────

impl Parse for TraceImplInput {
    fn parse(input: ParseStream) -> Result<Self> {
        parse_at_key(input, "provider_mod")?;
        let provider_mod = parse_path_tokens(input)?;

        input.parse::<Token![,]>()?;
        parse_at_key(input, "ifr_state")?;
        let ifr_state = parse_path_tokens(input)?;

        input.parse::<Token![,]>()?;
        parse_at_key(input, "provider_name")?;
        let provider_name: LitStr = input.parse()?;

        input.parse::<Token![,]>()?;
        parse_at_key(input, "guid")?;
        let guid: LitStr = input.parse()?;

        input.parse::<Token![,]>()?;
        parse_at_key(input, "level")?;
        let level_expr: Expr = input.parse()?;

        input.parse::<Token![,]>()?;
        parse_at_key(input, "level_name")?;
        let level_name_ident: Ident = input.parse()?;

        input.parse::<Token![,]>()?;
        parse_at_key(input, "keyword")?;
        let keyword_ident: Ident = input.parse()?;

        input.parse::<Token![,]>()?;
        parse_at_key(input, "fmt")?;
        let fmt_lit: LitStr = input.parse()?;

        let mut args = Vec::new();
        while input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            args.push(input.parse::<Expr>()?);
        }

        Ok(TraceImplInput {
            provider_mod,
            ifr_state,
            provider_name: provider_name.value(),
            guid: guid.value(),
            level_expr,
            level_name: level_name_ident.to_string(),
            keyword_ident,
            format_string: fmt_lit.value(),
            args,
        })
    }
}

fn parse_at_key(input: ParseStream, expected: &str) -> Result<()> {
    input.parse::<Token![@]>()?;
    let key: Ident = input.parse()?;
    if key != expected {
        return Err(syn::Error::new(
            key.span(),
            format!("expected @{}", expected),
        ));
    }
    Ok(())
}

fn parse_path_tokens(input: ParseStream) -> Result<TokenStream> {
    let mut tokens = TokenStream::new();
    while !input.is_empty() && !input.peek(Token![,]) {
        let tt: proc_macro2::TokenTree = input.parse()?;
        tokens.extend(std::iter::once(tt));
    }
    Ok(tokens)
}

// ─── Code generation ────────────────────────────────────────────────────────

pub fn generate(input: TokenStream) -> Result<TokenStream> {
    let parsed: TraceImplInput = syn::parse2(input)?;

    let provider_mod = &parsed.provider_mod;
    let ifr_state = &parsed.ifr_state;
    let provider_name = &parsed.provider_name;
    let guid = &parsed.guid;
    let level_expr = &parsed.level_expr;
    let keyword_ident = &parsed.keyword_ident;
    let format_string = &parsed.format_string;
    let args = &parsed.args;
    let field_count = args.len();

    let event_id = compute_event_id(format_string);
    let placeholder_count = count_placeholders(format_string);
    if placeholder_count != field_count {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "format string has {} placeholder(s) but {} argument(s)",
                placeholder_count, field_count
            ),
        ));
    }

    let event_id_lit = event_id;
    let field_count_u32 = field_count as u32;
    let event_id_str = event_id.to_string();
    let level_str = level_name_to_num(&parsed.level_name);
    let keyword_str = keyword_ident.to_string();

    let type_params: Vec<Ident> =
        (0..field_count).map(|i| format_ident!("T{}", i)).collect();
    let arg_names: Vec<Ident> =
        (0..field_count).map(|i| format_ident!("__a{}", i)).collect();
    let param_names: Vec<Ident> =
        (0..field_count).map(|i| format_ident!("__f{}", i)).collect();
    let bytes_names: Vec<Ident> =
        (0..field_count).map(|i| format_ident!("__b{}", i)).collect();

    let fn_params: Vec<TokenStream> = type_params
        .iter()
        .zip(param_names.iter())
        .map(|(t, p)| quote!(#p: &#t))
        .collect();
    let bounds: Vec<TokenStream> = type_params
        .iter()
        .map(|t| quote!(#t: ::wpp::IntoWppField))
        .collect();
    let type_name_exprs: Vec<TokenStream> =
        type_params.iter().map(|t| quote!(<<#t as ::wpp::IntoWppField>::Output as ::wpp::WppField>::TYPE_NAME)).collect();
    let call_args: Vec<TokenStream> = arg_names.iter().map(|a| quote!(&#a)).collect();

    let data_descriptors: Vec<TokenStream> = bytes_names
        .iter()
        .map(|b| {
            quote! {
                ::wpp::etw::EVENT_DATA_DESCRIPTOR {
                    Ptr: #b.as_ptr() as u64,
                    Size: #b.len() as u32,
                    Reserved: 0,
                }
            }
        })
        .collect();

    // IFR: byte pair arguments for WppAutoLogTrace variadic call
    let ifr_arg_pairs: Vec<TokenStream> = bytes_names
        .iter()
        .map(|b| {
            quote! { #b.as_ptr() as *const core::ffi::c_void, #b.len(), }
        })
        .collect();

    let output = if field_count > 0 {
        quote! {{
            #[inline(always)]
            fn __wpp_schema< #(#bounds),* >( #(#fn_params),* ) {
                core::hint::codeview_annotation!(
                    "WPP_EVENT",
                    #provider_name,
                    #guid,
                    #event_id_str,
                    #level_str,
                    #keyword_str,
                    #format_string,
                    #(#type_name_exprs),*
                );
            }
            #(let #arg_names = #args;)*
            __wpp_schema( #(#call_args),* );

            #(let #param_names = ::wpp::IntoWppField::into_wpp_field(#arg_names);)*
            #(let #bytes_names = ::wpp::WppField::as_bytes(&#param_names);)*

            // ETW: gated by is_enabled (real-time trace session active)
            {
                let __wpp_kw_val: u64 = #provider_mod::#keyword_ident;
                if #provider_mod::STATE.is_enabled(#level_expr, __wpp_kw_val) {
                    const __WPP_EVT_DESC: ::wpp::etw::EVENT_DESCRIPTOR = ::wpp::etw::EVENT_DESCRIPTOR {
                        Id: #event_id_lit, Version: 0, Channel: 0, Level: #level_expr,
                        Opcode: 0, Task: 0, Keyword: #provider_mod::#keyword_ident,
                    };

                    let __wpp_data: [::wpp::etw::EVENT_DATA_DESCRIPTOR; #field_count_u32 as usize] = [
                        #(#data_descriptors),*
                    ];

                    unsafe {
                        ::wpp::etw::write(
                            #provider_mod::STATE.reg_handle(),
                            &__WPP_EVT_DESC,
                            #field_count_u32,
                            __wpp_data.as_ptr(),
                        );
                    }
                }
            }

            // IFR: gated by IFR state auto_log_context (always records when IFR is initialized)
            {
                let __wpp_auto_ctx = #ifr_state.auto_log_context();
                if !__wpp_auto_ctx.is_null() {
                    let mut __wpp_ifr_guid = *#provider_mod::control_guid();
                    let __wpp_ifr_status = unsafe {
                        ::wpp::ifr::WppAutoLogTrace(
                            __wpp_auto_ctx,
                            #level_expr,
                            (#provider_mod::#keyword_ident & 0xFFFF_FFFF) as u32,
                            &mut __wpp_ifr_guid as *mut ::wpp::GUID
                                as *mut core::ffi::c_void,
                            #event_id_lit,
                            #(#ifr_arg_pairs)*
                            core::ptr::null::<core::ffi::c_void>(),
                        )
                    };
                    ::wdf::println!("WppAutoLogTrace status: {}, ctx: {:?}", __wpp_ifr_status, __wpp_auto_ctx);
                } else {
                    ::wdf::println!("WppAutoLogTrace skipped: auto_log_context is null");
                }
            }
        }}
    } else {
        quote! {{
            core::hint::codeview_annotation!(
                "WPP_EVENT", #provider_name, #guid, #event_id_str,
                #level_str, #keyword_str, #format_string
            );

            // ETW: gated by is_enabled (real-time trace session active)
            {
                let __wpp_kw_val: u64 = #provider_mod::#keyword_ident;
                if #provider_mod::STATE.is_enabled(#level_expr, __wpp_kw_val) {
                    const __WPP_EVT_DESC: ::wpp::etw::EVENT_DESCRIPTOR = ::wpp::etw::EVENT_DESCRIPTOR {
                        Id: #event_id_lit, Version: 0, Channel: 0, Level: #level_expr,
                        Opcode: 0, Task: 0, Keyword: #provider_mod::#keyword_ident,
                    };

                    unsafe {
                        ::wpp::etw::write(
                            #provider_mod::STATE.reg_handle(), &__WPP_EVT_DESC, 0, core::ptr::null(),
                        );
                    }
                }
            }

            // IFR: gated by IFR state auto_log_context (always records when IFR is initialized)
            {
                let __wpp_auto_ctx = #ifr_state.auto_log_context();
                if !__wpp_auto_ctx.is_null() {
                    let mut __wpp_ifr_guid = *#provider_mod::control_guid();
                    let __wpp_ifr_status = unsafe {
                        ::wpp::ifr::WppAutoLogTrace(
                            __wpp_auto_ctx,
                            #level_expr,
                            (#provider_mod::#keyword_ident & 0xFFFF_FFFF) as u32,
                            &mut __wpp_ifr_guid as *mut ::wpp::GUID
                                as *mut core::ffi::c_void,
                            #event_id_lit,
                            core::ptr::null::<core::ffi::c_void>(),
                        )
                    };
                    ::wdf::println!("WppAutoLogTrace status: {}, ctx: {:?}", __wpp_ifr_status, __wpp_auto_ctx);
                } else {
                    ::wdf::println!("WppAutoLogTrace skipped: auto_log_context is null");
                }
            }
        }}
    };

    Ok(output)
}

// ─── Helpers ────────────────────────────────────────────────────────────────

fn level_name_to_num(name: &str) -> String {
    match name {
        "CRITICAL" => "1",
        "ERROR" => "2",
        "WARNING" => "3",
        "INFO" => "4",
        "VERBOSE" => "5",
        other => other,
    }
    .into()
}

fn compute_event_id(fmt: &str) -> u16 {
    ((fnv1a_64(fmt.as_bytes()) % 65534) + 1) as u16
}

fn count_placeholders(fmt: &str) -> usize {
    let mut count = 0;
    let mut chars = fmt.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                chars.next();
            } else {
                while let Some(c2) = chars.next() {
                    if c2 == '}' {
                        break;
                    }
                }
                count += 1;
            }
        }
    }
    count
}

fn fnv1a_64(data: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325;
    for &byte in data {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn count_simple() {
        assert_eq!(count_placeholders("{} {}"), 2);
    }

    #[test]
    fn count_escaped() {
        assert_eq!(count_placeholders("{{}} {}"), 1);
    }

    #[test]
    fn event_id_deterministic() {
        assert_eq!(compute_event_id("test"), compute_event_id("test"));
    }

    #[test]
    fn event_id_differs() {
        assert_ne!(compute_event_id("hello"), compute_event_id("world"));
    }
}
