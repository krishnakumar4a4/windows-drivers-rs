// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of `__wpp_trace_impl!` proc macro.
//!
//! Parses defmt-style type hints (e.g. `{=i32}`, `{=str}`) from the format
//! string to recover argument types at macro-expansion time, then emits a
//! wppv1 `TMF:` annotation into the PDB via `codeview_annotation`.

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

    // Parse defmt-style type hints (e.g. `{=i32}`, `{=str}`) out of the format
    // string. Because the hints carry the concrete type at macro-expansion
    // time, we no longer need a monomorphized generic to recover type names.
    let (hints, wpp_format) = parse_format(format_string)?;
    if hints.len() != field_count {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "format string has {} type-hinted placeholder(s) but {} argument(s)",
                hints.len(),
                field_count
            ),
        ));
    }

    let event_id = compute_event_id(format_string);
    let event_id_lit = event_id;
    let field_count_u32 = field_count as u32;

    // Build the wppv1 TMF annotation lines. Every part is known at expansion
    // time, so each line is emitted as a plain string literal in the PDB's
    // `S_ANNOTATION` record. The trace GUID is the provider's control GUID and
    // the message number is the computed event id.
    let annotation_lines = build_tmf_annotation(
        provider_name,
        guid,
        event_id,
        &wpp_format,
        &level_name_to_wpp(&parsed.level_name),
        &keyword_ident.to_string(),
        &hints,
        args,
    );
    let annotation_strings: Vec<TokenStream> =
        annotation_lines.iter().map(|s| quote!(#s)).collect();
    let annotation_call = quote! {
        core::hint::codeview_annotation!( #(#annotation_strings),* );
    };

    // Serialization plumbing (unchanged): argument capture, `IntoWppField`
    // conversion, and raw-byte extraction for ETW / IFR.
    let arg_names: Vec<Ident> =
        (0..field_count).map(|i| format_ident!("__a{}", i)).collect();
    let param_names: Vec<Ident> =
        (0..field_count).map(|i| format_ident!("__f{}", i)).collect();
    let bytes_names: Vec<Ident> =
        (0..field_count).map(|i| format_ident!("__b{}", i)).collect();

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
            #annotation_call

            #(let #arg_names = #args;)*
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
            #annotation_call

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

/// A parsed type hint: the WPP `Item*` reader type and its printf specifier.
type Hint = (&'static str, &'static str);

/// Maps a defmt-style type name (the text after `=` in `{=type}`) to the WPP
/// item type used in the TMF annotation and its printf-style format specifier.
///
/// The item type controls how the trace decoder reads the serialized bytes, so
/// the byte width must match the wire format produced by `WppField::as_bytes`.
fn hint_for(ty: &str) -> Option<Hint> {
    Some(match ty {
        "i8" => ("ItemChar", "!d!"),
        "u8" => ("ItemUChar", "!u!"),
        "i16" => ("ItemShort", "!d!"),
        "u16" => ("ItemUShort", "!u!"),
        "i32" => ("ItemLong", "!d!"),
        "u32" => ("ItemULong", "!u!"),
        "i64" => ("ItemLongLong", "!I64d!"),
        "u64" => ("ItemULongLong", "!I64u!"),
        "isize" => ("ItemLongLong", "!I64d!"),
        "usize" => ("ItemULongLong", "!I64u!"),
        "f32" => ("ItemFloat", "!f!"),
        "f64" => ("ItemDouble", "!f!"),
        "bool" => ("ItemUChar", "!d!"),
        "str" | "cstr" => ("ItemString", "!s!"),
        _ => return None,
    })
}

fn err(msg: &str) -> syn::Error {
    syn::Error::new(proc_macro2::Span::call_site(), msg.to_string())
}

/// Parses a defmt-style format string and produces:
///
/// * the ordered list of type hints for the placeholders, and
/// * the equivalent wppv1 format string where each `{=type}` becomes
///   `%N!spec!` (arguments numbered from 10) and the whole string is prefixed
///   with `%0` (the WPP "no prefix" marker).
///
/// Literal braces are written as `{{` / `}}`, and literal `%` is escaped to
/// `%%` so it survives WPP's printf-style rendering.
fn parse_format(fmt: &str) -> Result<(Vec<Hint>, String)> {
    let mut hints = Vec::new();
    let mut out = String::from("%0");
    let mut arg_num = 10u32;
    let mut chars = fmt.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '{' => {
                if chars.peek() == Some(&'{') {
                    chars.next();
                    out.push('{');
                    continue;
                }
                let mut inner = String::new();
                let mut closed = false;
                for c2 in chars.by_ref() {
                    if c2 == '}' {
                        closed = true;
                        break;
                    }
                    inner.push(c2);
                }
                if !closed {
                    return Err(err("unclosed '{' in format string"));
                }
                let inner = inner.trim();
                let ty = inner.strip_prefix('=').ok_or_else(|| {
                    err(&format!(
                        "placeholder '{{{}}}' must carry a type hint like '{{=i32}}'; \
                         bare '{{}}' is not supported",
                        inner
                    ))
                })?;
                let ty = ty.trim();
                let (item, spec) = hint_for(ty)
                    .ok_or_else(|| err(&format!("unknown type hint '={}'", ty)))?;
                out.push('%');
                out.push_str(&arg_num.to_string());
                out.push_str(spec);
                arg_num += 1;
                hints.push((item, spec));
            }
            '}' => {
                if chars.peek() == Some(&'}') {
                    chars.next();
                    out.push('}');
                } else {
                    return Err(err("unmatched '}' in format string"));
                }
            }
            '%' => out.push_str("%%"),
            other => out.push(other),
        }
    }

    Ok((hints, out))
}

/// Builds the ordered list of strings that make up a wppv1 `TMF:` annotation.
///
/// Each returned string becomes one line inside the PDB `S_ANNOTATION` record.
fn build_tmf_annotation(
    provider_name: &str,
    guid: &str,
    event_id: u16,
    wpp_format: &str,
    level_wpp: &str,
    keyword: &str,
    hints: &[Hint],
    args: &[Expr],
) -> Vec<String> {
    let mut lines = Vec::new();
    lines.push("TMF:".to_string());
    lines.push(format!(
        "{} {} // SRC={}.rs MJ= MN=",
        guid, provider_name, provider_name
    ));

    let flags = if keyword == "__WPP_NO_KEYWORD" {
        String::new()
    } else {
        format!(" FLAGS={}", keyword)
    };
    lines.push(format!(
        "#typev {}_{} {} \"{}\" //   LEVEL={}{}",
        provider_name, event_id, event_id, wpp_format, level_wpp, flags
    ));

    lines.push("{".to_string());
    for (i, ((item, _spec), arg)) in hints.iter().zip(args.iter()).enumerate() {
        let arg_text = quote!(#arg).to_string();
        lines.push(format!("{}, {} -- {}", arg_text, item, 10 + i));
    }
    lines.push("}".to_string());
    lines
}

fn level_name_to_wpp(name: &str) -> String {
    match name {
        "CRITICAL" => "TRACE_LEVEL_CRITICAL",
        "ERROR" => "TRACE_LEVEL_ERROR",
        "WARNING" => "TRACE_LEVEL_WARNING",
        "INFO" => "TRACE_LEVEL_INFORMATION",
        "VERBOSE" => "TRACE_LEVEL_VERBOSE",
        other => other,
    }
    .into()
}

fn compute_event_id(fmt: &str) -> u16 {
    ((fnv1a_64(fmt.as_bytes()) % 65534) + 1) as u16
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
    fn parse_simple_hints() {
        let (hints, fmt) = parse_format("Int: {=i32}, Str: {=str}").unwrap();
        assert_eq!(hints.len(), 2);
        assert_eq!(hints[0], ("ItemLong", "!d!"));
        assert_eq!(hints[1], ("ItemString", "!s!"));
        assert_eq!(fmt, "%0Int: %10!d!, Str: %11!s!");
    }

    #[test]
    fn parse_escaped_braces() {
        let (hints, fmt) = parse_format("{{literal}} {=u32}").unwrap();
        assert_eq!(hints.len(), 1);
        assert_eq!(fmt, "%0{literal} %10!u!");
    }

    #[test]
    fn parse_no_args() {
        let (hints, fmt) = parse_format("no args here").unwrap();
        assert!(hints.is_empty());
        assert_eq!(fmt, "%0no args here");
    }

    #[test]
    fn parse_rejects_bare_placeholder() {
        assert!(parse_format("value {}").is_err());
    }

    #[test]
    fn parse_rejects_unknown_hint() {
        assert!(parse_format("value {=widget}").is_err());
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
