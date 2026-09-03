// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of `__wpp_trace_impl!` proc macro.
//!
//! Parses Rust-style placeholders from the format string, then emits a wppv1
//! `TMF:` annotation into the PDB via `codeview_annotation`.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Expr,
    Ident,
    Lit,
    LitStr,
    Result,
    Token,
    parse::{Parse, ParseStream},
};

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
    /// Span of the format-string literal — used to recover the source file
    /// (module) the trace call lives in, which keys the per-module decode GUID.
    format_span: proc_macro2::Span,
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
        let format_span = fmt_lit.span();

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
            format_span,
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

    // Option B (per-module decode GUID): derive a stable decode GUID from the
    // source file that this trace call lives in. Every trace statement in the
    // same module (file) hashes to the same GUID, so all of a module's TMF
    // annotations — and the decode GUID stamped on each event at runtime — are
    // keyed under one identity that is distinct from the provider control GUID.
    // This avoids the Option A collision where the control GUID doubled as the
    // trace/decode GUID and interleaved with TMC records. `guid` (the control
    // GUID) is retained only for enablement/registration, not for decoding.
    let _ = guid;
    let module_guid = compute_module_decode_guid(&module_key_from_span(parsed.format_span));
    let decode_guid_str = &module_guid.guid_str;
    let (dg1, dg2, dg3) = (module_guid.d1, module_guid.d2, module_guid.d3);
    let dg4_tokens: Vec<TokenStream> = module_guid
        .d4
        .iter()
        .map(|b| {
            let b = *b;
            quote!(#b)
        })
        .collect();

    let format_segments = parse_format(format_string)?;
    let placeholder_count = format_segments.len() - 1;
    if placeholder_count != field_count {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "format string has {} placeholder(s) but {} argument(s)",
                placeholder_count, field_count
            ),
        ));
    }

    let event_id = compute_event_id(format_string);
    let event_id_lit = event_id;

    let annotation_lines = build_tmf_annotation_header(provider_name, decode_guid_str);
    let annotation_strings: Vec<TokenStream> =
        annotation_lines.iter().map(|s| quote!(#s)).collect();

    let flags = if keyword_ident == "__WPP_NO_KEYWORD" {
        String::new()
    } else {
        format!(" FLAGS={}", keyword_ident)
    };
    let typev_prefix = format!(
        "#typev {}_{} {} \"{}",
        provider_name, event_id, event_id, format_segments[0]
    );
    let typev_suffix = format!(
        "\" //   LEVEL={}{}",
        level_name_to_wpp(&parsed.level_name),
        flags
    );

    // Serialization plumbing (unchanged): argument capture, `IntoWppField`
    // conversion, and raw-byte extraction for ETW / IFR.
    let arg_names: Vec<Ident> = (0..field_count)
        .map(|i| format_ident!("__a{}", i))
        .collect();
    let param_names: Vec<Ident> = (0..field_count)
        .map(|i| format_ident!("__f{}", i))
        .collect();
    let bytes_names: Vec<Ident> = (0..field_count)
        .map(|i| format_ident!("__b{}", i))
        .collect();
    let type_names: Vec<Ident> = (0..field_count)
        .map(|i| format_ident!("__WppT{}", i))
        .collect();
    let annotation_const_names: Vec<Ident> = (0..field_count)
        .map(|i| format_ident!("ARG_{}", i))
        .collect();
    let annotation_prefixes: Vec<String> = args
        .iter()
        .map(annotation_argument_prefix)
        .collect();
    let annotation_suffixes: Vec<String> = (0..field_count)
        .map(|i| format!(" -- {}", 10 + i))
        .collect();
    let mut format_parts = Vec::with_capacity(field_count * 2 + 1);
    format_parts.push(quote!(#typev_prefix));
    for (index, type_name) in type_names.iter().enumerate() {
        let segment = &format_segments[index + 1];
        format_parts.push(quote!(#type_name::FORMAT_SPEC));
        format_parts.push(quote!(#segment));
    }
    format_parts.push(quote!(#typev_suffix));

    let annotation_call = if field_count > 0 {
        quote! {
            {
                fn __wpp_emit_annotation<#(#type_names: ::wpp::WppField),*>(
                    #(#param_names: &#type_names),*
                ) {
                    struct Args<#(#type_names),*>(
                        core::marker::PhantomData<(#(#type_names,)*)>
                    );

                    impl<#(#type_names: ::wpp::WppField),*> Args<#(#type_names),*> {
                        const FORMAT_LINE: ::wpp::field::AnnotationString<512> =
                            ::wpp::field::AnnotationString::concat_parts(&[
                                #(#format_parts),*
                            ]);

                        #(
                            const #annotation_const_names: ::wpp::field::AnnotationString<150> =
                                ::wpp::field::AnnotationString::concat(
                                    #annotation_prefixes,
                                    #type_names::TYPE_NAME,
                                    #annotation_suffixes,
                                );
                        )*
                    }

                    impl<#(#type_names: ::wpp::WppField),*>
                        core::hint::CodeViewAnnotationArgs for Args<#(#type_names),*>
                    {
                        const ARGS: &'static [&'static str] = &[
                            #(#annotation_strings),*,
                            Self::FORMAT_LINE.as_str(),
                            "{",
                            #(Self::#annotation_const_names.as_str()),*,
                            "}",
                        ];
                    }

                    core::hint::codeview_annotation::<Args<#(#type_names),*>>();
                }

                __wpp_emit_annotation(#(&#param_names),*);
            }
        }
    } else {
        let typev_line = format!("{}{}", typev_prefix, typev_suffix);
        quote! {
            {
                struct Args;

                impl core::hint::CodeViewAnnotationArgs for Args {
                    const ARGS: &'static [&'static str] = &[
                        #(#annotation_strings),*, #typev_line, "{", "}"
                    ];
                }

                core::hint::codeview_annotation::<Args>();
            }
        }
    };

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

    // Per-module decode GUID: a compile-time constant matching the GUID written
    // into the TMF annotation above. It is sent on every event as the leading
    // `EVENT_DATA_DESCRIPTOR` of type `RESERVED1` (a.k.a. the decode-GUID
    // descriptor), so the kernel stamps `EventHeader.ProviderId` with it and the
    // trace decoder looks up the module's TMF under this same GUID.
    let decode_guid_const = quote! {
        const __WPP_DECODE_GUID: ::wpp::GUID = ::wpp::GUID {
            data1: #dg1, data2: #dg2, data3: #dg3, data4: [#(#dg4_tokens),*],
        };
    };
    // Leading decode-GUID descriptor (Type = EVENT_DATA_DESCRIPTOR_TYPE_RESERVED1).
    let decode_descriptor = quote! {
        ::wpp::etw::EVENT_DATA_DESCRIPTOR {
            Ptr: &__WPP_DECODE_GUID as *const ::wpp::GUID as u64,
            Size: core::mem::size_of::<::wpp::GUID>() as u32,
            Reserved: ::wpp::etw::EVENT_DATA_DESCRIPTOR_TYPE_RESERVED1,
        }
    };
    // Total descriptors written = decode-GUID descriptor + one per field.
    let total_desc_count = (field_count + 1) as u32;

    let output = if field_count > 0 {
        quote! {{
            #decode_guid_const

            #(let #arg_names = #args;)*
            #(let #param_names = ::wpp::IntoWppField::into_wpp_field(#arg_names);)*
            #annotation_call
            #(let #bytes_names = ::wpp::WppField::as_bytes(&#param_names);)*

            // ETW: gated by is_enabled (real-time trace session active)
            {
                let __wpp_kw_val: u64 = #provider_mod::#keyword_ident;
                if #provider_mod::STATE.is_enabled(#level_expr, __wpp_kw_val) {
                    const __WPP_EVT_DESC: ::wpp::etw::EVENT_DESCRIPTOR = ::wpp::etw::EVENT_DESCRIPTOR {
                        Id: #event_id_lit, Version: 0, Channel: 0, Level: #level_expr,
                        Opcode: 0, Task: 0, Keyword: #provider_mod::#keyword_ident,
                    };

                    let __wpp_data: [::wpp::etw::EVENT_DATA_DESCRIPTOR; #total_desc_count as usize] = [
                        #decode_descriptor,
                        #(#data_descriptors),*
                    ];

                    unsafe {
                        ::wpp::etw::write(
                            #provider_mod::STATE.reg_handle(),
                            &__WPP_EVT_DESC,
                            #total_desc_count,
                            __wpp_data.as_ptr(),
                        );
                    }
                }
            }

            // IFR: gated by IFR state auto_log_context (always records when IFR is initialized)
            {
                let __wpp_auto_ctx = #ifr_state.auto_log_context();
                if !__wpp_auto_ctx.is_null() {
                    let mut __wpp_ifr_guid = __WPP_DECODE_GUID;
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
            #decode_guid_const

            // ETW: gated by is_enabled (real-time trace session active)
            {
                let __wpp_kw_val: u64 = #provider_mod::#keyword_ident;
                if #provider_mod::STATE.is_enabled(#level_expr, __wpp_kw_val) {
                    const __WPP_EVT_DESC: ::wpp::etw::EVENT_DESCRIPTOR = ::wpp::etw::EVENT_DESCRIPTOR {
                        Id: #event_id_lit, Version: 0, Channel: 0, Level: #level_expr,
                        Opcode: 0, Task: 0, Keyword: #provider_mod::#keyword_ident,
                    };

                    // Only the leading decode-GUID descriptor is sent (no fields).
                    let __wpp_data: [::wpp::etw::EVENT_DATA_DESCRIPTOR; 1] = [
                        #decode_descriptor,
                    ];

                    unsafe {
                        ::wpp::etw::write(
                            #provider_mod::STATE.reg_handle(),
                            &__WPP_EVT_DESC,
                            1,
                            __wpp_data.as_ptr(),
                        );
                    }
                }
            }

            // IFR: gated by IFR state auto_log_context (always records when IFR is initialized)
            {
                let __wpp_auto_ctx = #ifr_state.auto_log_context();
                if !__wpp_auto_ctx.is_null() {
                    let mut __wpp_ifr_guid = __WPP_DECODE_GUID;
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

fn err(msg: &str) -> syn::Error {
    syn::Error::new(proc_macro2::Span::call_site(), msg.to_string())
}

fn annotation_argument_prefix(arg: &Expr) -> String {
    let mut expression = quote!(#arg).to_string();
    if is_c_string_literal(arg) && expression.starts_with('c') {
        expression.remove(0);
    }

    format!("{expression}, ")
}

fn is_c_string_literal(expr: &Expr) -> bool {
    match expr {
        Expr::Group(group) => is_c_string_literal(&group.expr),
        Expr::Paren(paren) => is_c_string_literal(&paren.expr),
        Expr::Lit(expr_lit) => matches!(expr_lit.lit, Lit::CStr(_)),
        _ => false,
    }
}

/// Parses a Rust-style format string and produces:
///
/// Returns static string segments split at each placeholder. Each placeholder's
/// segment ends with its numbered WPP argument reference (`%10`, `%11`, ...),
/// allowing the type-driven `!spec!` to be inserted between adjacent segments.
///
/// Literal braces are written as `{{` / `}}`, and literal `%` is escaped to
/// `%%` so it survives WPP's printf-style rendering.
fn parse_format(fmt: &str) -> Result<Vec<String>> {
    let mut segments = Vec::new();
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
                if !inner.is_empty() {
                    return Err(err(&format!(
                        "unsupported format placeholder '{{{}}}'; use '{{}}'",
                        inner
                    )));
                }
                out.push('%');
                out.push_str(&arg_num.to_string());
                arg_num += 1;
                segments.push(out);
                out = String::new();
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

    segments.push(out);
    Ok(segments)
}

/// Builds the ordered list of strings that make up a wppv1 `TMF:` annotation.
///
/// Each returned string becomes one line inside the PDB `S_ANNOTATION` record.
fn build_tmf_annotation_header(provider_name: &str, guid: &str) -> Vec<String> {
    let mut lines = Vec::new();
    lines.push("TMF:".to_string());
    lines.push(format!(
        "{} {} // SRC={}.rs MJ= MN=",
        guid, provider_name, provider_name
    ));

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
    let mut hash: u64 = 0xCBF29CE484222325;
    for &byte in data {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001B3);
    }
    hash
}

/// A per-module decode GUID: its canonical string form plus the raw fields used
/// to emit the matching compile-time `::wpp::GUID` constant.
struct ModuleGuid {
    guid_str: String,
    d1: u32,
    d2: u16,
    d3: u16,
    d4: [u8; 8],
}

/// Derives the module key that seeds a module's decode GUID.
///
/// Option B keys the decode GUID on "module path + module name". In Rust's
/// file-based module system the source file path uniquely identifies a module
/// (e.g. `src/device.rs` ⇔ `crate::device`), so the file the trace call lives
/// in is used as the key. Path separators are normalised so the key is stable
/// regardless of host OS conventions. Every trace statement in the same file
/// yields the same key — and therefore the same module decode GUID.
fn module_key_from_span(span: proc_macro2::Span) -> String {
    // `Span::unwrap()` yields the underlying `proc_macro::Span`, whose stable
    // `file()` accessor returns the source path. This is only ever called from
    // within real proc-macro expansion (never unit tests), where `unwrap()` is
    // valid.
    let file = span.unwrap().file();
    file.replace('\\', "/")
}

/// Computes a deterministic v4-shaped decode GUID from a module key.
///
/// The same key always produces the same GUID, so all trace statements in a
/// module share one decode identity. The 128 bits are filled from two FNV-1a
/// passes; the version (4) and variant (RFC 4122) nibbles are then fixed so the
/// value is a well-formed UUID that the trace decoder accepts.
fn compute_module_decode_guid(module_key: &str) -> ModuleGuid {
    let hash = fnv1a_64(module_key.as_bytes());
    let d1 = (hash & 0xFFFF_FFFF) as u32;
    let d2 = ((hash >> 32) & 0xFFFF) as u16;
    // Top nibble of d3 = version 4.
    let d3 = (((hash >> 48) & 0x0FFF) as u16) | 0x4000;

    let hash2 = fnv1a_64(&hash.to_le_bytes());
    let mut d4 = [0u8; 8];
    d4.copy_from_slice(&hash2.to_le_bytes());
    // Two most-significant bits of d4[0] = RFC 4122 variant (10xx).
    d4[0] = (d4[0] & 0x3F) | 0x80;

    let guid_str = format!(
        "{:08x}-{:04x}-{:04x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        d1, d2, d3, d4[0], d4[1], d4[2], d4[3], d4[4], d4[5], d4[6], d4[7]
    );

    ModuleGuid {
        guid_str,
        d1,
        d2,
        d3,
        d4,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_placeholders() {
        let segments = parse_format("Int: {}, Str: {}").unwrap();
        assert_eq!(segments, ["%0Int: %10", ", Str: %11", ""]);
    }

    #[test]
    fn parse_escaped_braces() {
        let segments = parse_format("{{literal}} {}").unwrap();
        assert_eq!(segments, ["%0{literal} %10", ""]);
    }

    #[test]
    fn parse_no_args() {
        let segments = parse_format("no args here").unwrap();
        assert_eq!(segments, ["%0no args here"]);
    }

    #[test]
    fn parse_rejects_format_modifiers() {
        assert!(parse_format("value {:x}").is_err());
        assert!(parse_format("value {=u32}").is_err());
    }

    #[test]
    fn annotation_prefix_drops_c_string_literal_prefix() {
        let expression: Expr = syn::parse_str("c\"test\"").unwrap();
        assert_eq!(annotation_argument_prefix(&expression), "\"test\", ");
    }

    #[test]
    fn annotation_prefix_detects_wrapped_c_string_literal() {
        let expression: Expr = syn::parse_str("(c\"test\")").unwrap();
        assert!(is_c_string_literal(&expression));
    }

    #[test]
    fn annotation_prefix_preserves_other_expressions() {
        let expression: Expr = syn::parse_str("value").unwrap();
        assert_eq!(annotation_argument_prefix(&expression), "value, ");
    }

    #[test]
    fn event_id_deterministic() {
        assert_eq!(compute_event_id("test"), compute_event_id("test"));
    }

    #[test]
    fn event_id_differs() {
        assert_ne!(compute_event_id("hello"), compute_event_id("world"));
    }

    #[test]
    fn module_decode_guid_deterministic() {
        let a = compute_module_decode_guid("src/device.rs");
        let b = compute_module_decode_guid("src/device.rs");
        assert_eq!(a.guid_str, b.guid_str);
    }

    #[test]
    fn module_decode_guid_differs_per_module() {
        let a = compute_module_decode_guid("src/device.rs");
        let b = compute_module_decode_guid("src/queue.rs");
        assert_ne!(a.guid_str, b.guid_str);
    }

    #[test]
    fn module_decode_guid_is_v4_variant() {
        let g = compute_module_decode_guid("src/lib.rs");
        // Version nibble (top of d3) must be 4.
        assert_eq!(g.d3 & 0xF000, 0x4000);
        // Variant (top two bits of d4[0]) must be 0b10.
        assert_eq!(g.d4[0] & 0xC0, 0x80);
    }
}
