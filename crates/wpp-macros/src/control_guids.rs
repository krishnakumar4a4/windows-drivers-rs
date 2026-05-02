// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of `wpp_control_guids!` proc macro.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{braced, Ident, LitInt, Token, Result};

// ─── AST ────────────────────────────────────────────────────────────────────

struct ProviderDecl {
    name: Ident,
    guid_str: String,
    guid_parts: GuidParts,
    keywords: Vec<KeywordDecl>,
}

struct KeywordDecl {
    name: Ident,
    bit_position: u32,
}

#[derive(Clone)]
struct GuidParts {
    d1: u32,
    d2: u16,
    d3: u16,
    d4: [u8; 8],
}

struct ControlGuidsInput {
    providers: Vec<ProviderDecl>,
}

// ─── Parsing ────────────────────────────────────────────────────────────────

impl Parse for ControlGuidsInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut providers = Vec::new();
        while !input.is_empty() {
            providers.push(input.parse::<ProviderDecl>()?);
        }
        if providers.is_empty() {
            return Err(input.error("expected at least one provider declaration"));
        }
        Ok(ControlGuidsInput { providers })
    }
}

impl Parse for ProviderDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        let guid_str = parse_guid(input)?;
        let guid_parts = parse_guid_parts(&guid_str)
            .map_err(|e| input.error(format!("invalid GUID: {}", e)))?;

        let content;
        braced!(content in input);
        let mut keywords = Vec::new();
        let mut next_auto_bit: u32 = 0;

        while !content.is_empty() {
            let kw_name: Ident = content.parse()?;
            let bit_pos = if content.peek(Token![=]) {
                content.parse::<Token![=]>()?;
                let lit: LitInt = content.parse()?;
                let pos = lit.base10_parse::<u32>()?;
                next_auto_bit = pos + 1;
                pos
            } else {
                let pos = next_auto_bit;
                next_auto_bit += 1;
                pos
            };
            keywords.push(KeywordDecl { name: kw_name, bit_position: bit_pos });
            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }

        Ok(ProviderDecl { name, guid_str, guid_parts, keywords })
    }
}

fn parse_guid(input: ParseStream) -> Result<String> {
    let mut raw = String::new();
    while !input.is_empty() && !input.peek(syn::token::Brace) {
        let tt: proc_macro2::TokenTree = input.parse()?;
        match &tt {
            proc_macro2::TokenTree::Punct(p) => raw.push(p.as_char()),
            _ => raw.push_str(&tt.to_string()),
        }
    }
    let guid: String = raw.chars().filter(|c| !c.is_whitespace()).collect();
    let segs: Vec<&str> = guid.split('-').collect();
    if segs.len() != 5 {
        return Err(input.error(format!(
            "GUID must have 5 dash-separated segments, found {}: '{}'",
            segs.len(),
            guid
        )));
    }
    let expected_lens = [8, 4, 4, 4, 12];
    for (i, (seg, &exp)) in segs.iter().zip(&expected_lens).enumerate() {
        if seg.len() != exp {
            return Err(input.error(format!(
                "GUID segment {} has length {} (expected {}): '{}'",
                i,
                seg.len(),
                exp,
                guid
            )));
        }
        if !seg.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(input.error(format!(
                "GUID segment {} contains non-hex characters: '{}'",
                i, seg
            )));
        }
    }
    Ok(guid)
}

fn parse_guid_parts(s: &str) -> core::result::Result<GuidParts, String> {
    let segs: Vec<&str> = s.split('-').collect();
    if segs.len() != 5 {
        return Err("expected 5 segments".into());
    }
    let d1 = u32::from_str_radix(segs[0], 16).map_err(|e| e.to_string())?;
    let d2 = u16::from_str_radix(segs[1], 16).map_err(|e| e.to_string())?;
    let d3 = u16::from_str_radix(segs[2], 16).map_err(|e| e.to_string())?;
    let d4_hi = u16::from_str_radix(segs[3], 16).map_err(|e| e.to_string())?;
    let d4_lo = u64::from_str_radix(segs[4], 16).map_err(|e| e.to_string())?;
    let mut d4 = [0u8; 8];
    d4[0] = (d4_hi >> 8) as u8;
    d4[1] = d4_hi as u8;
    d4[2] = (d4_lo >> 40) as u8;
    d4[3] = (d4_lo >> 32) as u8;
    d4[4] = (d4_lo >> 24) as u8;
    d4[5] = (d4_lo >> 16) as u8;
    d4[6] = (d4_lo >> 8) as u8;
    d4[7] = d4_lo as u8;
    Ok(GuidParts { d1, d2, d3, d4 })
}

// ─── Code generation ────────────────────────────────────────────────────────

pub fn generate(input: TokenStream) -> Result<TokenStream> {
    let parsed: ControlGuidsInput = syn::parse2(input)?;
    let provider_count = parsed.providers.len();
    let mut output = TokenStream::new();

    for provider in &parsed.providers {
        output.extend(generate_provider_module(provider));
    }
    for provider in &parsed.providers {
        let macro_name = if provider_count == 1 {
            format_ident!("trace")
        } else {
            format_ident!("{}_trace", provider.name)
        };
        output.extend(generate_trace_macro(&macro_name, provider));
    }
    output.extend(generate_level_macro());
    Ok(output)
}

fn generate_provider_module(p: &ProviderDecl) -> TokenStream {
    let mod_name = &p.name;
    let guid_str = &p.guid_str;
    let gp = &p.guid_parts;
    let (d1, d2, d3) = (gp.d1, gp.d2, gp.d3);
    let d4_tokens: Vec<TokenStream> = gp.d4.iter().map(|b| {
        let b = *b;
        quote!(#b)
    }).collect();

    let dg = compute_decode_guid(&p.name.to_string(), guid_str);
    let (dd1, dd2, dd3) = (dg.d1, dg.d2, dg.d3);
    let dd4_tokens: Vec<TokenStream> = dg.d4.iter().map(|b| {
        let b = *b;
        quote!(#b)
    }).collect();

    let kw_consts: Vec<TokenStream> = p.keywords.iter().map(|kw| {
        let name = &kw.name;
        let value = 1u64 << kw.bit_position;
        quote! { #[allow(non_upper_case_globals)] pub const #name: u64 = #value; }
    }).collect();

    let provider_name_str = p.name.to_string();
    let kw_annotation_strings: Vec<String> = p.keywords.iter()
        .map(|kw| format!("{}={}", kw.name, kw.bit_position))
        .collect();

    quote! {
        #[allow(non_snake_case)]
        pub mod #mod_name {
            pub const CONTROL_GUID: ::wpp::GUID = ::wpp::GUID {
                data1: #d1, data2: #d2, data3: #d3, data4: [#(#d4_tokens),*],
            };
            pub const DECODE_GUID: ::wpp::GUID = ::wpp::GUID {
                data1: #dd1, data2: #dd2, data3: #dd3, data4: [#(#dd4_tokens),*],
            };
            #(#kw_consts)*
            pub static STATE: ::wpp::ProviderState = ::wpp::ProviderState::new();

            /// # Safety
            ///
            /// The caller must ensure `clean_up()` is called before the
            /// module containing this provider is unloaded.
            pub unsafe fn init() {
                core::hint::codeview_annotation!(
                    "WPP_PROVIDER", #provider_name_str, #guid_str,
                    #(#kw_annotation_strings),*
                );
                if STATE.init_state.compare_exchange(
                    ::wpp::provider::UNINITIALIZED,
                    ::wpp::provider::INITIALIZING,
                    core::sync::atomic::Ordering::Acquire,
                    core::sync::atomic::Ordering::Relaxed,
                ).is_err() {
                    return;
                }
                let (_, handle) = unsafe { ::wpp::etw::register(
                    &CONTROL_GUID,
                    Some(::wpp::provider::enable_callback),
                    &STATE as *const ::wpp::ProviderState as *mut core::ffi::c_void,
                ) };
                STATE.reg_handle.store(handle, core::sync::atomic::Ordering::Relaxed);
                unsafe { ::wpp::etw::set_decode_guid(handle, &DECODE_GUID) };
                STATE.init_state.store(
                    ::wpp::provider::INITIALIZED,
                    core::sync::atomic::Ordering::Release,
                );
            }

            pub fn clean_up() {
                if STATE.init_state.compare_exchange(
                    ::wpp::provider::INITIALIZED,
                    ::wpp::provider::UNINITIALIZED,
                    core::sync::atomic::Ordering::Acquire,
                    core::sync::atomic::Ordering::Relaxed,
                ).is_err() {
                    return;
                }
                STATE.enabled_level.store(0, core::sync::atomic::Ordering::Relaxed);
                STATE.enabled_keywords.store(0, core::sync::atomic::Ordering::Relaxed);
                let handle = STATE.reg_handle.swap(0, core::sync::atomic::Ordering::Relaxed);
                if handle != 0 { unsafe { ::wpp::etw::unregister(handle) }; }
            }
        }
    }
}

fn generate_trace_macro(macro_name: &Ident, provider: &ProviderDecl) -> TokenStream {
    let mod_name = &provider.name;
    let provider_name_str = provider.name.to_string();
    let guid_str = &provider.guid_str;
    let dollar = proc_macro2::Punct::new('$', proc_macro2::Spacing::Alone);

    quote! {
        #[macro_export]
        macro_rules! #macro_name {
            (#dollar level:ident, #dollar kw:ident, #dollar fmt:literal #dollar(, #dollar arg:expr)*) => {{
                const __WPP_LEVEL: u8 = __wpp_level_to_u8!(#dollar level);
                let __wpp_kw_val: u64 = #dollar crate::#mod_name::#dollar kw;
                if #dollar crate::#mod_name::STATE.is_enabled(__WPP_LEVEL, __wpp_kw_val) {
                    ::wpp::__wpp_trace_impl!(
                        @provider_mod #dollar crate::#mod_name,
                        @provider_name #provider_name_str,
                        @guid #guid_str,
                        @level __WPP_LEVEL,
                        @level_name #dollar level,
                        @keyword #dollar kw,
                        @fmt #dollar fmt
                        #dollar(, #dollar arg)*
                    );
                }
            }};
        }
    }
}

fn generate_level_macro() -> TokenStream {
    quote! {
        #[macro_export]
        macro_rules! __wpp_level_to_u8 {
            (CRITICAL) => { 1u8 };
            (ERROR)    => { 2u8 };
            (WARNING)  => { 3u8 };
            (INFO)     => { 4u8 };
            (VERBOSE)  => { 5u8 };
        }
    }
}

fn compute_decode_guid(name: &str, guid_str: &str) -> GuidParts {
    let input = format!("{}:{}", name, guid_str);
    let hash = fnv1a_64(input.as_bytes());

    let d1 = (hash & 0xFFFF_FFFF) as u32;
    let d2 = ((hash >> 32) & 0xFFFF) as u16;
    let d3 = ((hash >> 48) & 0x0FFF) as u16 | 0xD000;

    let hash2 = fnv1a_64(&hash.to_le_bytes());
    let mut d4 = [0u8; 8];
    for (i, b) in hash2.to_le_bytes().iter().enumerate() {
        d4[i] = *b;
    }
    d4[0] = (d4[0] & 0x3F) | 0x80;

    GuidParts { d1, d2, d3, d4 }
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
    fn guid_parsing() {
        let gp = parse_guid_parts("84bdb2e9-829e-41b3-b891-02f454bc2bd7").unwrap();
        assert_eq!(gp.d1, 0x84bdb2e9);
        assert_eq!(gp.d2, 0x829e);
        assert_eq!(gp.d3, 0x41b3);
    }

    #[test]
    fn decode_guid_deterministic() {
        let g1 = compute_decode_guid("Test", "12345678-1234-1234-1234-123456789abc");
        let g2 = compute_decode_guid("Test", "12345678-1234-1234-1234-123456789abc");
        assert_eq!(g1.d1, g2.d1);
        assert_eq!(g1.d2, g2.d2);
    }
}
