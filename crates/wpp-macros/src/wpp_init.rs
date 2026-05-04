// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of the `wpp_init!` proc macro — WPP control block &
//! flag/level initialization.

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{Error, Ident, Token, parse_macro_input};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

use crate::guid::{guid_str_to_tokens, is_valid_guid};

/// Represents a single trace control definition with provider name, GUID and optional flags
#[derive(Debug, Clone)]
pub(crate) struct TraceControlDef {
    pub provider_name: String,
    pub guid: String,
    pub flags: Vec<String>,
}

impl Parse for TraceControlDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::parenthesized!(content in input);

        let provider_name_lit: syn::LitStr = content.parse()?;
        let provider_name = provider_name_lit.value();

        if provider_name.is_empty() {
            return Err(Error::new_spanned(provider_name_lit, "Provider name must not be empty"));
        }

        content.parse::<Token![,]>()?;

        let guid_lit: syn::LitStr = content.parse()?;
        let guid = guid_lit.value();

        if !is_valid_guid(&guid) {
            return Err(Error::new_spanned(guid_lit, "Not a valid GUID"));
        }

        let flags = if content.peek(Token![,]) {
            content.parse::<Token![,]>()?;

            let flags_content;
            syn::bracketed!(flags_content in content);

            let flags_punctuated: Punctuated<Ident, Token![,]> =
                Punctuated::parse_terminated(&flags_content)?;

            flags_punctuated.iter()
                .map(|ident| ident.to_string())
                .collect()
        } else {
            Vec::new()
        };

        Ok(TraceControlDef { provider_name, guid, flags })
    }
}

/// Represents all trace control definitions
pub(crate) struct TraceControlArgs {
    pub controls: Vec<TraceControlDef>,
}

impl Parse for TraceControlArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let controls: Punctuated<TraceControlDef, Token![,]> =
            Punctuated::parse_separated_nonempty(input)?;

        Ok(TraceControlArgs {
            controls: controls.into_iter().collect(),
        })
    }
}

fn validate_no_duplicate_flags(controls: &[TraceControlDef]) -> Result<(), (String, String, String)> {
    let mut seen_flags: HashSet<String> = HashSet::new();

    for control in controls {
        for flag in &control.flags {
            if !seen_flags.insert(flag.clone()) {
                for prev_control in controls {
                    if prev_control.flags.contains(flag) {
                        return Err((flag.clone(), prev_control.guid.clone(), control.guid.clone()));
                    }
                }
            }
        }
    }

    Ok(())
}

/// Generates the WppFlag enum declaration
fn generate_wpp_flags_declaration(trace_controls: &[TraceControlDef]) -> proc_macro2::TokenStream {
    let has_flags = trace_controls.iter().any(|tc| !tc.flags.is_empty());

    // TODO: TraceLevel enum declaration generation is skipped when flags are absent. Fix this as we
    // want TraceLevel to be available regardless of flags.
    if !has_flags {
        return quote! {};
    }

    struct VariantInfo {
        variant_ident: syn::Ident,
        flag_name: String,
        control_idx: usize,
        flag_idx: usize,
    }

    let variants: Vec<VariantInfo> = trace_controls.iter()
        .enumerate()
        .flat_map(|(control_idx, tc)| {
            tc.flags.iter().enumerate().map(move |(flag_idx, flag_name)| {
                VariantInfo {
                    variant_ident: Ident::new(flag_name, proc_macro2::Span::call_site()),
                    flag_name: flag_name.clone(),
                    control_idx,
                    flag_idx,
                }
            })
        })
        .collect();

    let enum_variants: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { #ident(usize, usize) }
    }).collect();

    let name_match_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        let name = &v.flag_name;
        let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
        let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
        quote! { #name => Some(WppFlag::#ident(#ctrl_idx, #flag_idx)) }
    }).collect();

    let static_entries: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
        let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
        quote! { WppFlag::#ident(#ctrl_idx, #flag_idx) }
    }).collect();

    let control_idx_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { WppFlag::#ident(ctrl, _) => *ctrl }
    }).collect();

    let flag_idx_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { WppFlag::#ident(_, flag) => *flag }
    }).collect();

    let tuple_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { WppFlag::#ident(ctrl, flag) => (*ctrl, *flag) }
    }).collect();

    let num_flags = variants.len();

    quote! {
        #[allow(missing_docs)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u8)]
        pub enum TraceLevel {
            None = 0,
            Critical = 1,
            Error = 2,
            Warning = 3,
            Information = 4,
            Verbose = 5,
            Reserved6 = 6,
            Reserved7 = 7,
            Reserved8 = 8,
            Reserved9 = 9,
        }

        impl TraceLevel {
            #[inline]
            pub const fn value(&self) -> u8 { *self as u8 }

            #[inline]
            pub const fn is_verbose_or_below(&self) -> bool {
                (*self as u8) <= (TraceLevel::Verbose as u8)
            }
        }

        #[allow(missing_docs)]
        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(C)]
        pub enum WppFlag {
            #(#enum_variants),*
        }

        impl WppFlag {
            #[inline]
            pub const fn control_index(&self) -> usize {
                match self { #(#control_idx_arms),* }
            }

            #[inline]
            pub const fn flag_index(&self) -> usize {
                match self { #(#flag_idx_arms),* }
            }

            #[inline]
            pub const fn as_tuple(&self) -> (usize, usize) {
                match self { #(#tuple_arms),* }
            }

            #[inline]
            pub fn by_name(name: &str) -> Option<WppFlag> {
                match name {
                    #(#name_match_arms,)*
                    _ => None
                }
            }

            #[inline]
            pub const fn all() -> &'static [WppFlag] {
                &[#(#static_entries),*]
            }

            #[inline]
            pub const fn count() -> usize { #num_flags }
        }
    }
}

/// Core implementation of the `wpp_init!` proc macro.
pub(crate) fn wpp_init_impl(item: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(item as TraceControlArgs);
    let trace_controls = parsed.controls;

    if let Err((flag, guid1, guid2)) = validate_no_duplicate_flags(&trace_controls) {
        let err = Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "Duplicate flag '{}' found in trace controls. First in GUID '{}', duplicate in GUID '{}'",
                flag, guid1, guid2
            )
        );
        return err.to_compile_error().into();
    }

    let codeview_annotations: Vec<proc_macro2::TokenStream> = trace_controls.iter().map(|tc| {
        let guid = &tc.guid;
        let provider_name = &tc.provider_name;
        let flags = &tc.flags;
        quote! {
            core::hint::codeview_annotation!("TMC:", #guid, #provider_name, #(#flags),*);
        }
    }).collect();

    let wpp_flags_declaration = generate_wpp_flags_declaration(&trace_controls);

    let wpp_flag_len: usize = trace_controls.iter()
        .map(|tc| (tc.flags.len() + 31) / 32)
        .max()
        .unwrap_or(0)
        .max(1);
    let wpp_flag_len_lit = proc_macro2::Literal::usize_unsuffixed(wpp_flag_len);

    let num_controls = trace_controls.len();
    let num_controls_lit = proc_macro2::Literal::usize_unsuffixed(num_controls);

    let guid_statics: Vec<proc_macro2::TokenStream> = trace_controls.iter().enumerate().map(|(idx, tc)| {
        let static_name = Ident::new(&format!("__WPP_CTLGUID_{}", idx), proc_macro2::Span::call_site());
        let guid_literal = guid_str_to_tokens(&tc.guid);
        quote! {
            static #static_name: ::wdf::__internal::GUID = #guid_literal;
        }
    }).collect();

    let init_statements: Vec<proc_macro2::TokenStream> = (0..num_controls).map(|idx| {
        let idx_lit = proc_macro2::Literal::usize_unsuffixed(idx);
        let guid_static_name = Ident::new(&format!("__WPP_CTLGUID_{}", idx), proc_macro2::Span::call_site());

        let next_expr = if idx + 1 < num_controls {
            let next_idx = proc_macro2::Literal::usize_unsuffixed(idx + 1);
            quote! {
                base.add(#next_idx).cast::<::wdf::tracing::WPP_TRACE_CONTROL_BLOCK>()
                    as *const ::wdf::tracing::WPP_TRACE_CONTROL_BLOCK
            }
        } else {
            quote! { core::ptr::null() }
        };

        quote! {
            {
                let control = unsafe { &mut *arr[#idx_lit].Control };
                control.ControlGuid = &#guid_static_name as *const ::wdf::__internal::GUID;
                control.Next = unsafe { #next_expr };
            }
        }
    }).collect();

    let expanded = quote! {
        #wpp_flags_declaration

        const WPP_FLAG_LEN: usize = #wpp_flag_len_lit;
        const WPP_LAST_CTL: usize = #num_controls_lit;

        #(#guid_statics)*

        #[repr(C)]
        union WppCbType {
            Control: core::mem::ManuallyDrop<::wdf::tracing::WPP_TRACE_CONTROL_BLOCK>,
            ReserveSpace: [u8;
                core::mem::size_of::<::wdf::tracing::WPP_TRACE_CONTROL_BLOCK>()
                    + core::mem::size_of::<u32>() * (WPP_FLAG_LEN - 1)],
        }

        #[repr(transparent)]
        struct WppControlBlockArray(core::cell::UnsafeCell<[WppCbType; WPP_LAST_CTL]>);

        unsafe impl Sync for WppControlBlockArray {}

        impl WppControlBlockArray {
            const fn new() -> Self {
                const INIT: WppCbType = WppCbType {
                    Control: core::mem::ManuallyDrop::new(
                        ::wdf::tracing::WPP_TRACE_CONTROL_BLOCK::new(WPP_FLAG_LEN as u8)
                    ),
                };
                Self(core::cell::UnsafeCell::new([INIT; WPP_LAST_CTL]))
            }

            fn as_mut_ptr(&self) -> *mut [WppCbType; WPP_LAST_CTL] {
                self.0.get()
            }
        }

        static WPP_MAIN_CB: WppControlBlockArray = WppControlBlockArray::new();

        fn wpp_init_control_array() {
            let arr: &mut [WppCbType; WPP_LAST_CTL] = unsafe { &mut *WPP_MAIN_CB.as_mut_ptr() };
            let base: *mut WppCbType = arr.as_mut_ptr();
            #(#init_statements)*
        }

        fn __wpp_driver_init() -> (*mut ::wdf::tracing::WPP_PROJECT_CONTROL_BLOCK, usize) {
            #(#codeview_annotations)*
            wpp_init_control_array();
            let ptr = unsafe { (*WPP_MAIN_CB.as_mut_ptr()).as_mut_ptr().cast::<::wdf::tracing::WPP_PROJECT_CONTROL_BLOCK>() };
            (ptr, WPP_LAST_CTL)
        }
    };

    expanded.into()
}
