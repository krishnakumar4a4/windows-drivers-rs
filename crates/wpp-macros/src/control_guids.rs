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
    let mut output = TokenStream::new();

    validate_unique_keywords(&parsed.providers)?;

    for (idx, provider) in parsed.providers.iter().enumerate() {
        output.extend(generate_provider_module(provider, idx));
    }
    output.extend(generate_wpp_flag_enum(&parsed.providers));
    output.extend(generate_unified_trace_macro(&parsed.providers));
    output.extend(generate_level_macro());
    output.extend(generate_ifr_init(&parsed.providers));
    Ok(output)
}

fn generate_provider_module(p: &ProviderDecl, idx: usize) -> TokenStream {
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

    let cb_index_lit = proc_macro2::Literal::usize_unsuffixed(idx);

    quote! {
        #[allow(non_snake_case)]
        pub mod #mod_name {
            pub const CONTROL_GUID: ::wpp::GUID = ::wpp::GUID {
                data1: #d1, data2: #d2, data3: #d3, data4: [#(#d4_tokens),*],
            };
            pub const DECODE_GUID: ::wpp::GUID = ::wpp::GUID {
                data1: #dd1, data2: #dd2, data3: #dd3, data4: [#(#dd4_tokens),*],
            };
            /// Index of this provider's control block in the IFR CB array.
            pub const CB_INDEX: usize = #cb_index_lit;
            #(#kw_consts)*
            #[doc(hidden)]
            pub const __WPP_NO_KEYWORD: u64 = 0;
            pub static STATE: ::wpp::ProviderState = ::wpp::ProviderState::new();

            /// Initializes this provider: emits PDB annotation, registers with
            /// ETW, and sets the control GUID on its IFR control block.
            ///
            /// # Safety
            ///
            /// `__wpp_ifr_create_cbs()` must have been called before this so
            /// that the control block array exists.
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
                // Set the control GUID on this provider's IFR control block
                unsafe {
                    let __cb = super::__wpp_get_cb(CB_INDEX);
                    (*__cb).ControlGuid = &CONTROL_GUID as *const ::wpp::GUID;
                }
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

fn validate_unique_keywords(providers: &[ProviderDecl]) -> Result<()> {
    let mut seen = std::collections::HashMap::<String, String>::new();
    for p in providers {
        for kw in &p.keywords {
            let name = kw.name.to_string();
            if let Some(prev_provider) = seen.insert(name.clone(), p.name.to_string()) {
                return Err(syn::Error::new(
                    kw.name.span(),
                    format!(
                        "keyword '{}' is defined in both '{}' and '{}'; \
                         keywords must be unique across all providers",
                        name, prev_provider, p.name
                    ),
                ));
            }
        }
    }
    Ok(())
}

/// Generates the `TraceLevel` and `WppFlag` enums.
///
/// `WppFlag` follows the reference implementation: each variant carries
/// `(control_idx, flag_idx)` and provides `control_index()`, `flag_index()`,
/// `as_tuple()`, `by_name()`, `all()`, `count()`.
fn generate_wpp_flag_enum(providers: &[ProviderDecl]) -> TokenStream {
    let has_flags = providers.iter().any(|p| !p.keywords.is_empty());

    struct VariantInfo {
        variant_ident: Ident,
        flag_name: String,
        control_idx: usize,
        flag_idx: usize,
    }

    let variants: Vec<VariantInfo> = providers
        .iter()
        .enumerate()
        .flat_map(|(control_idx, p)| {
            p.keywords
                .iter()
                .enumerate()
                .map(move |(flag_idx, kw)| VariantInfo {
                    variant_ident: kw.name.clone(),
                    flag_name: kw.name.to_string(),
                    control_idx,
                    flag_idx,
                })
        })
        .collect();

    let enum_variants: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { #ident(usize, usize) }
        })
        .collect();

    let name_match_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            let name = &v.flag_name;
            let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
            let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
            quote! { #name => Some(WppFlag::#ident(#ctrl_idx, #flag_idx)) }
        })
        .collect();

    let static_entries: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
            let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
            quote! { WppFlag::#ident(#ctrl_idx, #flag_idx) }
        })
        .collect();

    let control_idx_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { WppFlag::#ident(ctrl, _) => *ctrl }
        })
        .collect();

    let flag_idx_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { WppFlag::#ident(_, flag) => *flag }
        })
        .collect();

    let tuple_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { WppFlag::#ident(ctrl, flag) => (*ctrl, *flag) }
        })
        .collect();

    let num_flags = variants.len();

    // TraceLevel is always generated; WppFlag only when flags exist
    let wpp_flag_block = if has_flags {
        quote! {
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
    } else {
        quote! {}
    };

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

        #wpp_flag_block
    }
}

/// Generates a single unified `trace!` macro with one arm per keyword.
///
/// Each keyword is matched literally and routes to the correct provider.
/// A default arm (no keyword) routes to the first provider with keyword=0.
fn generate_unified_trace_macro(providers: &[ProviderDecl]) -> TokenStream {
    let dollar = proc_macro2::Punct::new('$', proc_macro2::Spacing::Alone);

    // Build one arm per keyword across all providers
    let mut arms: Vec<TokenStream> = Vec::new();
    for provider in providers {
        let mod_name = &provider.name;
        let provider_name_str = provider.name.to_string();
        let guid_str = &provider.guid_str;

        for kw in &provider.keywords {
            let kw_name = &kw.name;
            arms.push(quote! {
                (#dollar level:ident, #kw_name, #dollar fmt:literal #dollar(, #dollar arg:expr)*) => {{
                    const __WPP_LEVEL: u8 = __wpp_level_to_u8!(#dollar level);
                    let __wpp_kw_val: u64 = #dollar crate::#mod_name::#kw_name;
                    if #dollar crate::#mod_name::STATE.is_enabled(__WPP_LEVEL, __wpp_kw_val) {
                        ::wpp::__wpp_trace_impl!(
                            @provider_mod #dollar crate::#mod_name,
                            @provider_name #provider_name_str,
                            @guid #guid_str,
                            @level __WPP_LEVEL,
                            @level_name #dollar level,
                            @keyword #kw_name,
                            @fmt #dollar fmt
                            #dollar(, #dollar arg)*
                        );
                    }
                }};
            });
        }
    }

    // Default arm: no keyword → first provider, keyword = 0
    if let Some(first) = providers.first() {
        let mod_name = &first.name;
        let provider_name_str = first.name.to_string();
        let guid_str = &first.guid_str;

        arms.push(quote! {
            (#dollar level:ident, #dollar fmt:literal #dollar(, #dollar arg:expr)*) => {{
                const __WPP_LEVEL: u8 = __wpp_level_to_u8!(#dollar level);
                if #dollar crate::#mod_name::STATE.is_enabled(__WPP_LEVEL, 0) {
                    ::wpp::__wpp_trace_impl!(
                        @provider_mod #dollar crate::#mod_name,
                        @provider_name #provider_name_str,
                        @guid #guid_str,
                        @level __WPP_LEVEL,
                        @level_name #dollar level,
                        @keyword __WPP_NO_KEYWORD,
                        @fmt #dollar fmt
                        #dollar(, #dollar arg)*
                    );
                }
            }};
        });
    }

    quote! {
        macro_rules! trace {
            #(#arms)*
        }
    }
}

fn generate_level_macro() -> TokenStream {
    quote! {
        macro_rules! __wpp_level_to_u8 {
            (CRITICAL) => { 1u8 };
            (ERROR)    => { 2u8 };
            (WARNING)  => { 3u8 };
            (INFO)     => { 4u8 };
            (VERBOSE)  => { 5u8 };
        }
    }
}

/// Generates the IFR infrastructure:
///
/// - `WPP_GLOBAL_Control` / `WPP_RECORDER_INITIALIZED` (`#[no_mangle]` statics)
/// - `__wpp_ifr_create_cbs()` — phase (a): creates CB array, links Next ptrs,
///   emits TMC annotations, returns `(ptr, count)`
/// - `__wpp_get_cb(idx)` — accessor into the CB array
/// - `__wpp_ifr_start(...)` — phase (b): calls `wpp::ifr::start_ifr()`
/// - `__wpp_ifr_cleanup()` — calls `wpp::ifr::stop_ifr()`
fn generate_ifr_init(providers: &[ProviderDecl]) -> TokenStream {
    let num_controls = providers.len();
    let num_controls_lit = proc_macro2::Literal::usize_unsuffixed(num_controls);

    // WPP_FLAG_LEN: max number of 32-bit flag words needed
    let wpp_flag_len: usize = providers
        .iter()
        .map(|p| (p.keywords.len() + 31) / 32)
        .max()
        .unwrap_or(0)
        .max(1);
    let wpp_flag_len_lit = proc_macro2::Literal::usize_unsuffixed(wpp_flag_len);

    // TMC codeview annotations for each provider
    let codeview_annotations: Vec<TokenStream> = providers
        .iter()
        .map(|p| {
            let guid = &p.guid_str;
            let provider_name = p.name.to_string();
            let flags: Vec<String> = p.keywords.iter().map(|kw| kw.name.to_string()).collect();
            quote! {
                core::hint::codeview_annotation!("TMC:", #guid, #provider_name, #(#flags),*);
            }
        })
        .collect();

    // Link Next pointers (but NOT ControlGuid — that's per-provider now)
    let link_statements: Vec<TokenStream> = (0..num_controls)
        .map(|idx| {
            let idx_lit = proc_macro2::Literal::usize_unsuffixed(idx);
            let next_expr = if idx + 1 < num_controls {
                let next_idx = proc_macro2::Literal::usize_unsuffixed(idx + 1);
                quote! {
                    __wpp_base.add(#next_idx)
                        .cast::<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>()
                        as *const ::wpp::ifr::WPP_TRACE_CONTROL_BLOCK
                }
            } else {
                quote! { core::ptr::null() }
            };
            quote! {
                {
                    let control = unsafe { &mut *__wpp_arr[#idx_lit].Control };
                    control.Next = unsafe { #next_expr };
                }
            }
        })
        .collect();

    quote! {
        // IFR globals — must live in the consuming crate (not in wpp lib crate)
        // to avoid LTO bitcode errors with #[no_mangle] statics.
        #[unsafe(no_mangle)]
        static mut WPP_GLOBAL_Control: *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK =
            core::ptr::null_mut();
        #[unsafe(no_mangle)]
        static mut WPP_RECORDER_INITIALIZED: *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK =
            core::ptr::null_mut();

        // ── IFR control block array (module-level) ──────────────────────────

        const __WPP_FLAG_LEN: usize = #wpp_flag_len_lit;
        const __WPP_CONTROLS_COUNT: usize = #num_controls_lit;

        #[repr(C)]
        union __WppCbType {
            Control: core::mem::ManuallyDrop<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>,
            ReserveSpace: [u8;
                core::mem::size_of::<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>()
                    + core::mem::size_of::<u32>() * (__WPP_FLAG_LEN - 1)],
        }

        #[repr(transparent)]
        struct __WppControlBlockArray(
            core::cell::UnsafeCell<[__WppCbType; __WPP_CONTROLS_COUNT]>,
        );

        unsafe impl Sync for __WppControlBlockArray {}

        impl __WppControlBlockArray {
            const fn new() -> Self {
                const INIT: __WppCbType = __WppCbType {
                    Control: core::mem::ManuallyDrop::new(
                        ::wpp::ifr::WPP_TRACE_CONTROL_BLOCK::new(
                            #wpp_flag_len_lit as u8,
                        ),
                    ),
                };
                Self(core::cell::UnsafeCell::new([INIT; __WPP_CONTROLS_COUNT]))
            }
            fn as_mut_ptr(&self) -> *mut [__WppCbType; __WPP_CONTROLS_COUNT] {
                self.0.get()
            }
        }

        static __WPP_MAIN_CB: __WppControlBlockArray =
            __WppControlBlockArray::new();

        // ── IFR functions ───────────────────────────────────────────────────

        /// Phase (a): link Next pointers in the control block array,
        /// emit TMC codeview annotations. Returns `(ptr, count)`.
        ///
        /// Does NOT set `ControlGuid` — each provider's `init()` does that.
        #[doc(hidden)]
        fn __wpp_ifr_create_cbs() -> (
            *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK,
            usize,
        ) {
            // Emit TMC codeview annotations
            #(#codeview_annotations)*

            // Link control block Next pointers
            let __wpp_arr: &mut [__WppCbType; __WPP_CONTROLS_COUNT] =
                unsafe { &mut *__WPP_MAIN_CB.as_mut_ptr() };
            let __wpp_base: *mut __WppCbType = __wpp_arr.as_mut_ptr();
            #(#link_statements)*

            let ptr = unsafe {
                (*__WPP_MAIN_CB.as_mut_ptr())
                    .as_mut_ptr()
                    .cast::<::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK>()
            };
            (ptr, __WPP_CONTROLS_COUNT)
        }

        /// Returns a mutable pointer to the control block at `idx`.
        #[doc(hidden)]
        #[inline]
        fn __wpp_get_cb(idx: usize) -> *mut ::wpp::ifr::WPP_TRACE_CONTROL_BLOCK {
            unsafe {
                (*__WPP_MAIN_CB.as_mut_ptr())
                    .as_mut_ptr()           // *mut __WppCbType
                    .add(idx)               // strides by size_of::<__WppCbType>()
                    .cast::<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>()
            }
        }

        /// Phase (b): start IFR auto-log recording.
        #[doc(hidden)]
        fn __wpp_ifr_start(
            cb_ptr: *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK,
            driver_obj: *mut core::ffi::c_void,
            reg_path: *const core::ffi::c_void,
            provider_states: &[&::wpp::ProviderState],
        ) {
            unsafe {
                ::wpp::ifr::start_ifr(
                    cb_ptr,
                    driver_obj,
                    reg_path,
                    provider_states,
                    &raw mut WPP_GLOBAL_Control,
                    &raw mut WPP_RECORDER_INITIALIZED,
                );
            }
        }

        /// Cleans up IFR tracing.
        #[doc(hidden)]
        fn __wpp_ifr_cleanup() {
            unsafe {
                ::wpp::ifr::stop_ifr(
                    &raw mut WPP_GLOBAL_Control,
                    &raw mut WPP_RECORDER_INITIALIZED,
                );
            }
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
