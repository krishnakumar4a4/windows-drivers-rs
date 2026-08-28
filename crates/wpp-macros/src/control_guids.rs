// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of `wpp_control_guids!` proc macro.

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident,
    LitInt,
    Result,
    Token,
    braced,
    parse::{Parse, ParseStream},
};

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
        let guid_parts =
            parse_guid_parts(&guid_str).map_err(|e| input.error(format!("invalid GUID: {}", e)))?;

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
            keywords.push(KeywordDecl {
                name: kw_name,
                bit_position: bit_pos,
            });
            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }

        Ok(ProviderDecl {
            name,
            guid_str,
            guid_parts,
            keywords,
        })
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

    output.extend(generate_control_guid_array(&parsed.providers));
    for (idx, provider) in parsed.providers.iter().enumerate() {
        output.extend(generate_provider_module(provider, idx));
    }
    output.extend(generate_wpp_flag_enum(&parsed.providers));
    output.extend(generate_unified_trace_macro(&parsed.providers));
    output.extend(generate_level_macro());
    output.extend(generate_ifr_module(&parsed.providers));
    Ok(output)
}

/// Generates a global static array of control GUIDs for all providers.
///
/// Each provider module references its GUID via
/// `super::WPP_CONTROL_GUIDS[CB_INDEX]`.
fn generate_control_guid_array(providers: &[ProviderDecl]) -> TokenStream {
    let num_providers = providers.len();
    let num_providers_lit = proc_macro2::Literal::usize_unsuffixed(num_providers);

    let guid_elements: Vec<TokenStream> = providers
        .iter()
        .map(|p| {
            let gp = &p.guid_parts;
            let (d1, d2, d3) = (gp.d1, gp.d2, gp.d3);
            let d4_tokens: Vec<TokenStream> = gp
                .d4
                .iter()
                .map(|b| {
                    let b = *b;
                    quote!(#b)
                })
                .collect();
            quote! {
                ::wpp::GUID { data1: #d1, data2: #d2, data3: #d3, data4: [#(#d4_tokens),*] }
            }
        })
        .collect();

    quote! {
        /// Global static array of control GUIDs for all providers.
        static WPP_CONTROL_GUIDS: [::wpp::GUID; #num_providers_lit] = [
            #(#guid_elements),*
        ];
    }
}

fn generate_provider_module(p: &ProviderDecl, idx: usize) -> TokenStream {
    let mod_name = &p.name;

    let kw_consts: Vec<TokenStream> = p
        .keywords
        .iter()
        .map(|kw| {
            let name = &kw.name;
            let value = 1u64 << kw.bit_position;
            quote! { #[allow(non_upper_case_globals)] pub const #name: u64 = #value; }
        })
        .collect();

    let provider_name_str = p.name.to_string();
    let guid_str = &p.guid_str;
    // TMC annotation lists each flag by name only (no bit suffix).
    let kw_annotation_strings: Vec<String> =
        p.keywords.iter().map(|kw| kw.name.to_string()).collect();

    let cb_index_lit = proc_macro2::Literal::usize_unsuffixed(idx);

    quote! {
        #[allow(non_snake_case)]
        pub mod #mod_name {
            /// Reference to this provider's control GUID in the global array.
            #[inline]
            pub fn control_guid() -> &'static ::wpp::GUID {
                &super::WPP_CONTROL_GUIDS[CB_INDEX]
            }
            /// Index of this provider's control block in the IFR CB array.
            pub const CB_INDEX: usize = #cb_index_lit;
            #(#kw_consts)*
            #[doc(hidden)]
            pub const __WPP_NO_KEYWORD: u64 = 0;
            pub static STATE: ::wpp::ProviderState = ::wpp::ProviderState::new();

            #[doc(hidden)]
            pub fn __wpp_ifr_descriptor() -> ::wpp::ifr::ProviderDescriptor {
                ::wpp::ifr::ProviderDescriptor {
                    owner_id: super::__wpp_ifr::owner_id(),
                    prepare: super::__wpp_ifr::prepare,
                    init,
                    clean_up,
                }
            }

            /// Initializes this provider: emits PDB annotation and registers
            /// with ETW.
            ///
            /// # Safety
            ///
            /// Driver-wide IFR initialization must have completed before this
            /// provider emits traces.
            /// The caller must ensure `clean_up()` is called before the
            /// module containing this provider is unloaded.
            pub unsafe fn init() {
                // Emit this provider's `TMC:` control-GUID annotation (control
                // GUID -> provider name + flag names). It is keyed under the
                // control GUID, which under Option B is distinct from the
                // per-module decode GUIDs that key the TMF records, so TMC and
                // TMF annotations never share a GUID and cannot collide.
                core::hint::codeview_annotation!(
                    "TMC:", #guid_str, #provider_name_str,
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
                    control_guid(),
                    Some(::wpp::provider::enable_callback),
                    &STATE as *const ::wpp::ProviderState as *mut core::ffi::c_void,
                ) };
                STATE.reg_handle.store(handle, core::sync::atomic::Ordering::Relaxed);
                // ModernWpp (WPPv3): mark this Crimson provider as a WPP
                // trace-message provider so the kernel stamps the WPP header
                // flag and tracks the PDB DebugId for TMF decoding, and opt in
                // to descriptor typing so the per-module decode-GUID descriptor
                // (Option B) is honoured. Best-effort — ignored on kernels
                // without the feature. Enablement stays keyed on this control
                // GUID; each event carries its module's decode GUID separately.
                let __wpp_modern_status = unsafe { ::wpp::etw::enable_modern_wpp(handle) };
                ::wdf::println!(
                    "enable_modern_wpp[{}]: status={:#010x} -> {}",
                    #provider_name_str,
                    __wpp_modern_status,
                    if __wpp_modern_status == 0 {
                        "ModernWpp enabled"
                    } else {
                        "ModernWpp NOT available on this machine"
                    }
                );
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
                        "keyword '{}' is defined in both '{}' and '{}'; keywords must be unique \
                         across all providers",
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
                    // __wpp_trace_impl handles ETW (gated by is_enabled)
                    // and IFR (gated by IFR state auto_log_context) independently.
                    ::wpp::__wpp_trace_impl!(
                        @provider_mod #dollar crate::#mod_name,
                        @ifr_state ::wpp::ifr::GLOBAL_STATE,
                        @provider_name #provider_name_str,
                        @guid #guid_str,
                        @level __WPP_LEVEL,
                        @level_name #dollar level,
                        @keyword #kw_name,
                        @fmt #dollar fmt
                        #dollar(, #dollar arg)*
                    );
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
                ::wpp::__wpp_trace_impl!(
                    @provider_mod #dollar crate::#mod_name,
                    @ifr_state ::wpp::ifr::GLOBAL_STATE,
                    @provider_name #provider_name_str,
                    @guid #guid_str,
                    @level __WPP_LEVEL,
                    @level_name #dollar level,
                    @keyword __WPP_NO_KEYWORD,
                    @fmt #dollar fmt
                    #dollar(, #dollar arg)*
                );
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

/// Generates the `__wpp_ifr` module containing all IFR infrastructure:
///
/// - Control block array and helper types
/// - `prepare()` — links the crate's CBs, sets their control GUIDs, and returns
///   the chain bounds for aggregation by `driver_entry`
fn generate_ifr_module(providers: &[ProviderDecl]) -> TokenStream {
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
    // let codeview_annotations: Vec<TokenStream> = providers
    //     .iter()
    //     .map(|p| {
    //         let guid = &p.guid_str;
    //         let provider_name = p.name.to_string();
    //         let flags: Vec<String> = p.keywords.iter().map(|kw|
    // kw.name.to_string()).collect();         quote! {
    //             core::hint::codeview_annotation!("TMC:", #guid, #provider_name,
    // #(#flags),*);         }
    //     })
    //     .collect();

    // Link Next pointers between control blocks
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

    // Set ControlGuid on each CB from the global WPP_CONTROL_GUIDS array
    let set_guid_statements: Vec<TokenStream> = (0..num_controls)
        .map(|idx| {
            let idx_lit = proc_macro2::Literal::usize_unsuffixed(idx);
            quote! {
                unsafe {
                    (*get_cb(#idx_lit)).ControlGuid =
                        &super::WPP_CONTROL_GUIDS[#idx_lit] as *const ::wpp::GUID;
                }
            }
        })
        .collect();

    quote! {
        /// IFR (In-Flight Recorder) module — prepares this crate's control
        /// blocks for the driver-wide IFR lifecycle.
        #[allow(non_snake_case)]
        pub mod __wpp_ifr {
            // ── Control block array types ────────────────────────────────────

            const WPP_FLAG_LEN: usize = #wpp_flag_len_lit;
            const CONTROLS_COUNT: usize = #num_controls_lit;

            #[repr(C)]
            union CbType {
                Control: core::mem::ManuallyDrop<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>,
                ReserveSpace: [u8;
                    core::mem::size_of::<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>()
                        + core::mem::size_of::<u32>() * (WPP_FLAG_LEN - 1)],
            }

            #[repr(transparent)]
            struct ControlBlockArray(
                core::cell::UnsafeCell<[CbType; CONTROLS_COUNT]>,
            );

            unsafe impl Sync for ControlBlockArray {}

            impl ControlBlockArray {
                const fn new() -> Self {
                    const INIT: CbType = CbType {
                        Control: core::mem::ManuallyDrop::new(
                            ::wpp::ifr::WPP_TRACE_CONTROL_BLOCK::new(
                                #wpp_flag_len_lit as u8,
                            ),
                        ),
                    };
                    Self(core::cell::UnsafeCell::new([INIT; CONTROLS_COUNT]))
                }
                fn as_mut_ptr(&self) -> *mut [CbType; CONTROLS_COUNT] {
                    self.0.get()
                }
            }

            static MAIN_CB: ControlBlockArray = ControlBlockArray::new();
            static OWNER_ID: u8 = 0;

            #[doc(hidden)]
            #[inline]
            pub fn owner_id() -> *const () {
                (&OWNER_ID as *const u8).cast::<()>()
            }

            /// Returns a mutable pointer to the control block at `idx`.
            #[inline]
            fn get_cb(idx: usize) -> *mut ::wpp::ifr::WPP_TRACE_CONTROL_BLOCK {
                unsafe {
                    (*MAIN_CB.as_mut_ptr())
                        .as_mut_ptr()
                        .add(idx)
                        .cast::<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>()
                }
            }

            /// Prepares this crate's IFR control-block chain for the driver.
            ///
            /// # Safety
            ///
            /// Must only be called once during `DriverEntry`, before IFR starts.
            #[doc(hidden)]
            #[inline(never)]
            pub unsafe fn prepare() -> (
                *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK,
                *mut ::wpp::ifr::WPP_TRACE_CONTROL_BLOCK,
            ) {
                // Link control block Next pointers
                let __wpp_arr: &mut [CbType; CONTROLS_COUNT] =
                    unsafe { &mut *MAIN_CB.as_mut_ptr() };
                let __wpp_base: *mut CbType = __wpp_arr.as_mut_ptr();
                #(#link_statements)*

                // Set ControlGuid on each CB from the global GUID array
                #(#set_guid_statements)*

                let head = unsafe {
                    (*MAIN_CB.as_mut_ptr())
                        .as_mut_ptr()
                        .cast::<::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK>()
                };
                let tail = get_cb(CONTROLS_COUNT - 1);
                (head, tail)
            }

            #[doc(hidden)]
            pub unsafe fn init_driver(
                driver_obj: *mut core::ffi::c_void,
                reg_path: *const core::ffi::c_void,
                providers: &[::wpp::ifr::ProviderDescriptor],
                global_control: *mut *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK,
                recorder_initialized: *mut *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK,
            ) {
                if ::wpp::ifr::GLOBAL_STATE.init_state.compare_exchange(
                    ::wpp::ifr::UNINITIALIZED,
                    ::wpp::ifr::INITIALIZING,
                    core::sync::atomic::Ordering::Acquire,
                    core::sync::atomic::Ordering::Relaxed,
                ).is_ok() {
                    let mut __wpp_control_head:
                        *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK = core::ptr::null_mut();
                    let mut __wpp_control_tail:
                        *mut ::wpp::ifr::WPP_TRACE_CONTROL_BLOCK = core::ptr::null_mut();

                    let mut __wpp_provider_index = 0;
                    while __wpp_provider_index < providers.len() {
                        let __wpp_provider = providers[__wpp_provider_index];
                        let mut __wpp_seen_owner = false;
                        let mut __wpp_previous_index = 0;
                        while __wpp_previous_index < __wpp_provider_index {
                            if providers[__wpp_previous_index].owner_id == __wpp_provider.owner_id {
                                __wpp_seen_owner = true;
                                break;
                            }
                            __wpp_previous_index += 1;
                        }

                        if !__wpp_seen_owner {
                            let (__wpp_chain_head, __wpp_chain_tail) =
                                unsafe { (__wpp_provider.prepare)() };
                            if __wpp_control_head.is_null() {
                                __wpp_control_head = __wpp_chain_head;
                            } else {
                                unsafe {
                                    (*__wpp_control_tail).Next =
                                        __wpp_chain_head.cast::<::wpp::ifr::WPP_TRACE_CONTROL_BLOCK>();
                                }
                            }
                            __wpp_control_tail = __wpp_chain_tail;
                        }

                        __wpp_provider_index += 1;
                    }

                    unsafe {
                        ::wpp::ifr::start_ifr(
                            __wpp_control_head,
                            driver_obj,
                            reg_path,
                            &::wpp::ifr::GLOBAL_STATE,
                            global_control,
                            recorder_initialized,
                        );
                    }
                    ::wpp::ifr::GLOBAL_STATE.init_state.store(
                        ::wpp::ifr::INITIALIZED,
                        core::sync::atomic::Ordering::Release,
                    );
                }

                let mut __wpp_provider_index = 0;
                while __wpp_provider_index < providers.len() {
                    unsafe { (providers[__wpp_provider_index].init)() };
                    __wpp_provider_index += 1;
                }
            }

            #[doc(hidden)]
            pub fn clean_up_driver(
                providers: &[::wpp::ifr::ProviderDescriptor],
                global_control: *mut *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK,
                recorder_initialized: *mut *mut ::wpp::ifr::WPP_PROJECT_CONTROL_BLOCK,
            ) {
                let mut __wpp_provider_index = providers.len();
                while __wpp_provider_index > 0 {
                    __wpp_provider_index -= 1;
                    (providers[__wpp_provider_index].clean_up)();
                }

                if ::wpp::ifr::GLOBAL_STATE.init_state.compare_exchange(
                    ::wpp::ifr::INITIALIZED,
                    ::wpp::ifr::STOPPING,
                    core::sync::atomic::Ordering::AcqRel,
                    core::sync::atomic::Ordering::Relaxed,
                ).is_ok() {
                    unsafe {
                        ::wpp::ifr::stop_ifr(
                            &::wpp::ifr::GLOBAL_STATE,
                            global_control,
                            recorder_initialized,
                        );
                    }
                    ::wpp::ifr::GLOBAL_STATE.init_state.store(
                        ::wpp::ifr::UNINITIALIZED,
                        core::sync::atomic::Ordering::Release,
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guid_parsing() {
        let gp = parse_guid_parts("84bdb2e9-829e-41b3-b891-02f454bc2bd7").unwrap();
        assert_eq!(gp.d1, 0x84BDB2E9);
        assert_eq!(gp.d2, 0x829E);
        assert_eq!(gp.d3, 0x41B3);
    }

    #[test]
    fn ifr_module_prepares_without_owning_driver_wide_globals() {
        let input: ControlGuidsInput = syn::parse2(quote! {
            TestProvider 84bdb2e9-829e-41b3-b891-02f454bc2bd7 {
                TRACE_FLAG
            }
        })
        .unwrap();

        let generated = generate_ifr_module(&input.providers).to_string();
        let all_generated = generate(quote! {
            TestProvider 84bdb2e9-829e-41b3-b891-02f454bc2bd7 {
                TRACE_FLAG
            }
        })
        .unwrap()
        .to_string();

        assert!(all_generated.contains("pub fn __wpp_ifr_descriptor"));
        assert!(generated.contains("pub mod __wpp_ifr"));
        assert!(generated.contains("pub unsafe fn prepare"));
        assert!(generated.contains("pub unsafe fn init_driver"));
        assert!(generated.contains("pub fn clean_up_driver"));
        assert!(!generated.contains("pub static mut WPP_GLOBAL_Control"));
        assert!(!generated.contains("pub static mut WPP_RECORDER_INITIALIZED"));
    }
}
