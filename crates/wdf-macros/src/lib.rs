// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! A collection of macros used for writing WDF-based drivers in safe Rust

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Error,
    Ident,
    ItemFn,
    ItemImpl,
    ItemStruct,
    Lit,
    Token,
    meta::parser,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
};

/// A procedural macro that when placed on a safe Rust impl of a driver
/// generates the relevant FFI wrappers
#[proc_macro_attribute]
pub fn driver_impl(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_clone = input.clone();
    let item_impl = parse_macro_input!(input_clone as ItemImpl);

    let driver_ty = item_impl.self_ty;

    let mut wrappers: TokenStream = quote! {
        use wdf::{nt_success, println, WdkAllocator};
        use wdf::ffi::{
            ntddk::KeGetCurrentIrql,
            APC_LEVEL,
            DRIVER_OBJECT,
            NTSTATUS,
            PCUNICODE_STRING,
            PDRIVER_OBJECT,
            PWDFDEVICE_INIT,
            STATUS_SUCCESS,
            ULONG,
            UNICODE_STRING,
            WDFDRIVER,
            WDFOBJECT,
            WDFSTRING,
            WDFDEVICE,
            WDFDEVICE_INIT,
            WDF_DRIVER_CONFIG,
            WDF_DRIVER_VERSION_AVAILABLE_PARAMS,
            WDF_NO_HANDLE,
            WDF_NO_OBJECT_ATTRIBUTES,
        };

        extern crate alloc;

        use alloc::{slice, string::String};

        #[global_allocator]
        static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;

        static SAFE_DRIVER: #driver_ty = #driver_ty;

        #[link_section = "INIT"]
        #[export_name = "DriverEntry"] // WDF expects a symbol with the name DriverEntry
        extern "system" fn driver_entry(driver: &mut DRIVER_OBJECT, registry_path: PCUNICODE_STRING,) -> NTSTATUS {
            driver.DriverUnload = Some(driver_exit);

            let mut driver_config = WDF_DRIVER_CONFIG {
                Size: core::mem::size_of::<WDF_DRIVER_CONFIG>() as ULONG,
                EvtDriverDeviceAdd: Some(evt_driver_device_add),
                ..WDF_DRIVER_CONFIG::default()
            };

            let driver_handle_output = WDF_NO_HANDLE.cast::<WDFDRIVER>();

            let nt_status = unsafe {
                wdk_sys::call_unsafe_wdf_function_binding!(
                    WdfDriverCreate,
                    driver as PDRIVER_OBJECT,
                    registry_path,
                    WDF_NO_OBJECT_ATTRIBUTES,
                    &mut driver_config,
                    driver_handle_output,
                )
            };

            if !nt_success(nt_status) {
                return nt_status;
            }

            match SAFE_DRIVER.driver_entry() {
                Ok(_) => 0,
                Err(nt_status) => nt_status,
            }
        }

        #[link_section = "PAGE"]
        extern "C" fn evt_driver_device_add(
            _driver: WDFDRIVER,
            mut device_init: *mut WDFDEVICE_INIT,
        ) -> NTSTATUS {
            // println!("EvtDriverDeviceAdd Entered!");

            let mut device_handle_output: WDFDEVICE = WDF_NO_HANDLE.cast();

            let ntstatus;
            // SAFETY: This is safe because:
            //       1. `device_init` is provided by `EvtDriverDeviceAdd` and is never null
            //       2. the argument receiving `WDF_NO_OBJECT_ATTRIBUTES` is allowed to be
            //          null
            //       3. `device_handle_output` is expected to be null
            unsafe {
                ntstatus = wdk_sys::call_unsafe_wdf_function_binding!(
                    WdfDeviceCreate,
                    &mut device_init,
                    WDF_NO_OBJECT_ATTRIBUTES,
                    &mut device_handle_output,
                );
            }

            // println!("WdfDeviceCreate NTSTATUS: {ntstatus:#02x}");

            match SAFE_DRIVER.evt_driver_device_add() {
                Ok(_) => 0,
                Err(nt_status) => nt_status,
            }
        }

        extern "C" fn driver_exit(_driver: *mut DRIVER_OBJECT) {
            // println!("Goodbye World!");
            // println!("Driver Exit Complete!");
        }
    }
    .into();

    wrappers.extend(input);

    wrappers
}

/// Represents a single trace control definition with GUID and optional flags
#[derive(Debug, Clone)]
struct TraceControlDef {
    guid: String,
    flags: Vec<String>,
}

/// Parser for trace_control argument
/// Supports two syntaxes:
/// 1. Just a GUID string: "guid-string"
/// 2. Tuple with GUID and flags: ("guid-string", [FLAG_ONE, FLAG_TWO]) or
///    ("guid-string", [])
impl Parse for TraceControlDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Check if it's a parenthesized tuple or just a string literal
        if input.peek(syn::token::Paren) {
            // Tuple format: ("guid", [FLAGS...])
            let content;
            syn::parenthesized!(content in input);

            // Parse GUID string
            let guid_lit: syn::LitStr = content.parse()?;
            let guid = guid_lit.value();

            if !is_valid_guid(&guid) {
                return Err(Error::new_spanned(guid_lit, "Not a valid GUID"));
            }

            // Parse comma separator
            content.parse::<Token![,]>()?;

            // Parse flag array [FLAG_ONE, FLAG_TWO, ...] or []
            let flags_content;
            syn::bracketed!(flags_content in content);

            let flags_punctuated: Punctuated<Ident, Token![,]> =
                Punctuated::parse_terminated(&flags_content)?;

            let flags: Vec<String> = flags_punctuated
                .iter()
                .map(|ident| ident.to_string())
                .collect();

            Ok(TraceControlDef { guid, flags })
        } else {
            // Simple string format: "guid"
            let guid_lit: syn::LitStr = input.parse()?;
            let guid = guid_lit.value();

            if !is_valid_guid(&guid) {
                return Err(Error::new_spanned(guid_lit, "Not a valid GUID"));
            }

            Ok(TraceControlDef {
                guid,
                flags: Vec::new(),
            })
        }
    }
}

/// Represents all trace control definitions
struct TraceControlArgs {
    controls: Vec<TraceControlDef>,
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

/// Validates that there are no duplicate flags across all trace controls
fn validate_no_duplicate_flags(
    controls: &[TraceControlDef],
) -> Result<(), (String, String, String)> {
    let mut seen_flags: HashSet<String> = HashSet::new();

    for control in controls {
        for flag in &control.flags {
            if !seen_flags.insert(flag.clone()) {
                // Find which GUID had this flag first
                for prev_control in controls {
                    if prev_control.flags.contains(flag) {
                        return Err((
                            flag.clone(),
                            prev_control.guid.clone(),
                            control.guid.clone(),
                        ));
                    }
                }
            }
        }
    }

    Ok(())
}

/// Generates the WppFlag enum declaration
fn generate_wpp_flags_declaration(trace_controls: &[TraceControlDef]) -> proc_macro2::TokenStream {
    // Check if there are any flags defined
    let has_flags = trace_controls.iter().any(|tc| !tc.flags.is_empty());

    if !has_flags {
        return quote! {};
    }

    // Collect all variants with their metadata
    struct VariantInfo {
        variant_ident: syn::Ident,
        flag_name: String,
        control_idx: usize,
        flag_idx: usize,
    }

    let variants: Vec<VariantInfo> = trace_controls
        .iter()
        .enumerate()
        .flat_map(|(control_idx, tc)| {
            tc.flags
                .iter()
                .enumerate()
                .map(move |(flag_idx, flag_name)| {
                    let variant_ident = Ident::new(flag_name, proc_macro2::Span::call_site());

                    VariantInfo {
                        variant_ident,
                        flag_name: flag_name.clone(),
                        control_idx,
                        flag_idx,
                    }
                })
        })
        .collect();

    // Generate enum variant definitions with tuple types: FLAG_NAME(usize, usize)
    let enum_variants: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { #ident(usize, usize) }
        })
        .collect();

    // Generate name match arms for by_name lookup
    let name_match_arms: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            let name = &v.flag_name;
            let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
            let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
            quote! { #name => Some(WppFlag::#ident(#ctrl_idx, #flag_idx)) }
        })
        .collect();

    // Generate static array entries
    let static_entries: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
            let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
            quote! { WppFlag::#ident(#ctrl_idx, #flag_idx) }
        })
        .collect();

    // Generate match arms for control_index
    let control_idx_arms: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { WppFlag::#ident(ctrl, _) => *ctrl }
        })
        .collect();

    // Generate match arms for flag_index
    let flag_idx_arms: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { WppFlag::#ident(_, flag) => *flag }
        })
        .collect();

    // Generate match arms for as_tuple
    let tuple_arms: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.variant_ident;
            quote! { WppFlag::#ident(ctrl, flag) => (*ctrl, *flag) }
        })
        .collect();

    let num_flags = variants.len();

    quote! {
        /// Standard WPP trace levels.
        /// These correspond to the standard Windows trace levels.
        #[allow(missing_docs)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u8)]
        pub enum TraceLevel {
            /// No tracing
            None = 0,
            /// Critical errors that cause system failure
            Critical = 1,
            /// Non-critical errors
            Error = 2,
            /// Warnings
            Warning = 3,
            /// Informational messages
            Information = 4,
            /// Verbose debug messages
            Verbose = 5,
            /// Reserved level 6
            Reserved6 = 6,
            /// Reserved level 7
            Reserved7 = 7,
            /// Reserved level 8
            Reserved8 = 8,
            /// Reserved level 9
            Reserved9 = 9,
        }

        impl TraceLevel {
            /// Returns the numeric value of the trace level.
            #[inline]
            pub const fn value(&self) -> u8 {
                *self as u8
            }

            /// Returns true if this level is less than or equal to Verbose.
            #[inline]
            pub const fn is_verbose_or_below(&self) -> bool {
                (*self as u8) <= (TraceLevel::Verbose as u8)
            }
        }

        /// WPP tracing flags for all trace controls.
        /// Each variant holds a tuple (control_index, flag_index) where both are 0-indexed.
        /// Flag names must be unique across all trace control definitions.
        #[allow(missing_docs)]
        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(C)]
        pub enum WppFlag {
            #(#enum_variants),*
        }

        impl WppFlag {
            /// Returns the control index (0-indexed).
            /// This indicates which trace control GUID this flag belongs to.
            #[inline]
            pub const fn control_index(&self) -> usize {
                match self {
                    #(#control_idx_arms),*
                }
            }

            /// Returns the flag index (0-indexed).
            /// This is the position of the flag within its trace control definition.
            #[inline]
            pub const fn flag_index(&self) -> usize {
                match self {
                    #(#flag_idx_arms),*
                }
            }

            /// Returns both the control index and flag index as a tuple.
            #[inline]
            pub const fn as_tuple(&self) -> (usize, usize) {
                match self {
                    #(#tuple_arms),*
                }
            }

            /// Looks up a WppFlag by its name.
            /// Returns `Some(WppFlag)` if found, `None` otherwise.
            #[inline]
            pub fn by_name(name: &str) -> Option<WppFlag> {
                match name {
                    #(#name_match_arms,)*
                    _ => None
                }
            }

            /// Returns all WPP flags as a slice.
            #[inline]
            pub const fn all() -> &'static [WppFlag] {
                &[#(#static_entries),*]
            }

            /// Returns the number of flags.
            #[inline]
            pub const fn count() -> usize {
                #num_flags
            }
        }
    }
}

/// A procedural macro used to mark the entry point of a WDF driver
///
/// # Supported Attributes
///
/// ## Simple GUID format (no flags):
/// ```ignore
/// #[driver_entry(trace_control = "guid-string")]
/// ```
///
/// ## GUID with flags:
/// ```ignore
/// #[driver_entry(trace_control = ("guid-string", [FLAG_ONE, FLAG_TWO]))]
/// ```
///
/// ## GUID with empty flags:
/// ```ignore
/// #[driver_entry(trace_control = ("guid-string", []))]
/// ```
///
/// ## Multiple trace controls:
/// ```ignore
/// #[driver_entry(trace_control = ("guid1", [FLAG_A, FLAG_B]), ("guid2", [FLAG_C, FLAG_D]))]
/// ```
///
/// Note: Flag names must be unique across all trace control definitions.
#[proc_macro_attribute]
pub fn driver_entry(args: TokenStream, input: TokenStream) -> TokenStream {
    const TRACE_CONTROL_ATTR_NAME: &str = "trace_control";

    let input_clone = input.clone();
    let item_fn = parse_macro_input!(input_clone as ItemFn);
    let safe_driver_entry = item_fn.sig.ident;

    let mut trace_controls: Vec<TraceControlDef> = Vec::new();

    let trace_control_parser = parser(|meta| {
        if meta.path.is_ident(TRACE_CONTROL_ATTR_NAME) {
            // Format: trace_control = "guid" or trace_control = ("guid", [FLAGS...]), ...
            meta.input.parse::<Token![=]>()?;

            let parsed: TraceControlArgs = meta.input.parse()?;
            trace_controls = parsed.controls;

            Ok(())
        } else {
            Err(meta.error(format!("Expected `{}`", TRACE_CONTROL_ATTR_NAME)))
        }
    });

    parse_macro_input!(args with trace_control_parser);

    // Validate no duplicate flags across trace controls
    if let Err((flag, guid1, guid2)) = validate_no_duplicate_flags(&trace_controls) {
        let err = Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "Duplicate flag '{}' found in trace controls. First in GUID '{}', duplicate in \
                 GUID '{}'",
                flag, guid1, guid2
            ),
        );
        return err.to_compile_error().into();
    }

    // Generate the tracing control GUID for the first control
    let parse_tracing_control_guid = if let Some(first_control) = trace_controls.first() {
        let guid = &first_control.guid;
        quote! {
            Some(wdf::Guid::parse(#guid).expect("Not a valid GUID"))
        }
    } else {
        quote! { None }
    };

    // Generate codeview annotations for all trace controls
    let codeview_annotations: Vec<proc_macro2::TokenStream> = trace_controls
        .iter()
        .map(|tc| {
            let guid = &tc.guid;
            let flags = &tc.flags;
            quote! {
                core::hint::codeview_annotation!("TMC:", #guid, "CtlGuid", #(#flags),*);
            }
        })
        .collect();

    // Generate the WPP flags declaration
    let wpp_flags_declaration = generate_wpp_flags_declaration(&trace_controls);

    let mut wrappers: TokenStream = quote! {
        #wpp_flags_declaration

        #[unsafe(link_section = "INIT")]
        #[unsafe(export_name = "DriverEntry")] // WDF expects a symbol with the name DriverEntry
        extern "system" fn __driver_entry(driver: &mut ::wdf::DRIVER_OBJECT, registry_path: ::wdf::PCUNICODE_STRING,) -> ::wdf::NTSTATUS {
            #(#codeview_annotations)*
            let tracing_control_guid = #parse_tracing_control_guid;
            ::wdf::call_safe_driver_entry(driver, registry_path, #safe_driver_entry, tracing_control_guid)
        }
    }
    .into();

    wrappers.extend(input);

    wrappers
}

/// The attribute used to mark a struct as a framework object context
#[proc_macro_attribute]
pub fn object_context(attr: TokenStream, item: TokenStream) -> TokenStream {
    object_context_impl("object_context", attr, item, false)
}

/// The attribute used to mark a struct as a framework object context.
/// It is the same as `object_context` but it also checks if the ref
/// count of the parent framework object is > 0 when this context is
/// being destroyed and bug checks if that is /// the case. This check
/// helps us catch a safety hole wherein a framework handle is left
/// behind, say in a static variable, when the framework is tearing
/// the handle down.
#[doc(hidden)]
#[proc_macro_attribute]
pub fn object_context_with_ref_count_check(attr: TokenStream, item: TokenStream) -> TokenStream {
    object_context_impl("object_context_with_ref_count_check", attr, item, true)
}

fn object_context_impl(
    attr_name: &str,
    attr: TokenStream,
    item: TokenStream,
    check_ref_count: bool,
) -> TokenStream {
    let fw_obj_type_name = parse_macro_input!(attr as Ident);
    let context_struct = parse_macro_input!(item as ItemStruct);

    // Make sure the struct is not generic.
    // This check is crucial to prevent fields of reference types
    // (e.g. `field: &SomeType`) from being used in context structs.
    // It is unsafe to allow reference fields in contexts because
    // they can dangle once the context is attached to the WDF object
    // and moved into the WDF heap.
    // The way this works is a bit indirect. By disallowing generics
    // we disallow lifetime annotations (e.g. `<'a>` in `Context<'a>``)
    // which in turn makes it impossible to have reference type
    // fields in the struct.
    // In addition to safety, generic context structs may also be
    // harder to reason about in general so it's good to just avoid them.
    if !context_struct.generics.params.is_empty() {
        return Error::new_spanned(
            context_struct,
            format!(
                "The `{}` attribute cannot be applied to generic structs",
                attr_name
            ),
        )
        .to_compile_error()
        .into();
    }

    // Make sure the struct does not have any odd alignment requirements
    // that conflict with the alignment of the framework's allocations.
    // This boils down to ensuring that the struct does not have any
    // repr attributes other than Rust and transparent.
    // Note that for performance reasons we check the repr attributes
    // only on the struct itself and not on its fields.
    // Alignment violations by the fields will be caught at run time
    // while attaching the context
    for attr in &context_struct.attrs {
        if attr.path().is_ident("repr") {
            let res = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("Rust") || meta.path.is_ident("transparent") {
                    Ok(())
                } else {
                    Err(Error::new_spanned(
                        attr,
                        format!(
                            "The `{attr_name}` attribute cannot be applied to structs with reprs \
                             other than `Rust` or `transparent`"
                        ),
                    ))
                }
            });

            if let Err(err) = res {
                return err.to_compile_error().into();
            }
        }
    }

    // Establish wdf crate's path to use
    let wdf_crate_path = if std::env::var("CARGO_PKG_NAME").ok() == Some("wdf".to_string()) {
        quote!(crate) // Inside the `wdf` crate itself
    } else {
        quote!(::wdf) // Outside of `wdf`, use the global path
    };

    let struct_name = &context_struct.ident;
    let static_name = Ident::new(
        &format!("__WDF_{struct_name}_TYPE_INFO"),
        struct_name.span(),
    );

    let cleanup_callback_name =
        Ident::new(&format!("__evt_{struct_name}_cleanup"), struct_name.span());

    let destroy_callback_name =
        Ident::new(&format!("__evt_{struct_name}_destroy"), struct_name.span());

    let attach_param_destroy_callback_name = if check_ref_count {
        quote! { Some(#destroy_callback_name) }
    } else {
        quote! { None }
    };

    let mut expanded = quote! {
        #context_struct

        #[allow(non_upper_case_globals)]
        #[unsafe(link_section = ".data")]
        static #static_name: #wdf_crate_path::WdfObjectContextTypeInfo = #wdf_crate_path::WdfObjectContextTypeInfo::new(#wdf_crate_path::WDF_OBJECT_CONTEXT_TYPE_INFO {
            Size: core::mem::size_of::<#wdf_crate_path::WdfObjectContextTypeInfo>() as u32,
            ContextName: concat!(stringify!(#struct_name),'\0').as_bytes().as_ptr().cast(),
            ContextSize: core::mem::size_of::<#struct_name>(),
            UniqueType: core::ptr::addr_of!(#static_name).cast::<#wdf_crate_path::WDF_OBJECT_CONTEXT_TYPE_INFO>(),
            EvtDriverGetUniqueContextType: None,
        });

        unsafe impl #wdf_crate_path::ObjectContext for #struct_name {
            fn get_type_info() -> &'static #wdf_crate_path::WdfObjectContextTypeInfo {
                &#static_name
            }
        }

        impl #struct_name {
            fn attach(fw_obj: &#fw_obj_type_name, context: #struct_name) -> #wdf_crate_path::NtResult<()> where Self: Sync {
                unsafe {
                    #wdf_crate_path::attach_context(fw_obj, context, #cleanup_callback_name, #attach_param_destroy_callback_name)
                }
            }

            fn get(fw_obj: &#fw_obj_type_name) -> &#struct_name where Self: Sync {
                Self::try_get(fw_obj).unwrap_or_else(|| Self::panic_on_missing_context())
            }

            fn get_mut(fw_obj: &mut #fw_obj_type_name) -> &mut #struct_name where Self: Sync {
                Self::try_get_mut(fw_obj).unwrap_or_else(|| Self::panic_on_missing_context())
            }

            fn try_get(fw_obj: &#fw_obj_type_name) -> Option<&#struct_name> where Self: Sync {
                unsafe {
                    #wdf_crate_path::try_get_context(fw_obj)
                }
            }

            fn try_get_mut(fw_obj: &mut #fw_obj_type_name) -> Option<&mut #struct_name> where Self: Sync {
                unsafe {
                    #wdf_crate_path::try_get_context_mut(fw_obj)
                }
            }

            fn panic_on_missing_context() -> ! {
                panic!(concat!("No context of type ", stringify!(#struct_name), " attached to the framework object"));
            }
        }

        #[allow(non_snake_case)]
        extern "C" fn #cleanup_callback_name(fw_obj: #wdf_crate_path::WDFOBJECT) {
            #wdf_crate_path::println!("Cleanup callback called for {} addr {:#x}", stringify!(#struct_name), fw_obj as usize);
            unsafe {
                #wdf_crate_path::drop_context::<#struct_name>(fw_obj);
            }
        }
    };

    if check_ref_count {
        let extended = quote! {
            #[allow(non_snake_case)]
            extern "C" fn #destroy_callback_name(fw_obj: #wdf_crate_path::WDFOBJECT) {
                #wdf_crate_path::bug_check_if_ref_count_not_zero::<#fw_obj_type_name, #struct_name>(fw_obj);
            }
        };

        expanded.extend(extended);
    }

    expanded.into()
}

// TODO: this code is repeated in the wdf::Guid.
// Move it to a common location
fn is_valid_guid(guid_str: &str) -> bool {
    // Remove dashes from the input string
    let guid_str = guid_str.replace('-', "");

    if guid_str.len() != 32 {
        return false;
    }

    if u32::from_str_radix(&guid_str[0..8], 16).is_err() {
        return false;
    }

    if u16::from_str_radix(&guid_str[8..12], 16).is_err() {
        return false;
    }

    if u16::from_str_radix(&guid_str[12..16], 16).is_err() {
        return false;
    }

    for i in 0..8 {
        if u8::from_str_radix(&guid_str[16 + i * 2..18 + i * 2], 16).is_err() {
            return false;
        }
    }

    true
}

/// A static counter for generating unique message IDs across trace invocations.
/// This is incremented for each trace! macro expansion.
static TRACE_MESSAGE_ID: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(10);

/// Represents a parsed trace argument with annotation metadata.
struct TraceArg {
    /// Display name for the argument in the annotation
    name: String,
    /// Optional display format override for the WPP typev line (e.g., "d", "x").
    /// Empty string means no override — bare `%N` is used in the typev line.
    display_format: String,
    /// The original expression for this argument
    expr: syn::Expr,
    /// Whether this arg should be wrapped in `CString` (string literals or `{:s}`)
    wrap_cstring: bool,
    /// Whether this arg should be type-annotated as `i32` (unsuffixed int literals)
    type_as_i32: bool,
}

/// Represents a single trace argument expression.
struct TraceArgInput {
    expr: syn::Expr,
}

impl Parse for TraceArgInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let expr: syn::Expr = input.parse()?;
        Ok(TraceArgInput { expr })
    }
}

/// Result of parsing trace macro arguments
struct ParsedTraceArgs {
    /// Optional flag identifier (e.g., FLAG_ONE)
    flag: Option<syn::Ident>,
    /// Optional trace level (e.g., TraceLevel::Information)
    level: Option<syn::Ident>,
    /// The format string
    format_str: String,
    /// The trace arguments
    args: Vec<TraceArg>,
}

/// Known trace level names for validation
const TRACE_LEVEL_NAMES: &[&str] = &[
    "None",
    "Critical",
    "Error",
    "Warning",
    "Information",
    "Verbose",
    "Reserved6",
    "Reserved7",
    "Reserved8",
    "Reserved9",
];

/// Parses the trace! macro input and extracts optional flag, optional level,
/// format string and arguments
///
/// Supported syntaxes:
/// - `trace!("fmt", args...)` - no flag, no level (defaults to
///   TraceLevel::None)
/// - `trace!(FLAG, "fmt", args...)` - with flag, no level
/// - `trace!(FLAG, Level, "fmt", args...)` - with flag and level
/// - `trace!(Level, "fmt", args...)` - with level only (uses default control
///   block)
fn parse_trace_args(item: TokenStream) -> Result<ParsedTraceArgs, Error> {
    use syn::{Expr, ExprLit, punctuated::Punctuated};

    // Parse as comma-separated TraceArgInput items
    let args = syn::parse::Parser::parse(
        Punctuated::<TraceArgInput, Token![,]>::parse_terminated,
        item,
    )?;

    // Convert to Vec for easier manipulation
    let all_args: Vec<TraceArgInput> = args.into_iter().collect();

    if all_args.is_empty() {
        return Err(Error::new(
            proc_macro2::Span::call_site(),
            "trace! requires at least a format string",
        ));
    }

    let mut flag: Option<syn::Ident> = None;
    let mut level: Option<syn::Ident> = None;
    let format_str: String;
    let remaining_start_idx: usize;

    // Determine the pattern based on the first few arguments
    let first_is_path = matches!(&all_args[0].expr, Expr::Path(_));
    let first_is_string = matches!(
        &all_args[0].expr,
        Expr::Lit(ExprLit {
            lit: Lit::Str(_),
            ..
        })
    );

    if first_is_string {
        // Pattern: trace!("fmt", args...)
        format_str = match &all_args[0].expr {
            Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) => s.value(),
            _ => unreachable!(),
        };
        remaining_start_idx = 1;
    } else if first_is_path && all_args.len() >= 2 {
        let second_is_string = matches!(
            &all_args[1].expr,
            Expr::Lit(ExprLit {
                lit: Lit::Str(_),
                ..
            })
        );
        let second_is_path = matches!(&all_args[1].expr, Expr::Path(_));

        if second_is_string {
            // Pattern: trace!(IDENT, "fmt", ...) - could be FLAG or Level
            let ident = extract_path_ident(&all_args[0].expr)?;
            let ident_str = ident.to_string();

            if TRACE_LEVEL_NAMES.contains(&ident_str.as_str()) {
                // It's a level: trace!(Level, "fmt", ...)
                level = Some(ident);
            } else {
                // It's a flag: trace!(FLAG, "fmt", ...)
                flag = Some(ident);
            }

            format_str = match &all_args[1].expr {
                Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) => s.value(),
                _ => unreachable!(),
            };
            remaining_start_idx = 2;
        } else if second_is_path && all_args.len() >= 3 {
            let third_is_string = matches!(
                &all_args[2].expr,
                Expr::Lit(ExprLit {
                    lit: Lit::Str(_),
                    ..
                })
            );

            if third_is_string {
                // Pattern: trace!(FLAG, Level, "fmt", ...)
                let first_ident = extract_path_ident(&all_args[0].expr)?;
                let second_ident = extract_path_ident(&all_args[1].expr)?;

                // Second should be a level
                let second_str = second_ident.to_string();
                if !TRACE_LEVEL_NAMES.contains(&second_str.as_str()) {
                    return Err(Error::new_spanned(
                        &all_args[1].expr,
                        format!(
                            "Expected a trace level (one of: {:?}), got '{}'",
                            TRACE_LEVEL_NAMES, second_str
                        ),
                    ));
                }

                flag = Some(first_ident);
                level = Some(second_ident);

                format_str = match &all_args[2].expr {
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) => s.value(),
                    _ => unreachable!(),
                };
                remaining_start_idx = 3;
            } else {
                return Err(Error::new_spanned(
                    &all_args[0].expr,
                    "Expected a format string",
                ));
            }
        } else {
            return Err(Error::new_spanned(
                &all_args[0].expr,
                "Expected a format string",
            ));
        }
    } else {
        return Err(Error::new_spanned(
            &all_args[0].expr,
            "Expected a format string or flag identifier",
        ));
    }

    // Parse format placeholders to extract per-argument specifier overrides
    let placeholders = parse_format_placeholders(&format_str);

    // Remaining arguments are the trace parameters
    let remaining_args: Vec<TraceArgInput> =
        all_args.into_iter().skip(remaining_start_idx).collect();

    let mut trace_args = Vec::new();
    for (idx, arg_input) in remaining_args.into_iter().enumerate() {
        let spec_override = placeholders
            .get(idx)
            .and_then(|p| p.spec_override.as_deref());
        let trace_arg = infer_arg_info(&arg_input.expr, idx, spec_override)?;
        trace_args.push(trace_arg);
    }

    Ok(ParsedTraceArgs {
        flag,
        level,
        format_str,
        args: trace_args,
    })
}

/// Extracts an identifier from a path expression
fn extract_path_ident(expr: &syn::Expr) -> Result<syn::Ident, Error> {
    if let syn::Expr::Path(path) = expr {
        if let Some(seg) = path.path.segments.last() {
            return Ok(seg.ident.clone());
        }
    }
    Err(Error::new_spanned(expr, "Expected an identifier"))
}

/// Result of parsing a single `{}` placeholder in the format string.
struct FormatPlaceholder {
    /// The format specifier override (e.g., "NTSTATUS"), or None for bare `{}`
    spec_override: Option<String>,
}

/// Parses the format string and extracts placeholder information.
fn parse_format_placeholders(format_str: &str) -> Vec<FormatPlaceholder> {
    let mut placeholders = Vec::new();
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                chars.next(); // Escaped {{
                continue;
            }
            let mut inner = String::new();
            while let Some(inner_c) = chars.next() {
                if inner_c == '}' {
                    break;
                }
                inner.push(inner_c);
            }
            let spec_override = if let Some(colon_pos) = inner.find(':') {
                let spec = inner[colon_pos + 1..].trim();
                if spec.is_empty() {
                    None
                } else {
                    Some(spec.to_string())
                }
            } else {
                None
            };
            placeholders.push(FormatPlaceholder { spec_override });
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
                chars.next();
            }
        }
    }
    placeholders
}

/// Returns `true` if the target platform has 64-bit pointers.
///
/// Uses `CARGO_CFG_TARGET_POINTER_WIDTH` which Cargo sets based on the
/// **target** (not host), so cross-compilation works correctly.
fn target_is_64bit() -> bool {
    std::env::var("CARGO_CFG_TARGET_POINTER_WIDTH")
        .map(|w| w == "64")
        .unwrap_or(true) // default to 64-bit if unknown
}

/// Returns the (ETW type, format spec) for a signed pointer-sized integer
/// on the current target.
// fn isize_etw() -> (&'static str, &'static str) {
//     if target_is_64bit() {
//         ("ItemLongLong", "I64d")
//     } else {
//         ("ItemLong", "d")
//     }
// }

/// Returns the (ETW type, format spec) for an unsigned pointer-sized integer
/// on the current target.
// fn usize_etw() -> (&'static str, &'static str) {
//     if target_is_64bit() {
//         ("ItemULongLong", "I64u")
//     } else {
//         ("ItemULong", "u")
//     }
// }

/// Validates that a user-facing format specifier is known and returns
/// the corresponding C printf format spec for the WPP typev line.
///
/// Returns `Some(display_fmt)` for known specifiers, `None` for unknown.
/// The ETW type is no longer resolved here — it comes from `T::ETW_TYPE`
/// at monomorphization time via the generic annotation function.
fn display_format_for(spec: &str) -> Option<String> {
    match spec {
        // C-style format specifiers
        "d" | "i" => Some("d".to_string()),
        "u" => Some("u".to_string()),
        "x" => Some("x".to_string()),
        "X" => Some("X".to_string()),
        "o" => Some("o".to_string()),
        "s" | "str" => Some("s".to_string()),
        "p" => Some("p".to_string()),

        // Rust type-name specifiers → mapped to C printf display format
        "i8" | "i16" | "i32" => Some("d".to_string()),
        "u8" | "u16" | "u32" => Some("u".to_string()),
        "i64" | "I64d" => Some("I64d".to_string()),
        "u64" | "I64u" => Some("I64u".to_string()),
        "isize" => Some(if target_is_64bit() { "I64d" } else { "d" }.to_string()),
        "usize" => Some(if target_is_64bit() { "I64u" } else { "u" }.to_string()),
        "bool" => Some("d".to_string()),
        "I64x" => Some("I64x".to_string()),

        // WPP special types — ETW_TYPE in the body handles identification,
        // no specific display format needed in the typev line.
        "NTSTATUS" | "STATUS" | "HRESULT" | "GUID" => Some(String::new()),

        _ => None,
    }
}

// /// Old format_spec_override that mapped specifiers to (etw_type, format_spec)
// /// pairs. No longer needed — ETW type comes from TraceData::ETW_TYPE via
// /// generic monomorphization.
// fn format_spec_override(spec: &str) -> Option<(&'static str, &'static str)> { ... }

/// Infers the argument name and metadata from an expression,
/// with an optional display format override from the format string.
///
/// **Variables no longer require explicit format specifiers.** The ETW type
/// is resolved at compile time via `TraceData::ETW_TYPE` through the generic
/// annotation function. Format specifiers like `{:x}` are optional display
/// hints for the WPP typev line.
fn infer_arg_info(
    expr: &syn::Expr,
    idx: usize,
    spec_override: Option<&str>,
) -> Result<TraceArg, Error> {
    use syn::Expr;

    let name = extract_arg_name(expr, idx);
    let mut display_format = String::new();
    let mut wrap_cstring = false;
    let mut type_as_i32 = false;

    // Handle display format override from {:spec}
    if let Some(spec) = spec_override {
        if let Some(fmt) = display_format_for(spec) {
            display_format = fmt;
        } else {
            return Err(Error::new_spanned(
                expr,
                format!(
                    "Unknown format specifier ':{spec}'. Supported specifiers:\n\n  C-style:     \
                     d, u, x, X, o, s, p\n  Rust types:  i8, i16, i32, u8, u16, u32, i64, u64, \
                     isize, usize, bool\n  WPP types:   NTSTATUS, STATUS, HRESULT, GUID\n  \
                     64-bit:      I64d, I64u, I64x",
                ),
            ));
        }
        if matches!(spec, "s" | "str") {
            wrap_cstring = true;
        }
    }

    // Detect string literals → auto CString wrap
    match expr {
        Expr::Lit(lit) => match &lit.lit {
            Lit::Str(_) => {
                wrap_cstring = true;
            }
            Lit::Int(int_lit) if int_lit.suffix().is_empty() => {
                type_as_i32 = true;
            }
            _ => {}
        },
        Expr::Reference(r) => {
            if let Expr::Lit(lit) = r.expr.as_ref() {
                if matches!(&lit.lit, Lit::Str(_)) {
                    wrap_cstring = true;
                }
            }
        }
        _ => {}
    }

    Ok(TraceArg {
        name,
        display_format,
        expr: expr.clone(),
        wrap_cstring,
        type_as_i32,
    })
}

// /// Old infer_arg_info that required explicit format specifiers for variables.
// /// No longer needed — the generic annotation function resolves ETW types
// /// via TraceData::ETW_TYPE at monomorphization time.

/// Extracts a human-readable name from an expression for use in trace
/// annotations.
fn extract_arg_name(expr: &syn::Expr, idx: usize) -> String {
    use syn::Expr;
    match expr {
        Expr::Path(p) => p
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_else(|| format!("arg{}", idx)),
        Expr::Lit(lit) => match &lit.lit {
            Lit::Int(_) => format!("literal{}", idx),
            Lit::Str(_) => format!("str{}", idx),
            _ => format!("arg{}", idx),
        },
        Expr::Reference(r) => {
            if let Expr::Path(p) = r.expr.as_ref() {
                p.path
                    .segments
                    .last()
                    .map(|s| s.ident.to_string())
                    .unwrap_or_else(|| format!("arg{}", idx))
            } else {
                format!("arg{}", idx)
            }
        }
        Expr::Field(f) => match &f.member {
            syn::Member::Named(ident) => ident.to_string(),
            syn::Member::Unnamed(index) => format!("field{}", index.index),
        },
        Expr::MethodCall(m) => m.method.to_string(),
        Expr::Call(call) => {
            // For calls like NtStatus::from(val), extract inner arg name
            if let Some(first_arg) = call.args.first() {
                extract_arg_name(first_arg, idx)
            } else {
                format!("arg{}", idx)
            }
        }
        _ => format!("arg{}", idx),
    }
}

/// Extracts the literal text segments between `{}` placeholders in a format
/// string.  For N placeholders (args), returns N+1 segments.
///
/// # Example
/// ```ignore
/// extract_format_literal_segments("i32={}, u64={}")
///   => ["i32=", ", u64=", ""]
/// ```
fn extract_format_literal_segments(format_str: &str) -> Vec<String> {
    let mut segments = Vec::new();
    let mut current = String::new();
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                chars.next(); // Escaped {{
                current.push('{');
                continue;
            }
            // Skip everything inside {}
            while let Some(inner) = chars.next() {
                if inner == '}' {
                    break;
                }
            }
            segments.push(std::mem::take(&mut current));
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
                chars.next();
                current.push('}');
            } else {
                current.push(c);
            }
        } else {
            current.push(c);
        }
    }
    segments.push(current);
    segments
}

/// Generates the WPP-style format string with numbered placeholders.
/// Strips any `:SPEC` suffixes from the format string.
/// Uses bare `%N` when no display format override is specified,
/// or `%N!spec!` when an explicit display format is given.
///
/// NOTE: This function is retained for reference / zero-arg fallback.
/// For args > 0 the format string is now constructed at compile time via
/// `generic_const_exprs` and `TraceData::FORMAT_SPEC`.
fn generate_wpp_format_string(format_str: &str, args: &[TraceArg]) -> String {
    let mut result = String::new();
    let mut arg_idx = 0;
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                chars.next();
                result.push('{');
            } else {
                while let Some(inner) = chars.next() {
                    if inner == '}' {
                        break;
                    }
                }
                if arg_idx < args.len() {
                    let param_num = 10 + arg_idx;
                    let spec = &args[arg_idx].display_format;
                    if spec.is_empty() {
                        result.push_str(&format!("%{}", param_num));
                    } else {
                        result.push_str(&format!("%{}!{}!", param_num, spec));
                    }
                    arg_idx += 1;
                }
            }
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
                chars.next();
                result.push('}');
            } else {
                result.push(c);
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// A procedural macro for WPP-style tracing with compile-time type resolution.
///
/// # Format Specifiers
///
/// Format specifiers are **optional**. The ETW type metadata (e.g., `ItemLong`,
/// `ItemString`) is resolved at compile time via `TraceData::ETW_TYPE` through
/// generic monomorphization — the proc macro does not need to know the type.
///
/// Optional display format overrides can be specified using `{:SPEC}` syntax
/// to control how the value appears in trace decoder output.
///
/// ## Auto-detected:
/// - **Integer literals**: `42` → `i32`, `1000u64` → `u64`
/// - **String literals**: `"hello"` → wrapped in `CString`
/// - **All typed variables**: ETW type resolved via `TraceData::ETW_TYPE`
///
/// ## Optional display overrides:
///
/// | Override           | Display Format | Use for                    |
/// |--------------------|----------------|----------------------------|
/// | `{:x}` / `{:X}`   | hex            | Hex display                |
/// | `{:o}`             | octal          | Octal display              |
/// | `{:d}`             | decimal        | Explicit decimal           |
/// | `{:s}`             | string         | Wrap variable in `CString` |
/// | `{:p}`             | pointer        | Pointer display            |
///
/// # Usage
///
/// ```ignore
/// // Variables — no format specifier needed!
/// let code: i32 = 42;
/// trace!("Code: {}", code);
///
/// // Integer literal — auto-typed as i32
/// trace!("Value: {}", 42);
///
/// // NtStatus variable — auto-detected via TraceData
/// let status = NtStatus::from(0);
/// trace!("Status: {}", status);
///
/// // HRESULT — use HResult wrapper
/// let hr = HResult(0);
/// trace!("Result: {}", hr);
///
/// // Hex display override
/// let flags: u32 = 0xFF;
/// trace!("Flags: {:x}", flags);
///
/// // With flag and level
/// trace!(FLAG_ONE, Information, "Status: {}", status);
/// ```
#[proc_macro]
pub fn trace(item: TokenStream) -> TokenStream {
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

    let ParsedTraceArgs {
        flag,
        level,
        format_str,
        args,
    } = parsed;

    let message_id = TRACE_MESSAGE_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

    let driver_name = std::env::var("CARGO_PKG_NAME")
        .unwrap_or_else(|_| "unknown_driver".to_string())
        .replace('-', "_");

    // TODO: Get actual msg GUID, this can be generated randomly per compilation or
    // as per algorithm
    let msg_guid = "e7602a7b-5034-321b-d450-a986113fc2e1";

    // Build the codeview_annotation parameters
    let param2 = format!("{} {} // SRC={} MJ= MN=", msg_guid, driver_name, file_name);
    let typev_name = format!("{}_{}", driver_name, line_number);

    let (arg_bindings, byte_slice_bindings, byte_slice_idents) = generate_arg_code(&args);

    // Build generic type parameters and annotation body entries.
    // Each arg gets a type parameter TN with TraceData bound, and the
    // annotation body uses TN::ETW_TYPE for the ETW type string.
    let type_params: Vec<syn::Ident> = (0..args.len())
        .map(|i| syn::Ident::new(&format!("T{}", i), proc_macro2::Span::call_site()))
        .collect();

    let fn_name = syn::Ident::new(
        &format!("__trace_codeview_{}", message_id),
        proc_macro2::Span::call_site(),
    );

    // -----------------------------------------------------------------------
    // Compile-time format string construction via generic_const_exprs.
    //
    // For args > 0: a helper struct concatenates static typev segments with
    // T::FORMAT_SPEC at compile time, producing a single &'static str.
    // For args == 0: the typev line is fully static.
    // -----------------------------------------------------------------------
    let fmt_segments = extract_format_literal_segments(&format_str);
    let typev_prefix = format!("#typev {} {} \"%0", typev_name, message_id);
    let typev_suffix = "\"";

    // Build the N+1 static segments that interleave with T::FORMAT_SPEC.
    // Pattern: static[0] + T0::FORMAT_SPEC + static[1] + T1::FORMAT_SPEC + ... + static[N]
    // Where static[0] = prefix + seg[0] + "%10!"
    //       static[i] = "!" + seg[i] + "%{10+i}!"   for 1 <= i < N
    //       static[N] = "!" + seg[N] + suffix
    let mut typev_static_segs: Vec<String> = Vec::new();
    if args.is_empty() {
        typev_static_segs
            .push(format!("{}{}{}", typev_prefix, fmt_segments[0], typev_suffix));
    } else {
        for i in 0..=args.len() {
            if i == 0 {
                typev_static_segs
                    .push(format!("{}{}%{}!", typev_prefix, fmt_segments[i], 10 + i));
            } else if i == args.len() {
                typev_static_segs
                    .push(format!("!{}{}", fmt_segments[i], typev_suffix));
            } else {
                typev_static_segs
                    .push(format!("!{}%{}!", fmt_segments[i], 10 + i));
            }
        }
    }

    // Compute total static segment length (known at proc-macro time).
    let static_total: usize = typev_static_segs.iter().map(|s| s.len()).sum();

    // ---- Generate the format-builder struct / where clause / param3 entry ----
    let (fmt_struct_code, fn_where_clause, param3_annotation) = if args.is_empty() {
        // Zero args: the typev line is fully static, no generic const needed.
        let param3 = typev_static_segs[0].clone();
        (quote! {}, quote! {}, quote! { #param3 })
    } else {
        let fmt_struct_name = syn::Ident::new(
            &format!("__TraceFmt{}", message_id),
            proc_macro2::Span::call_site(),
        );

        let static_total_lit =
            proc_macro2::Literal::usize_suffixed(static_total);

        // Sum expression:  static_total + T0::FORMAT_SPEC.len() + T1::FORMAT_SPEC.len() + ...
        let format_spec_lens: Vec<proc_macro2::TokenStream> = type_params
            .iter()
            .map(|tp| quote! { + #tp::FORMAT_SPEC.len() })
            .collect();

        // Build byte-copy loops: interleave static segment copies and
        // generic FORMAT_SPEC copies.
        let mut copy_loops: Vec<proc_macro2::TokenStream> = Vec::new();
        for (i, seg) in typev_static_segs.iter().enumerate() {
            // Static segment
            let seg_lit = proc_macro2::Literal::string(seg);
            copy_loops.push(quote! {
                {
                    let s = #seg_lit.as_bytes();
                    let mut i = 0usize;
                    while i < s.len() { buf[pos] = s[i]; pos += 1; i += 1; }
                }
            });
            // Generic FORMAT_SPEC segment (N generic segs between N+1 static segs)
            if i < args.len() {
                let tp = &type_params[i];
                copy_loops.push(quote! {
                    {
                        let s = #tp::FORMAT_SPEC.as_bytes();
                        let mut i = 0usize;
                        while i < s.len() { buf[pos] = s[i]; pos += 1; i += 1; }
                    }
                });
            }
        }

        // Struct bounds (shared between struct def and impl)
        let struct_bounds: Vec<proc_macro2::TokenStream> = type_params
            .iter()
            .map(|tp| quote! { #tp: ::wdf::__internal::TraceData })
            .collect();

        let struct_code = quote! {
            struct #fmt_struct_name <#(#struct_bounds),*> (
                core::marker::PhantomData<(#(#type_params),*)>
            );

            #[allow(unused_parens)]
            impl<#(#struct_bounds),*> #fmt_struct_name <#(#type_params),*>
            where [(); #static_total_lit #(#format_spec_lens)* ]:
            {
                const BYTES: [u8; #static_total_lit #(#format_spec_lens)*] = {
                    let mut buf = [0u8; #static_total_lit #(#format_spec_lens)*];
                    let mut pos = 0usize;
                    #(#copy_loops)*
                    buf
                };
                const VALUE: &'static str =
                    unsafe { core::str::from_utf8_unchecked(&Self::BYTES) };
            }
        };

        let where_clause = quote! {
            where [(); #static_total_lit #(#format_spec_lens)* ]:
        };

        let param3_entry =
            quote! { #fmt_struct_name::<#(#type_params),*>::VALUE };

        (struct_code, where_clause, param3_entry)
    };

    // Build per-argument concat structs that merge "name, " + T::ETW_TYPE + " -- N"
    // into a single &'static str at compile time, using the same generic_const_exprs
    // approach as the format string.
    let mut arg_concat_struct_codes: Vec<proc_macro2::TokenStream> = Vec::new();
    let mut arg_concat_value_tokens: Vec<proc_macro2::TokenStream> = Vec::new();

    for (idx, arg) in args.iter().enumerate() {
        let param_num = 10 + idx;
        let prefix = format!("{}, ", arg.name);
        let suffix = format!(" -- {}", param_num);
        let tp = &type_params[idx];

        let prefix_len = prefix.len();
        let suffix_len = suffix.len();
        let static_part_len = prefix_len + suffix_len;
        let static_part_lit = proc_macro2::Literal::usize_suffixed(static_part_len);

        let prefix_lit = proc_macro2::Literal::string(&prefix);
        let suffix_lit = proc_macro2::Literal::string(&suffix);

        let struct_name = syn::Ident::new(
            &format!("__TraceArg{}_{}", idx, message_id),
            proc_macro2::Span::call_site(),
        );

        let arg_struct = quote! {
            struct #struct_name <__TA: ::wdf::__internal::TraceData> (
                core::marker::PhantomData<__TA>
            );

            #[allow(unused_parens)]
            impl<__TA: ::wdf::__internal::TraceData> #struct_name <__TA>
            where [(); #static_part_lit + __TA::ETW_TYPE.len() ]:
            {
                const BYTES: [u8; #static_part_lit + __TA::ETW_TYPE.len()] = {
                    let mut buf = [0u8; #static_part_lit + __TA::ETW_TYPE.len()];
                    let mut pos = 0usize;
                    {
                        let s = #prefix_lit .as_bytes();
                        let mut i = 0usize;
                        while i < s.len() { buf[pos] = s[i]; pos += 1; i += 1; }
                    }
                    {
                        let s = __TA::ETW_TYPE.as_bytes();
                        let mut i = 0usize;
                        while i < s.len() { buf[pos] = s[i]; pos += 1; i += 1; }
                    }
                    {
                        let s = #suffix_lit .as_bytes();
                        let mut i = 0usize;
                        while i < s.len() { buf[pos] = s[i]; pos += 1; i += 1; }
                    }
                    buf
                };
                const VALUE: &'static str =
                    unsafe { core::str::from_utf8_unchecked(&Self::BYTES) };
            }
        };
        arg_concat_struct_codes.push(arg_struct);

        // Reference:  __TraceArgN_M::<TN>::VALUE
        arg_concat_value_tokens.push(quote! { #struct_name :: <#tp> :: VALUE });
    }

    // Build annotation array entries — each arg is now a single concatenated line.
    let mut annotation_entries: Vec<proc_macro2::TokenStream> = vec![
        quote! { "TMF:" },
        quote! { #param2 },
        param3_annotation,
        quote! { "{" },
    ];
    for arg_value in &arg_concat_value_tokens {
        annotation_entries.push(quote! { #arg_value });
    }
    annotation_entries.push(quote! { "}" });

    // Also add per-arg where bounds to the function where clause so the
    // arg concat structs' const exprs are constrained.
    let mut arg_where_bounds: Vec<proc_macro2::TokenStream> = Vec::new();
    for (idx, _arg) in args.iter().enumerate() {
        let prefix = format!("{}, ", _arg.name);
        let suffix = format!(" -- {}", 10 + idx);
        let static_part_len = prefix.len() + suffix.len();
        let static_part_lit = proc_macro2::Literal::usize_suffixed(static_part_len);
        let tp = &type_params[idx];
        arg_where_bounds.push(quote! { [(); #static_part_lit + #tp::ETW_TYPE.len() ]: });
    }

    // Merge the format-string where clause with per-arg where clauses.
    // All bounds go under a single `where` keyword, comma-separated.
    let fn_where_clause = if args.is_empty() {
        fn_where_clause  // already empty
    } else {
        // fn_where_clause is `where [(); <fmt_len_expr>]:` — strip it and
        // rebuild with all bounds comma-separated under one `where`.
        quote! {
            #fn_where_clause , #(#arg_where_bounds),*
        }
    };

    // Build the generic function: type params, bounds, parameters, and call args
    let generics = if type_params.is_empty() {
        quote! {}
    } else {
        let bounds: Vec<proc_macro2::TokenStream> = type_params
            .iter()
            .map(|tp| quote! { #tp: ::wdf::__internal::TraceData })
            .collect();
        quote! { <#(#bounds),*> }
    };

    let fn_params: Vec<proc_macro2::TokenStream> = type_params
        .iter()
        .enumerate()
        .map(|(i, tp)| {
            let pname = syn::Ident::new(&format!("_arg{}", i), proc_macro2::Span::call_site());
            quote! { #pname: &#tp }
        })
        .collect();

    let call_args: Vec<proc_macro2::TokenStream> = (0..args.len())
        .map(|i| {
            let arg_name =
                syn::Ident::new(&format!("__trace_arg{}", i), proc_macro2::Span::call_site());
            quote! { &#arg_name }
        })
        .collect();

    let variadic_args: Vec<proc_macro2::TokenStream> = byte_slice_idents
        .iter()
        .map(|ident| {
            quote! { #ident.as_ptr() as *const core::ffi::c_void, #ident.len(), }
        })
        .collect();

    let message_id_lit = proc_macro2::Literal::u16_unsuffixed(message_id as u16);

    let level_value = if let Some(level_ident) = &level {
        quote! { TraceLevel::#level_ident as u8 }
    } else {
        quote! { TraceLevel::None as u8 }
    };

    let (flags_value, control_index) = if let Some(flag_ident) = &flag {
        let flag_name = flag_ident.to_string();
        (
            quote! {
                {
                    let __flag = WppFlag::by_name(#flag_name).expect("Unknown WppFlag");
                    (__flag.flag_index() + 1) as u32
                }
            },
            quote! {
                {
                    let __flag = WppFlag::by_name(#flag_name).expect("Unknown WppFlag");
                    __flag.control_index()
                }
            },
        )
    } else {
        (quote! { 0u32 }, quote! { 0usize })
    };

    let expanded = quote! {
        {
            extern crate alloc;

            #(#arg_bindings)*

            #fmt_struct_code

            #(#arg_concat_struct_codes)*

            #[inline(always)]
            fn #fn_name #generics (#(#fn_params),*) #fn_where_clause {
                core::intrinsics::codeview_annotation(
                    &[#(#annotation_entries),*]
                );
            }

            #fn_name(#(#call_args),*);

            #(#byte_slice_bindings)*

            let __trace_level: u8 = #level_value;
            let __trace_flags: u32 = #flags_value;
            let __control_index: usize = #control_index;

            if let Some(__trace_writer) = ::wdf::get_trace_writer() {
                let __should_trace_wpp = (__trace_flags == 0 || __trace_writer.is_flag_enabled(__control_index, __trace_flags))
                                            && __trace_writer.is_level_enabled(__control_index, __trace_level);
                let __should_auto_log = __trace_level < TraceLevel::Verbose as u8 || __trace_writer.is_auto_log_verbose_enabled(__control_index);

                unsafe {
                    if __should_trace_wpp {
                        let __logger = ::wdf::__internal::get_wpp_logger().unwrap();
                        let _ = ::wdf::__internal::get_wpp_trace_message().unwrap()(
                            __logger,
                            ::wdf::__internal::WPP_TRACE_OPTIONS,
                            &::wdf::__internal::TRACE_GUID,
                            #message_id_lit,
                            #(#variadic_args)*
                            core::ptr::null::<core::ffi::c_void>(),
                        );
                    }

                    if __should_auto_log {
                        let __auto_log_context = ::wdf::__internal::get_auto_log_context().unwrap();
                        let _ = ::wdf::__internal::WppAutoLogTrace(
                            __auto_log_context,
                            __trace_level,
                            __trace_flags,
                            &::wdf::__internal::TRACE_GUID as *const _ as *mut _,
                            #message_id_lit,
                            #(#variadic_args)*
                            core::ptr::null::<core::ffi::c_void>(),
                        );
                    }
                }
            }
        }
    };

    expanded.into()
}

/// Generates argument bindings, byte-slice conversions via `as_bytes()`, and
/// byte-slice identifiers.
///
/// String literals (and `{:s}` overrides) are wrapped in `CString`.
/// Unsuffixed integer literals are type-annotated as `i32`.
/// All other expressions are passed through — the `TraceData` bound on the
/// generic annotation function ensures type safety at compile time.
fn generate_arg_code(
    args: &[TraceArg],
) -> (
    Vec<proc_macro2::TokenStream>,
    Vec<proc_macro2::TokenStream>,
    Vec<syn::Ident>,
) {
    let mut bindings = Vec::new();
    let mut byte_slices = Vec::new();
    let mut idents = Vec::new();

    for (idx, arg) in args.iter().enumerate() {
        let arg_name = syn::Ident::new(
            &format!("__trace_arg{}", idx),
            proc_macro2::Span::call_site(),
        );
        let bytes_name = syn::Ident::new(
            &format!("__trace_bytes{}", idx),
            proc_macro2::Span::call_site(),
        );
        let expr = &arg.expr;

        if arg.wrap_cstring {
            // String arguments: convert to CString first
            bindings.push(quote! {
                let #arg_name = alloc::ffi::CString::new(#expr).unwrap();
            });
        } else if arg.type_as_i32 {
            // Unsuffixed integer literals: type as i32
            bindings.push(quote! {
                let #arg_name: i32 = #expr;
            });
        } else {
            // All other expressions: pass through, TraceData bound
            // ensures correctness at compile time
            bindings.push(quote! {
                let #arg_name = #expr;
            });
        }

        byte_slices.push(quote! {
            let #bytes_name = ::wdf::__internal::TraceData::as_bytes(&#arg_name);
        });

        idents.push(bytes_name);
    }

    (bindings, byte_slices, idents)
}
