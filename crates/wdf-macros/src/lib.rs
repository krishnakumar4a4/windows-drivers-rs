// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! A collection of macros used for writing WDF-based drivers in safe Rust

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{Error, Ident, ItemFn, ItemImpl, ItemStruct, Lit, Token, Type, meta::parser, parse_macro_input};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

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
/// 2. Tuple with GUID and flags: ("guid-string", [FLAG_ONE, FLAG_TWO]) or ("guid-string", [])
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
            
            let flags: Vec<String> = flags_punctuated.iter()
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
            
            Ok(TraceControlDef { guid, flags: Vec::new() })
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
fn validate_no_duplicate_flags(controls: &[TraceControlDef]) -> Result<(), (String, String, String)> {
    let mut seen_flags: HashSet<String> = HashSet::new();
    
    for control in controls {
        for flag in &control.flags {
            if !seen_flags.insert(flag.clone()) {
                // Find which GUID had this flag first
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

    let variants: Vec<VariantInfo> = trace_controls.iter()
        .enumerate()
        .flat_map(|(control_idx, tc)| {
            tc.flags.iter().enumerate().map(move |(flag_idx, flag_name)| {
                let variant_ident = Ident::new(
                    flag_name,
                    proc_macro2::Span::call_site(),
                );
                
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
    let enum_variants: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { #ident(usize, usize) }
    }).collect();

    // Generate name match arms for by_name lookup
    let name_match_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        let name = &v.flag_name;
        let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
        let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
        quote! { #name => Some(WppFlag::#ident(#ctrl_idx, #flag_idx)) }
    }).collect();

    // Generate static array entries
    let static_entries: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        let ctrl_idx = proc_macro2::Literal::usize_unsuffixed(v.control_idx);
        let flag_idx = proc_macro2::Literal::usize_unsuffixed(v.flag_idx);
        quote! { WppFlag::#ident(#ctrl_idx, #flag_idx) }
    }).collect();

    // Generate match arms for control_index
    let control_idx_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { WppFlag::#ident(ctrl, _) => *ctrl }
    }).collect();

    // Generate match arms for flag_index
    let flag_idx_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { WppFlag::#ident(_, flag) => *flag }
    }).collect();

    // Generate match arms for as_tuple
    let tuple_arms: Vec<proc_macro2::TokenStream> = variants.iter().map(|v| {
        let ident = &v.variant_ident;
        quote! { WppFlag::#ident(ctrl, flag) => (*ctrl, *flag) }
    }).collect();

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
            /// Critical errors that cause system failure (Fatal is an alias)
            Critical = 1,
            /// Non-fatal errors
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

        /// Fatal is an alias for Critical
        pub const Fatal: TraceLevel = TraceLevel::Critical;

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
                "Duplicate flag '{}' found in trace controls. First in GUID '{}', duplicate in GUID '{}'",
                flag, guid1, guid2
            )
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
    let codeview_annotations: Vec<proc_macro2::TokenStream> = trace_controls.iter().map(|tc| {
        let guid = &tc.guid;
        quote! {
            core::hint::codeview_annotation!("TMC:", #guid, "CtlGuid");
        }
    }).collect();

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

/// Represents the type of a trace argument for WPP tracing
#[derive(Debug, Clone)]
enum TraceArgType {
    /// Signed integer types up to 32 bits (i8, i16, i32) - maps to ItemLong
    Long,
    /// Unsigned integer types up to 32 bits (u8, u16, u32) - maps to ItemULong
    ULong,
    /// Signed integer types 33-64 bits (i64, isize) - maps to ItemLongLong
    LongLong,
    /// Unsigned integer types 33-64 bits (u64, usize) - maps to ItemULongLong
    ULongLong,
    /// String types (&str, String) - maps to ItemString  
    String,
    // Future: Add more types as needed
    // Pointer,
    // Guid,
    // etc.
}

impl TraceArgType {
    /// Returns the WPP item type name
    fn item_type_name(&self) -> &'static str {
        match self {
            TraceArgType::Long => "ItemLong",
            TraceArgType::ULong => "ItemULong",
            TraceArgType::LongLong => "ItemLongLong",
            TraceArgType::ULongLong => "ItemULongLong",
            TraceArgType::String => "ItemString",
        }
    }

    /// Returns the C-style format specifier
    fn format_specifier(&self) -> &'static str {
        match self {
            TraceArgType::Long => "d",
            TraceArgType::ULong => "u",
            TraceArgType::LongLong => "I64d",
            TraceArgType::ULongLong => "I64u",
            TraceArgType::String => "s",
        }
    }

    /// Returns the single-character suffix for method naming
    fn method_char(&self) -> char {
        match self {
            TraceArgType::Long => 'd',
            TraceArgType::ULong => 'u',
            TraceArgType::LongLong => 'D',
            TraceArgType::ULongLong => 'U',
            TraceArgType::String => 's',
        }
    }

    /// Returns the Rust type used for this trace argument
    fn rust_type(&self) -> &'static str {
        match self {
            TraceArgType::Long => "i32",
            TraceArgType::ULong => "u32",
            TraceArgType::LongLong => "i64",
            TraceArgType::ULongLong => "u64",
            TraceArgType::String => "LPCSTR",
        }
    }
}

/// Generates a method suffix from argument types (e.g., [Long, String] -> "ds")
fn generate_method_suffix(args: &[TraceArg]) -> String {
    args.iter().map(|a| a.arg_type.method_char()).collect()
}

/// Represents a parsed trace argument
struct TraceArg {
    name: String,
    arg_type: TraceArgType,
    /// The original expression for this argument
    expr: syn::Expr,
}

/// Represents a trace argument input that may have an explicit type annotation
/// Syntax: `expr` or `expr: Type`
struct TraceArgInput {
    expr: syn::Expr,
    explicit_type: Option<syn::Type>,
}

impl Parse for TraceArgInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let expr: syn::Expr = input.parse()?;
        
        // Check if there's a colon followed by a type
        let explicit_type = if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Some(input.parse::<Type>()?)
        } else {
            None
        };
        
        Ok(TraceArgInput { expr, explicit_type })
    }
}

/// Maps a syn::Type to a TraceArgType
/// Signed types up to 32 bits → Long, Unsigned up to 32 bits → ULong
/// Signed 64 bits → LongLong, Unsigned 64 bits → ULongLong
/// Types over 64 bits (i128, u128) are not supported
fn type_to_trace_arg_type(ty: &syn::Type) -> Result<TraceArgType, Error> {
    match ty {
        Type::Path(type_path) => {
            let ident = type_path.path.segments.last()
                .map(|s| s.ident.to_string());
            
            match ident.as_deref() {
                // Signed types up to 32 bits → Long
                Some("i8" | "i16" | "i32") => Ok(TraceArgType::Long),
                // Unsigned types up to 32 bits → ULong
                Some("u8" | "u16" | "u32") => Ok(TraceArgType::ULong),
                // Signed 64-bit types → LongLong (isize treated as 64-bit)
                Some("i64" | "isize") => Ok(TraceArgType::LongLong),
                // Unsigned 64-bit types → ULongLong (usize treated as 64-bit)
                Some("u64" | "usize") => Ok(TraceArgType::ULongLong),
                // Types over 64 bits are not supported
                Some("i128" | "u128") => Err(Error::new_spanned(
                    ty,
                    "128-bit integer types (i128, u128) are not supported for tracing"
                )),
                // String types
                Some("str" | "String") => Ok(TraceArgType::String),
                Some(other) => Err(Error::new_spanned(
                    ty,
                    format!("Unsupported type '{}' for tracing. Supported types: i8-i64, u8-u64, isize, usize, &str, String", other)
                )),
                None => Err(Error::new_spanned(ty, "Cannot determine type for tracing")),
            }
        }
        Type::Reference(type_ref) => {
            // Handle &str and &String
            type_to_trace_arg_type(&type_ref.elem)
        }
        _ => Err(Error::new_spanned(ty, "Unsupported type syntax for tracing. Supported types: integer types, &str, String")),
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
    "None", "Critical", "Fatal", "Error", "Warning", 
    "Information", "Verbose", "Reserved6", "Reserved7", "Reserved8", "Reserved9"
];

/// Parses the trace! macro input and extracts optional flag, optional level, format string and arguments
/// 
/// Supported syntaxes:
/// - `trace!("fmt", args...)` - no flag, no level (defaults to TraceLevel::None)
/// - `trace!(FLAG, "fmt", args...)` - with flag, no level
/// - `trace!(FLAG, Level, "fmt", args...)` - with flag and level
/// - `trace!(Level, "fmt", args...)` - with level only (uses default control block)
fn parse_trace_args(item: TokenStream) -> Result<ParsedTraceArgs, Error> {
    use syn::{Expr, ExprLit, punctuated::Punctuated};
    
    // Parse as comma-separated TraceArgInput items
    let args = syn::parse::Parser::parse(
        Punctuated::<TraceArgInput, Token![,]>::parse_terminated,
        item,
    )?;
    
    // Convert to Vec for easier manipulation
    let mut all_args: Vec<TraceArgInput> = args.into_iter().collect();
    
    if all_args.is_empty() {
        return Err(Error::new(proc_macro2::Span::call_site(), "trace! requires at least a format string"));
    }
    
    let mut flag: Option<syn::Ident> = None;
    let mut level: Option<syn::Ident> = None;
    let format_str: String;
    let remaining_start_idx: usize;
    
    // Determine the pattern based on the first few arguments
    let first_is_path = matches!(&all_args[0].expr, Expr::Path(_));
    let first_is_string = matches!(&all_args[0].expr, Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));
    
    if first_is_string {
        // Pattern: trace!("fmt", args...)
        format_str = match &all_args[0].expr {
            Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
            _ => unreachable!(),
        };
        remaining_start_idx = 1;
    } else if first_is_path && all_args.len() >= 2 {
        let second_is_string = matches!(&all_args[1].expr, Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));
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
                Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
                _ => unreachable!(),
            };
            remaining_start_idx = 2;
        } else if second_is_path && all_args.len() >= 3 {
            let third_is_string = matches!(&all_args[2].expr, Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));
            
            if third_is_string {
                // Pattern: trace!(FLAG, Level, "fmt", ...)
                let first_ident = extract_path_ident(&all_args[0].expr)?;
                let second_ident = extract_path_ident(&all_args[1].expr)?;
                
                // Second should be a level
                let second_str = second_ident.to_string();
                if !TRACE_LEVEL_NAMES.contains(&second_str.as_str()) {
                    return Err(Error::new_spanned(&all_args[1].expr, 
                        format!("Expected a trace level (one of: {:?}), got '{}'", TRACE_LEVEL_NAMES, second_str)));
                }
                
                flag = Some(first_ident);
                level = Some(second_ident);
                
                format_str = match &all_args[2].expr {
                    Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
                    _ => unreachable!(),
                };
                remaining_start_idx = 3;
            } else {
                return Err(Error::new_spanned(&all_args[0].expr, "Expected a format string"));
            }
        } else {
            return Err(Error::new_spanned(&all_args[0].expr, "Expected a format string"));
        }
    } else {
        return Err(Error::new_spanned(&all_args[0].expr, "Expected a format string or flag identifier"));
    }
    
    // Remaining arguments are the trace parameters (with optional type annotations)
    let mut trace_args = Vec::new();
    for (idx, arg_input) in all_args.into_iter().skip(remaining_start_idx).enumerate() {
        let (name, arg_type) = infer_arg_info(&arg_input.expr, idx, arg_input.explicit_type.as_ref())?;
        trace_args.push(TraceArg { name, arg_type, expr: arg_input.expr });
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

/// Infers the argument name and type from an expression
/// If explicit_type is provided, uses that instead of inferring
fn infer_arg_info(
    expr: &syn::Expr,
    idx: usize,
    explicit_type: Option<&syn::Type>,
) -> Result<(String, TraceArgType), Error> {
    use syn::Expr;
    
    // Extract argument name from expression
    let name = match expr {
        Expr::Path(p) => {
            // Variable reference like `my_var`
            p.path.segments.last()
                .map(|s| s.ident.to_string())
                .unwrap_or_else(|| format!("arg{}", idx))
        }
        Expr::Lit(lit) => {
            // Literal value
            match &lit.lit {
                Lit::Int(_) => format!("literal{}", idx),
                Lit::Str(_) => format!("str{}", idx),
                _ => format!("arg{}", idx),
            }
        }
        Expr::Reference(r) => {
            // Reference like `&my_var`
            if let Expr::Path(p) = r.expr.as_ref() {
                p.path.segments.last()
                    .map(|s| s.ident.to_string())
                    .unwrap_or_else(|| format!("arg{}", idx))
            } else {
                format!("arg{}", idx)
            }
        }
        Expr::Field(f) => {
            // Field access like `obj.field`
            match &f.member {
                syn::Member::Named(ident) => ident.to_string(),
                syn::Member::Unnamed(index) => format!("field{}", index.index),
            }
        }
        Expr::MethodCall(m) => {
            // Method call like `obj.method()`
            m.method.to_string()
        }
        _ => format!("arg{}", idx),
    };
    
    // If explicit type is provided, use it
    if let Some(ty) = explicit_type {
        let arg_type = type_to_trace_arg_type(ty)?;
        return Ok((name, arg_type));
    }
    
    // Otherwise, infer type based on expression structure
    let arg_type = match expr {
        Expr::Lit(lit) => {
            match &lit.lit {
                Lit::Str(_) => TraceArgType::String,
                Lit::Int(_) => TraceArgType::ULong,
                _ => return Err(Error::new_spanned(lit, "Unsupported literal type for tracing")),
            }
        }
        Expr::Reference(r) => {
            // Match only &str (reference to a string literal)
            match r.expr.as_ref() {
                Expr::Lit(lit) if matches!(&lit.lit, Lit::Str(_)) => TraceArgType::String,
                _ => return Err(Error::new_spanned(
                    expr,
                    "Cannot infer type for reference. Add explicit type annotation: `&var: &str`"
                )),
            }
        }
        Expr::Path(_) => {
            return Err(Error::new_spanned(
                expr,
                "Cannot infer type for variable. Add explicit type annotation, e.g.: `my_var: i32` or `my_var: &str`"
            ));
        }
        Expr::Field(_) => {
            return Err(Error::new_spanned(
                expr,
                "Cannot infer type for field access. Add explicit type annotation, e.g.: `obj.field: i32`"
            ));
        }
        Expr::MethodCall(_) => {
            return Err(Error::new_spanned(
                expr,
                "Cannot infer type for method call. Add explicit type annotation, e.g.: `obj.method(): i32`"
            ));
        }
        _ => return Err(Error::new_spanned(
            expr,
            "Cannot infer type for this expression. Add explicit type annotation, e.g.: `expr: i32` or `expr: &str`"
        )),
    };
    
    Ok((name, arg_type))
}

/// Generates the WPP-style format string with numbered placeholders
fn generate_wpp_format_string(format_str: &str, args: &[TraceArg]) -> String {
    let mut result = String::new();
    let mut arg_idx = 0;
    let mut chars = format_str.chars().peekable();
    
    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                // Escaped {{ -> {
                chars.next();
                result.push('{');
            } else {
                // Format placeholder {}
                // Skip until closing }
                while let Some(inner) = chars.next() {
                    if inner == '}' {
                        break;
                    }
                }
                
                if arg_idx < args.len() {
                    let param_num = 10 + arg_idx;
                    let spec = args[arg_idx].arg_type.format_specifier();
                    result.push_str(&format!("%{}!{}!", param_num, spec));
                    arg_idx += 1;
                }
            }
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
                // Escaped }} -> }
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

/// A procedural macro for WPP-style tracing that generates codeview annotations.
///
/// # Usage
/// 
/// ## Basic usage (no flag, no level - uses default control block and TraceLevel::None):
/// ```ignore
/// trace!("My message with value: {} and string: {}", my_int: i32, my_string: &str);
/// ```
///
/// ## With flag only:
/// ```ignore
/// trace!(FLAG_ONE, "Message with flag {}", value: i32);
/// ```
///
/// ## With flag and level:
/// ```ignore
/// trace!(FLAG_ONE, Information, "Informational message {}", value: i32);
/// ```
///
/// ## With level only (uses default control block):
/// ```ignore
/// trace!(Warning, "Warning message {}", value: i32);
/// ```
///
/// This generates a `codeview_annotation!` call with WPP trace metadata.
/// The wpp_trace_message is conditionally called based on the flag's control block.
/// The wpp_auto_log_trace is conditionally called based on level (skipped for Verbose
/// unless AutoLogVerboseEnabled is set on the control block).
#[proc_macro]
pub fn trace(item: TokenStream) -> TokenStream {
    let span = proc_macro::Span::call_site();
    
    // Get source file information
    let source_file = span.file();
    let file_name = std::path::Path::new(&source_file)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown.rs");
    let line_number = span.line();
    
    // Parse the macro arguments
    let parsed = match parse_trace_args(item) {
        Ok(parsed) => parsed,
        Err(e) => return e.to_compile_error().into(),
    };
    
    let ParsedTraceArgs { flag, level, format_str, args } = parsed;
    
    // Generate unique message ID
    let message_id = TRACE_MESSAGE_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    
    // Get driver/crate name from environment
    let driver_name = std::env::var("CARGO_PKG_NAME")
        .unwrap_or_else(|_| "unknown_driver".to_string())
        .replace('-', "_");
    
    // TODO: Get actual msg GUID - for now use a placeholder
    let msg_guid = "e7602a7b-5034-321b-d450-a986113fc2e1";
    
    // Build the codeview_annotation parameters
    // Parameter 2: "<guid> <driver_name> // SRC=<filename> MJ= MN="
    let param2 = format!("{} {} // SRC={} MJ= MN=", msg_guid, driver_name, file_name);
    
    // Parameter 3: "#typev <driver_name>_<line> <msg_id> \"<format_with_placeholders>\""
    let wpp_format = generate_wpp_format_string(&format_str, &args);
    let typev_name = format!("{}_{}", driver_name, line_number);
    let param3 = format!("#typev {} {} \"%0{}\"", typev_name, message_id, wpp_format);
    
    // Build argument descriptors (parameters 5..n-1)
    let arg_descriptors: Vec<String> = args.iter().enumerate().map(|(idx, arg)| {
        let param_num = 10 + idx;
        format!("{}, {} -- {}", arg.name, arg.arg_type.item_type_name(), param_num)
    }).collect();
    
    // Generate the codeview_annotation! call
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
    
    // Generate method suffix based on argument types
    let method_suffix = generate_method_suffix(&args);
    let method_name = format!("trace_{}", if method_suffix.is_empty() { "empty".to_string() } else { method_suffix.clone() });
    let method_ident = syn::Ident::new(&method_name, proc_macro2::Span::call_site());
    
    // Create unique trait name per signature to avoid conflicts
    let trait_suffix = if method_suffix.is_empty() { "Empty".to_string() } else { method_suffix.to_uppercase() };
    let trait_name = format!("TraceWriterExt{}", trait_suffix);
    let trait_ident = syn::Ident::new(&trait_name, proc_macro2::Span::call_site());
    
    // Always generate the trait - Rust will handle duplicate definitions
    let trait_and_impl = generate_trace_writer_ext_method(&method_ident, &trait_ident, &args);
    
    // Generate the method call with arguments (now with flag and level)
    let method_call = generate_trace_method_call(&method_ident, &trait_ident, message_id as u16, &args, flag.as_ref(), level.as_ref());
    
    let expanded = quote! {
        core::hint::codeview_annotation!(#(#annotation_args),*);
        #trait_and_impl
        #method_call
    };
    
    expanded.into()
}

/// Generates the TraceWriterExt trait method definition and implementation
fn generate_trace_writer_ext_method(
    method_ident: &syn::Ident,
    trait_ident: &syn::Ident,
    args: &[TraceArg],
) -> proc_macro2::TokenStream {
    // Generate parameter declarations for the trait method
    let param_decls: Vec<proc_macro2::TokenStream> = args.iter().enumerate().map(|(idx, arg)| {
        let param_name = syn::Ident::new(&format!("a{}", idx + 1), proc_macro2::Span::call_site());
        match arg.arg_type {
            TraceArgType::Long => quote! { #param_name: i32 },
            TraceArgType::ULong => quote! { #param_name: u32 },
            TraceArgType::LongLong => quote! { #param_name: i64 },
            TraceArgType::ULongLong => quote! { #param_name: u64 },
            TraceArgType::String => quote! { #param_name: ::wdf::__internal::LPCSTR },
        }
    }).collect();
    
    // Generate length calculations for each argument
    let len_calculations: Vec<proc_macro2::TokenStream> = args.iter().enumerate().map(|(idx, arg)| {
        let param_name = syn::Ident::new(&format!("a{}", idx + 1), proc_macro2::Span::call_site());
        let len_name = syn::Ident::new(&format!("a{}_len", idx + 1), proc_macro2::Span::call_site());
        match arg.arg_type {
            TraceArgType::Long => quote! {
                let #len_name = core::mem::size_of::<i32>();
            },
            TraceArgType::ULong => quote! {
                let #len_name = core::mem::size_of::<u32>();
            },
            TraceArgType::LongLong => quote! {
                let #len_name = core::mem::size_of::<i64>();
            },
            TraceArgType::ULongLong => quote! {
                let #len_name = core::mem::size_of::<u64>();
            },
            TraceArgType::String => quote! {
                let #param_name = if !#param_name.is_null() {
                    #param_name
                } else {
                    c"NULL".as_ptr()
                };
                let #len_name = unsafe { ::wdf::__internal::strlen(#param_name) + 1 };
            },
        }
    }).collect();
    
    // Generate argument pairs for wpp_trace_message and WppAutoLogTrace
    let arg_pairs: Vec<proc_macro2::TokenStream> = args.iter().enumerate().map(|(idx, arg)| {
        let param_name = syn::Ident::new(&format!("a{}", idx + 1), proc_macro2::Span::call_site());
        let len_name = syn::Ident::new(&format!("a{}_len", idx + 1), proc_macro2::Span::call_site());
        match arg.arg_type {
            TraceArgType::Long | TraceArgType::ULong | 
            TraceArgType::LongLong | TraceArgType::ULongLong => quote! { &#param_name, #len_name, },
            TraceArgType::String => quote! { #param_name, #len_name, },
        }
    }).collect();
    
    quote! {
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        trait #trait_ident {
            fn #method_ident(
                &self,
                level: ::wdf::__internal::UCHAR,
                flags: ::wdf::__internal::ULONG,
                id: ::wdf::__internal::USHORT,
                trace_guid: ::wdf::__internal::LPCGUID,
                should_trace_wpp: bool,
                should_auto_log: bool,
                #(#param_decls),*
            );
        }
        
        impl #trait_ident for ::wdf::tracing::TraceWriter {
            fn #method_ident(
                &self,
                level: ::wdf::__internal::UCHAR,
                flags: ::wdf::__internal::ULONG,
                id: ::wdf::__internal::USHORT,
                trace_guid: ::wdf::__internal::LPCGUID,
                should_trace_wpp: bool,
                should_auto_log: bool,
                #(#param_decls),*
            ) {
                #(#len_calculations)*
                
                unsafe {
                    // Only call wpp_trace_message if should_trace_wpp is true
                    if should_trace_wpp {
                        let logger = ::wdf::__internal::get_wpp_logger().unwrap();
                        let _ = ::wdf::__internal::get_wpp_trace_message().unwrap()(
                            logger,
                            ::wdf::__internal::WPP_TRACE_OPTIONS,
                            trace_guid,
                            id,
                            #(#arg_pairs)*
                            core::ptr::null::<core::ffi::c_void>(), // sentinel
                        );
                    }

                    // Only call auto log trace if should_auto_log is true
                    if should_auto_log {
                        let auto_log_context = ::wdf::__internal::get_auto_log_context().unwrap();
                        let _ = ::wdf::__internal::WppAutoLogTrace(
                            auto_log_context,
                            level,
                            flags,
                            trace_guid.cast_mut().cast(),
                            id,
                            #(#arg_pairs)*
                            core::ptr::null::<core::ffi::c_void>(), // sentinel
                        );
                    }
                }
            }
        }
    }
}

/// Generates the method call to the TraceWriterExt method
fn generate_trace_method_call(
    method_ident: &syn::Ident,
    trait_ident: &syn::Ident,
    message_id: u16,
    args: &[TraceArg],
    flag: Option<&syn::Ident>,
    level: Option<&syn::Ident>,
) -> proc_macro2::TokenStream {
    // Generate argument conversions using the original expressions
    let arg_conversions: Vec<proc_macro2::TokenStream> = args.iter().enumerate().map(|(idx, arg)| {
        let var_name = syn::Ident::new(&format!("__trace_arg{}", idx), proc_macro2::Span::call_site());
        let expr = &arg.expr;
        match arg.arg_type {
            TraceArgType::Long => quote! {
                let #var_name: i32 = (#expr) as i32;
            },
            TraceArgType::ULong => quote! {
                let #var_name: u32 = (#expr) as u32;
            },
            TraceArgType::LongLong => quote! {
                let #var_name: i64 = (#expr) as i64;
            },
            TraceArgType::ULongLong => quote! {
                let #var_name: u64 = (#expr) as u64;
            },
            TraceArgType::String => quote! {
                let #var_name = alloc::ffi::CString::new(#expr).unwrap();
            },
        }
    }).collect();
    
    let arg_values: Vec<proc_macro2::TokenStream> = args.iter().enumerate().map(|(idx, arg)| {
        let var_name = syn::Ident::new(&format!("__trace_arg{}", idx), proc_macro2::Span::call_site());
        match arg.arg_type {
            TraceArgType::Long | TraceArgType::ULong |
            TraceArgType::LongLong | TraceArgType::ULongLong => quote! { #var_name },
            TraceArgType::String => quote! { #var_name.as_ptr() },
        }
    }).collect();
    
    let message_id_lit = proc_macro2::Literal::u16_unsuffixed(message_id);
    
    // Generate level value
    let level_value = if let Some(level_ident) = level {
        let level_str = level_ident.to_string();
        if level_str == "Fatal" {
            // Fatal is a constant alias for Critical
            quote! { Fatal as u8 }
        } else {
            quote! { TraceLevel::#level_ident as u8 }
        }
    } else {
        quote! { TraceLevel::None as u8 }
    };
    
    // Generate flags value and control index from the WppFlag
    // Use WppFlag::by_name to look up the flag at compile time
    let (flags_value, control_index) = if let Some(flag_ident) = flag {
        let flag_name = flag_ident.to_string();
        (
            quote! { 
                {
                    let __flag = WppFlag::by_name(#flag_name).expect("Unknown WppFlag");
                    1u32 << __flag.flag_index()
                }
            },
            quote! {
                {
                    let __flag = WppFlag::by_name(#flag_name).expect("Unknown WppFlag");
                    __flag.control_index()
                }
            }
        )
    } else {
        (quote! { 0u32 }, quote! { 0usize })
    };

    quote! {
        {
            #(#arg_conversions)*
            
            // Determine if we should trace based on flag's control block
            let __trace_level: u8 = #level_value;
            let __trace_flags: u32 = #flags_value;
            let __control_index: usize = #control_index;
            
            if let Some(__trace_writer) = ::wdf::get_trace_writer() {
                // Check if the flag is enabled in the control block
                let __should_trace_wpp = __trace_writer.is_flag_enabled(__control_index, __trace_flags);
                
                // Check if we should call auto log trace
                // Skip if level > Verbose (5) and AutoLogVerboseEnabled is false
                let __should_auto_log = __trace_level < 5 || __trace_writer.is_auto_log_verbose_enabled(__control_index);
                
                <::wdf::tracing::TraceWriter as #trait_ident>::#method_ident(
                    __trace_writer,
                    __trace_level,
                    __trace_flags,
                    #message_id_lit,
                    &::wdf::__internal::TRACE_GUID,
                    __should_trace_wpp,
                    __should_auto_log,
                    #(#arg_values),*
                );
            }
        }
    }
}
