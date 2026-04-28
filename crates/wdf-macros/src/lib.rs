// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! A collection of macros used for writing WDF-based drivers in safe Rust

use std::collections::HashSet;
use std::sync::Mutex;

use proc_macro::TokenStream;
use quote::quote;
use syn::{Error, Ident, ItemFn, ItemImpl, ItemStruct, Lit, Token, meta::parser, parse_macro_input};
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

/// Represents a single trace control definition with provider name, GUID and optional flags
#[derive(Debug, Clone)]
struct TraceControlDef {
    provider_name: String,
    guid: String,
    flags: Vec<String>,
}

/// Parser for trace_control argument
/// Supports two syntaxes:
/// 1. Provider name and GUID (no flags): ("ProviderName", "guid-string")
/// 2. Provider name, GUID and flags: ("ProviderName", "guid-string", [FLAG_ONE, FLAG_TWO])
impl Parse for TraceControlDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Tuple format: ("provider_name", "guid") or ("provider_name", "guid", [FLAGS...])
        let content;
        syn::parenthesized!(content in input);
        
        // Parse provider name string
        let provider_name_lit: syn::LitStr = content.parse()?;
        let provider_name = provider_name_lit.value();
        
        if provider_name.is_empty() {
            return Err(Error::new_spanned(provider_name_lit, "Provider name must not be empty"));
        }
        
        // Parse comma separator
        content.parse::<Token![,]>()?;
        
        // Parse GUID string
        let guid_lit: syn::LitStr = content.parse()?;
        let guid = guid_lit.value();
        
        if !is_valid_guid(&guid) {
            return Err(Error::new_spanned(guid_lit, "Not a valid GUID"));
        }
        
        // Check if there are flags (optional third element)
        let flags = if content.peek(Token![,]) {
            content.parse::<Token![,]>()?;
            
            // Parse flag array [FLAG_ONE, FLAG_TWO, ...] or []
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
    
    // TODO: TraceLevel enum declaration generation is skipped when flags are absent. Fix this as we 
    // want TraceLevel to be available regardless of flags.
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
/// ## Provider name and GUID (no flags):
/// ```ignore
/// #[driver_entry(trace_control = ("MyProvider", "guid-string"))]
/// ```
/// 
/// ## Provider name, GUID with flags:
/// ```ignore
/// #[driver_entry(trace_control = ("MyProvider", "guid-string", [FLAG_ONE, FLAG_TWO]))]
/// ```
/// 
/// ## Provider name, GUID with empty flags:
/// ```ignore
/// #[driver_entry(trace_control = ("MyProvider", "guid-string", []))]
/// ```
/// 
/// ## Multiple trace controls:
/// ```ignore
/// #[driver_entry(trace_control = ("Prov1", "guid1", [FLAG_A, FLAG_B]), ("Prov2", "guid2", [FLAG_C, FLAG_D]))]
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
        let provider_name = &tc.provider_name;
        let flags = &tc.flags;
        quote! {
            core::hint::codeview_annotation!("TMC:", #guid, #provider_name, #(#flags),*);
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

/// Generates a deterministic 16-byte GUID from the source file path using
/// SHA-256.
///
/// SHA-256 hashes the normalised source path and the first 16 bytes of the
/// digest become the GUID. The same file path always produces the same GUID
/// regardless of toolchain version, compilation order, or host platform.
fn generate_deterministic_guid(source_file: &str) -> [u8; 16] {
    use sha2::{Sha256, Digest};

    // Normalise path separators so Windows and Unix paths hash identically.
    let normalised = source_file.replace('\\', "/");

    let hash = Sha256::digest(normalised.as_bytes());
    let mut bytes = [0u8; 16];
    bytes.copy_from_slice(&hash[..16]);
    bytes
}

/// Formats 16 raw GUID bytes as a standard `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` string.
fn format_guid(b: &[u8; 16]) -> String {
    format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        b[0], b[1], b[2], b[3],
        b[4], b[5],
        b[6], b[7],
        b[8], b[9],
        b[10], b[11], b[12], b[13], b[14], b[15],
    )
}

/// Generates a `proc_macro2::TokenStream` that constructs a `::wdk_sys::GUID`
/// struct literal from raw bytes, suitable for embedding in generated code.
fn guid_to_tokens(b: &[u8; 16]) -> proc_macro2::TokenStream {
    let data1 = u32::from_be_bytes([b[0], b[1], b[2], b[3]]);
    let data2 = u16::from_be_bytes([b[4], b[5]]);
    let data3 = u16::from_be_bytes([b[6], b[7]]);
    let d4_0 = b[8];
    let d4_1 = b[9];
    let d4_2 = b[10];
    let d4_3 = b[11];
    let d4_4 = b[12];
    let d4_5 = b[13];
    let d4_6 = b[14];
    let d4_7 = b[15];

    quote! {
        ::wdk_sys::GUID {
            Data1: #data1,
            Data2: #data2,
            Data3: #data3,
            Data4: [#d4_0, #d4_1, #d4_2, #d4_3, #d4_4, #d4_5, #d4_6, #d4_7],
        }
    }
}

/// Represents the type of a trace argument for WPP tracing, derived from
/// C-style format specifiers as defined in defaultwpp.ini.
///
/// Supports both short-form (`%d`, `%I64u`) and long-form (`%!STATUS!`, `%!GUID!`)
/// format specifiers.
#[derive(Debug, Clone)]
struct CFormatSpec {
    /// The format specifier name as it appears in the format string (e.g. "d", "I64u", "STATUS")
    name: String,
    /// The WPP/ETW item type name (e.g. "ItemLong", "ItemNTSTATUS")
    etw_type: String,
    /// The format spec string to use in the TMF annotation (e.g. "d", "I64u", "s")
    format_spec: String,
    /// The Rust assertion type - the type we assign the trace arg to for compile-time checking
    rust_assert_type: String,
    /// Whether this is a "complex" type that needs special handling (strings, GUID, etc.)
    is_complex: bool,
}

/// Returns the mapping from C format specifier names to [`CFormatSpec`].
///
/// The map is derived from the `DEFINE_SIMPLE_TYPE`, `DEFINE_FLAVOR`, and
/// `DEFINE_CPLX_TYPE` definitions in `defaultwpp.ini` and is identical for
/// every `trace!` invocation, so it is built lazily once per `rustc` process
/// via [`OnceLock`] and then handed out as a shared `&'static` reference.
fn format_spec_map() -> &'static std::collections::HashMap<String, CFormatSpec> {
    static SPEC_MAP: std::sync::OnceLock<std::collections::HashMap<String, CFormatSpec>> =
        std::sync::OnceLock::new();
    SPEC_MAP.get_or_init(build_format_spec_map)
}

/// Builds the mapping from C format specifier names to CFormatSpec.
/// This is derived from the DEFINE_SIMPLE_TYPE, DEFINE_FLAVOR, and DEFINE_CPLX_TYPE
/// definitions in defaultwpp.ini.
///
/// Prefer [`format_spec_map`] over calling this directly — it caches the
/// result so the (~80-entry) map is only constructed once per process.
fn build_format_spec_map() -> std::collections::HashMap<String, CFormatSpec> {
    let mut map = std::collections::HashMap::new();

    // Helper to insert a spec
    macro_rules! spec {
        ($name:expr, $etw:expr, $fmt:expr, $rust_ty:expr) => {
            map.insert($name.to_string(), CFormatSpec {
                name: $name.to_string(),
                etw_type: $etw.to_string(),
                format_spec: $fmt.to_string(),
                rust_assert_type: $rust_ty.to_string(),
                is_complex: false,
            });
        };
        ($name:expr, $etw:expr, $fmt:expr, $rust_ty:expr, complex) => {
            map.insert($name.to_string(), CFormatSpec {
                name: $name.to_string(),
                etw_type: $etw.to_string(),
                format_spec: $fmt.to_string(),
                rust_assert_type: $rust_ty.to_string(),
                is_complex: true,
            });
        };
    }

    // ---- Short-form specifiers (DEFINE_FLAVOR entries from defaultwpp.ini) ----
    
    // Char — %c can be used with both i8 and u8; we map to u8 since that's
    // the more common Rust unsigned byte type (UBYTE in defaultwpp.ini)
    spec!("c", "ItemChar", "c", "u8");
    spec!("hc", "ItemChar", "c", "u8");

    // Signed short
    spec!("hi", "ItemShort", "hd", "i16");
    spec!("hd", "ItemShort", "hd", "i16");

    // Unsigned short
    spec!("hu", "ItemShort", "hu", "u16");
    spec!("hx", "ItemShort", "x", "u16");
    spec!("hX", "ItemShort", "X", "u16");
    spec!("ho", "ItemShort", "o", "u16");

    // Signed int (32-bit)
    spec!("i", "ItemLong", "d", "i32");
    spec!("d", "ItemLong", "d", "i32");

    // Unsigned int (32-bit)
    spec!("u", "ItemLong", "u", "u32");
    spec!("x", "ItemLong", "x", "u32");
    spec!("X", "ItemLong", "X", "u32");
    spec!("o", "ItemLong", "o", "u32");

    // Signed long (32-bit, same as int on Windows)
    spec!("li", "ItemLong", "d", "i32");
    spec!("ld", "ItemLong", "d", "i32");

    // Unsigned long (32-bit)
    spec!("lu", "ItemLong", "u", "u32");
    spec!("lx", "ItemLong", "x", "u32");
    spec!("lX", "ItemLong", "X", "u32");
    spec!("lo", "ItemLong", "o", "u32");

    // Signed 64-bit
    spec!("I64d", "ItemLongLong", "I64d", "i64");
    spec!("lld", "ItemLongLong", "I64d", "i64");

    // Unsigned 64-bit
    spec!("I64u", "ItemULongLong", "I64u", "u64");
    spec!("llu", "ItemULongLong", "I64u", "u64");

    // Hex 64-bit
    spec!("I64x", "ItemLongLongX", "I64x", "i64");
    spec!("I64X", "ItemLongLongXX", "I64X", "i64");
    spec!("I64o", "ItemLongLongO", "I64o", "i64");
    spec!("llx", "ItemLongLongX", "I64x", "i64");
    spec!("llX", "ItemLongLongXX", "I64X", "i64");
    spec!("llo", "ItemLongLongO", "I64o", "i64");

    // Pointer-sized (signed vs unsigned)
    spec!("Id", "ItemPtr", "Id", "isize");
    spec!("Iu", "ItemPtr", "Iu", "usize");
    spec!("Ix", "ItemPtr", "Ix", "usize");
    spec!("IX", "ItemPtr", "IX", "usize");
    spec!("Io", "ItemPtr", "Io", "usize");

    // Pointer
    spec!("p", "ItemPtr", "p", "usize");

    // Strings
    spec!("s", "ItemString", "s", "&str", complex);
    spec!("hs", "ItemString", "s", "&str", complex);
    spec!("S", "ItemWString", "s", "&str", complex);
    // TODO: ws, ls?

    // Double/float
    spec!("e", "ItemDouble", "s", "f64");
    spec!("E", "ItemDouble", "s", "f64");
    spec!("f", "ItemDouble", "s", "f64");
    spec!("g", "ItemDouble", "s", "f64");
    spec!("G", "ItemDouble", "s", "f64");

    // ---- Long-form specifiers (%!NAME!) from defaultwpp.ini ----

    // Basic integer types (long form)
    spec!("SINT", "ItemLong", "d", "i32");
    spec!("UINT", "ItemLong", "u", "u32");
    spec!("SINT64", "ItemLongLong", "I64d", "i64");
    spec!("UINT64", "ItemULongLong", "I64u", "u64");
    spec!("SBYTE", "ItemChar", "c", "i8");
    spec!("UBYTE", "ItemChar", "c", "u8");
    spec!("SSHORT", "ItemShort", "hd", "i16");
    spec!("USHORT", "ItemShort", "hu", "u16");
    spec!("SLONG", "ItemLong", "ld", "i32");
    spec!("ULONG", "ItemLong", "lu", "u32");
    spec!("DOUBLE", "ItemDouble", "s", "f64");

    // Hex display variants
    spec!("XINT", "ItemLong", "08x", "i32");
    spec!("OINT", "ItemLong", "o", "i32");
    spec!("XLONG", "ItemLong", "08lX", "i32");
    spec!("OLONG", "ItemLong", "lo", "i32");
    spec!("XSHORT", "ItemShort", "04hX", "i16");
    spec!("OSHORT", "ItemShort", "ho", "i16");
    spec!("XBYTE", "ItemChar", "02x", "i8");
    spec!("OBYTE", "ItemChar", "o", "i8");
    spec!("XINT64", "ItemLongLongX", "I64x", "i64");
    spec!("XXINT64", "ItemLongLongXX", "I64X", "i64");
    spec!("OINT64", "ItemLongLongO", "I64o", "i64");

    // Pointer types
    spec!("PTR", "ItemPtr", "p", "usize");
    spec!("HANDLE", "ItemPtr", "p", "usize");
    spec!("SLONGPTR", "ItemPtr", "Id", "isize");
    spec!("ULONGPTR", "ItemPtr", "Iu", "usize");
    spec!("XLONGPTR", "ItemPtr", "Ix", "isize");
    spec!("OLONGPTR", "ItemPtr", "Io", "isize");

    // Special format types (map to i32/u32 with special ETW decoding)
    spec!("STATUS", "ItemNTSTATUS", "s", "NtStatus");
    spec!("status", "ItemNTSTATUS", "s", "NtStatus");
    spec!("HRESULT", "ItemHRESULT", "s", "HResult");
    spec!("hresult", "ItemHRESULT", "s", "HResult");
    spec!("WINERROR", "ItemWINERROR", "s", "u32");
    spec!("winerr", "ItemWINERROR", "s", "u32");
    spec!("NDIS_STATUS", "ItemNDIS_STATUS", "s", "i32");
    spec!("NDIS_OID", "ItemNDIS_OID", "s", "u16");

    // GUID
    spec!("GUID", "ItemGuid", "s", "Guid", complex);
    spec!("guid", "ItemGuid", "s", "Guid", complex);
    spec!("CLSID", "ItemCLSID", "s", "Guid", complex);
    spec!("IID", "ItemIID", "s", "Guid", complex);

    // String types (long form)
    spec!("ASTR", "ItemString", "s", "&str", complex); // TODO: WPP_LOGASTR?
    spec!("WSTR", "ItemWString", "s", "&str", complex); // TODO: WPP_LOGWSTR?
    // TODO: Other types ARSTR, ARWSTR, CSTR, USTR, ANSTR, sid, BIN?

    // Network types
    spec!("IPADDR", "ItemIPAddr", "s", "u32");
    spec!("ipaddr", "ItemIPAddr", "s", "u32");
    spec!("PORT", "ItemPort", "s", "u16");
    spec!("port", "ItemPort", "s", "u16");

    // Boolean types
    spec!("bool", "ItemListLong(false,true)", "s", "bool");
    spec!("BOOLEAN", "ItemListByte(FALSE,TRUE)", "s", "bool");

    // Time types (all map to i64)
    spec!("TIMESTAMP", "ItemTimestamp", "s", "i64");
    spec!("TIME", "ItemTimestamp", "s", "i64");
    spec!("DATE", "ItemTimestamp", "s", "i64");
    spec!("WAITTIME", "ItemTimestamp", "s", "i64");

    // Custom Display types — any struct implementing core::fmt::Display
    // Usage: trace!("my obj: %!DISPLAY!", my_struct);
    // The macro formats the value via Display, collects into TraceFmtBuf,
    // and sends as ItemString. One Vec allocation, zero copies.
    spec!("DISPLAY", "ItemString", "s", "Display", complex);
    spec!("display", "ItemString", "s", "Display", complex);

    map
}

/// Parses a C-style format string and extracts format specifiers.
///
/// Supports:
/// - Short form: `%d`, `%I64u`, `%hu`, `%p`, `%s`
/// - Long form (shrieks): `%!STATUS!`, `%!GUID!`, `%!HRESULT!`
/// - Numbered: `%10!d!`, `%11!STATUS!`
/// - Escaped: `%%` (literal percent)
///
/// Returns the list of CFormatSpec for each format specifier found.
fn parse_c_format_string(format_str: &str) -> Result<Vec<CFormatSpec>, String> {
    let spec_map = format_spec_map();
    let mut specs = Vec::new();
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '%' {
            continue;
        }

        // Check for escaped %%
        if chars.peek() == Some(&'%') {
            chars.next();
            continue;
        }

        // Skip %0 (STDPREFIX) at start
        // Skip width/flag digits and prefix chars like '-', '+', '#', '.'
        while chars.peek().map_or(false, |c| matches!(c, '-' | '+' | '#' | '0'..='9' | '.')) {
            chars.next();
        }

        // Now determine if this is short-form or long-form (shriek)
        if chars.peek() == Some(&'!') {
            // Long form: %!NAME! or %NUM!NAME!
            chars.next(); // consume first '!'
            let mut name = String::new();
            while let Some(&ch) = chars.peek() {
                if ch == '!' {
                    chars.next(); // consume closing '!'
                    break;
                }
                name.push(ch);
                chars.next();
            }

            if name.is_empty() {
                continue; // skip empty %!!
            }

            match spec_map.get(&name) {
                Some(spec) => specs.push(spec.clone()),
                None => return Err(format!("Unknown format specifier '%!{}!'", name)),
            }
        } else {
            // Short form: collect the type specifier
            // Short type names: optional length prefix (I64, I, h, l, ll, w) + single specifier char
            let mut name = String::new();
            
            // Collect length prefix
            if chars.peek() == Some(&'I') {
                name.push('I');
                chars.next();
                // Check for I64
                if chars.peek() == Some(&'6') {
                    name.push('6');
                    chars.next();
                    if chars.peek() == Some(&'4') {
                        name.push('4');
                        chars.next();
                    }
                }
            } else if chars.peek() == Some(&'l') {
                name.push('l');
                chars.next();
                // Check for ll
                if chars.peek() == Some(&'l') {
                    name.push('l');
                    chars.next();
                }
            } else if chars.peek() == Some(&'h') {
                name.push('h');
                chars.next();
            } else if chars.peek() == Some(&'w') {
                name.push('w');
                chars.next();
            }

            // Now the specifier character
            if let Some(&ch) = chars.peek() {
                if ch.is_alphabetic() {
                    name.push(ch);
                    chars.next();
                }
            }

            if name.is_empty() {
                continue;
            }

            match spec_map.get(&name) {
                Some(spec) => specs.push(spec.clone()),
                None => return Err(format!("Unknown format specifier '%{}'", name)),
            }
        }
    }

    Ok(specs)
}

/// Returns the Rust assertion type token stream for compile-time type checking.
/// This generates a `let _: AssertType = expr;` statement that will fail compilation
/// if the expression type doesn't match.
fn rust_assert_type_tokens(rust_type: &str) -> proc_macro2::TokenStream {
    match rust_type {
        "i8" => quote! { i8 },
        "i16" => quote! { i16 },
        "i32" => quote! { i32 },
        "i64" => quote! { i64 },
        "u8" => quote! { u8 },
        "u16" => quote! { u16 },
        "u32" => quote! { u32 },
        "u64" => quote! { u64 },
        "isize" => quote! { isize },
        "usize" => quote! { usize },
        "bool" => quote! { bool },
        "f64" => quote! { f64 },
        "&str" => quote! { &str },
        "NtStatus" => quote! { ::wdf::NtStatus },
        "HResult" => quote! { ::wdf::HResult },
        "Guid" => quote! { ::wdf::Guid },
        _ => quote! { i32 }, // fallback
    }
}

/// Represents a parsed trace argument (expression paired with its format spec)
struct TraceArg {
    name: String,
    spec: CFormatSpec,
    /// The original expression for this argument
    expr: syn::Expr,
}

/// Result of parsing trace macro arguments
struct ParsedTraceArgs {
    /// Optional flag identifier (e.g., FLAG_ONE)
    flag: Option<syn::Ident>,
    /// Optional trace level (e.g., TraceLevel::Information)
    level: Option<syn::Ident>,
    /// The format string (C-style, with %d, %!STATUS!, etc.)
    format_str: String,
    /// The parsed format specifiers from the format string
    format_specs: Vec<CFormatSpec>,
    /// The trace arguments (expressions)
    args: Vec<TraceArg>,
}

/// Known trace level names for validation
const TRACE_LEVEL_NAMES: &[&str] = &[
    "None", "Critical", "Error", "Warning", 
    "Information", "Verbose", "Reserved6", "Reserved7", "Reserved8", "Reserved9"
];

/// Parses the trace! macro input with C-style format specifiers.
///
/// Supported syntaxes:
/// - `trace!("fmt %d %!STATUS!", arg1, arg2)` - no flag, no level
/// - `trace!(FLAG, "fmt %d", arg1)` - with flag
/// - `trace!(FLAG, Level, "fmt %d", arg1)` - with flag and level
/// - `trace!(Level, "fmt %d", arg1)` - with level only
fn parse_trace_args(item: TokenStream) -> Result<ParsedTraceArgs, Error> {
    use syn::{Expr, ExprLit, punctuated::Punctuated};
    
    // Parse as comma-separated expressions
    let args = syn::parse::Parser::parse(
        Punctuated::<Expr, Token![,]>::parse_terminated,
        item,
    )?;
    
    let all_args: Vec<Expr> = args.into_iter().collect();
    
    if all_args.is_empty() {
        return Err(Error::new(proc_macro2::Span::call_site(), "trace! requires at least a format string"));
    }
    
    let mut flag: Option<syn::Ident> = None;
    let mut level: Option<syn::Ident> = None;
    let format_str: String;
    let remaining_start_idx: usize;
    
    // Determine the pattern based on the first few arguments
    let first_is_path = matches!(&all_args[0], Expr::Path(_));
    let first_is_string = matches!(&all_args[0], Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));
    
    if first_is_string {
        format_str = match &all_args[0] {
            Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
            _ => unreachable!(),
        };
        remaining_start_idx = 1;
    } else if first_is_path && all_args.len() >= 2 {
        let second_is_string = matches!(&all_args[1], Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));
        let second_is_path = matches!(&all_args[1], Expr::Path(_));
        
        if second_is_string {
            let ident = extract_path_ident(&all_args[0])?;
            let ident_str = ident.to_string();
            
            if TRACE_LEVEL_NAMES.contains(&ident_str.as_str()) {
                level = Some(ident);
            } else {
                flag = Some(ident);
            }
            
            format_str = match &all_args[1] {
                Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
                _ => unreachable!(),
            };
            remaining_start_idx = 2;
        } else if second_is_path && all_args.len() >= 3 {
            let third_is_string = matches!(&all_args[2], Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));
            
            if third_is_string {
                let first_ident = extract_path_ident(&all_args[0])?;
                let second_ident = extract_path_ident(&all_args[1])?;
                
                let second_str = second_ident.to_string();
                if !TRACE_LEVEL_NAMES.contains(&second_str.as_str()) {
                    return Err(Error::new_spanned(&all_args[1], 
                        format!("Expected a trace level (one of: {:?}), got '{}'", TRACE_LEVEL_NAMES, second_str)));
                }
                
                flag = Some(first_ident);
                level = Some(second_ident);
                
                format_str = match &all_args[2] {
                    Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
                    _ => unreachable!(),
                };
                remaining_start_idx = 3;
            } else {
                return Err(Error::new_spanned(&all_args[0], "Expected a format string"));
            }
        } else {
            return Err(Error::new_spanned(&all_args[0], "Expected a format string"));
        }
    } else {
        return Err(Error::new_spanned(&all_args[0], "Expected a format string or flag identifier"));
    }

    // Parse format specifiers from the C-style format string
    let format_specs = match parse_c_format_string(&format_str) {
        Ok(specs) => specs,
        Err(e) => return Err(Error::new(proc_macro2::Span::call_site(), e)),
    };

    // Validate argument count matches format specifier count
    let arg_count = all_args.len() - remaining_start_idx;
    let spec_count = format_specs.len();
    if arg_count != spec_count {
        return Err(Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "Format string has {} specifier(s) but {} argument(s) were provided",
                spec_count, arg_count
            ),
        ));
    }

    // Build trace args pairing expressions with their format specs
    let mut trace_args = Vec::new();
    for (idx, expr) in all_args.into_iter().skip(remaining_start_idx).enumerate() {
        let name = extract_arg_name(&expr, idx);
        trace_args.push(TraceArg {
            name,
            spec: format_specs[idx].clone(),
            expr,
        });
    }
    
    Ok(ParsedTraceArgs {
        flag,
        level,
        format_str,
        format_specs,
        args: trace_args,
    })
}

/// Extracts a human-readable name from an expression for TMF annotations
fn extract_arg_name(expr: &syn::Expr, idx: usize) -> String {
    use syn::Expr;
    match expr {
        Expr::Path(p) => p.path.segments.last()
            .map(|s| s.ident.to_string())
            .unwrap_or_else(|| format!("arg{}", idx)),
        Expr::Lit(lit) => match &lit.lit {
            Lit::Int(_) => format!("literal{}", idx),
            Lit::Str(_) => format!("str{}", idx),
            _ => format!("arg{}", idx),
        },
        Expr::Reference(r) => {
            if let Expr::Path(p) = r.expr.as_ref() {
                p.path.segments.last()
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
        _ => format!("arg{}", idx),
    }
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

/// Generates the WPP-style format string with numbered placeholders from C-style format specifiers.
fn generate_wpp_format_string(format_str: &str, specs: &[CFormatSpec]) -> String {
    let mut result = String::new();
    let mut spec_idx = 0;
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '%' {
            result.push(c);
            continue;
        }

        if chars.peek() == Some(&'%') {
            chars.next();
            result.push('%');
            result.push('%');
            continue;
        }

        // Skip width/flag chars
        while chars.peek().map_or(false, |c| matches!(c, '-' | '+' | '#' | '0'..='9' | '.')) {
            chars.next();
        }

        if chars.peek() == Some(&'!') {
            chars.next();
            while let Some(&ch) = chars.peek() {
                if ch == '!' { chars.next(); break; }
                chars.next();
            }
        } else {
            if chars.peek() == Some(&'I') {
                chars.next();
                if chars.peek() == Some(&'6') { chars.next(); if chars.peek() == Some(&'4') { chars.next(); } }
            } else if chars.peek() == Some(&'l') {
                chars.next();
                if chars.peek() == Some(&'l') { chars.next(); }
            } else if chars.peek().map_or(false, |c| matches!(c, 'h' | 'w')) {
                chars.next();
            }
            if chars.peek().map_or(false, |c| c.is_alphabetic()) {
                chars.next();
            }
        }

        if spec_idx < specs.len() {
            let param_num = 10 + spec_idx;
            let fmt = &specs[spec_idx].format_spec;
            result.push_str(&format!("%{}!{}!", param_num, fmt));
            spec_idx += 1;
        }
    }

    result
}

/// A procedural macro for WPP-style tracing with C-style format specifiers.
///
/// # Usage
///
/// ## Short form specifiers:
/// ```ignore
/// trace!("Value: %d, Hex: %x, Str: %s", my_int, my_hex, my_str);
/// trace!("64-bit: %I64u", my_u64);
/// ```
///
/// ## Long form specifiers (shrieks):
/// ```ignore
/// trace!("Status: %!STATUS!, HR: %!HRESULT!", nt_status, hresult);
/// ```
///
/// ## With flag and/or level:
/// ```ignore
/// trace!(FLAG_ONE, "Value: %d", value);
/// trace!(FLAG_ONE, Information, "Status: %!STATUS!", status);
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
    
    let ParsedTraceArgs { flag, level, format_str, format_specs, args } = parsed;

    if args.len() > 8 {
        return Error::new(
            proc_macro2::Span::call_site(),
            format!("trace! supports at most 8 arguments (got {}); add a new pre-generated trace_N method on TraceWriter to raise the limit", args.len()),
        ).to_compile_error().into();
    }

    let message_id = TRACE_MESSAGE_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    
    let driver_name = std::env::var("CARGO_PKG_NAME")
        .unwrap_or_else(|_| "unknown_driver".to_string())
        .replace('-', "_");
    
    // Generate a deterministic message GUID from the source file path
    // so that the same trace site always produces the same GUID across builds,
    // regardless of compilation order or toolchain version.
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

    let method_call = generate_trace_method_call(&method_ident, &trace_guid_bytes, message_id as u16, &args, flag.as_ref(), level.as_ref());

    let expanded = quote! {
        core::hint::codeview_annotation!(#(#annotation_args),*);
        {
            #method_call
        }
    };
    
    expanded.into()
}

/// Generates the call site that invokes one of the pre-generated
/// `TraceWriter::trace_N` inherent methods.
///
/// For each argument, this emits a per-arg binding that:
///   * coerces the user expression into a value implementing `TraceArgData`
///     (`CString` for `&str`, finalised `TraceFmtBuf` for `Display`, or a
///     type-ascribed primitive),
///   * surfaces a clear compile error if the expression's type does not
///     match the format spec (e.g. `%d` requires `i32`),
///   * keeps the binding alive across the trace call so its byte slice
///     remains valid.
///
/// The bindings are then passed by reference into `trace_N`, which is
/// generic over each `Ai: TraceArgData` and forwards the bytes to WPP.
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
            // String path: coerce &str → CString (single allocation, null-terminated)
            quote! {
                let #arg_var = ::wdf::__internal::CString::new(#expr).unwrap();
            }
        } else if arg.spec.is_complex && rust_type == "Display" {
            // Custom Display type path: format via TraceFmtBuf, then finalize.
            // The inline assertion fn surfaces a clear compile error if the
            // expression's type does not implement core::fmt::Display.
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
            // Primitive path: type-ascribe the expression to enforce the
            // expected Rust type at compile time.
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
        quote! { TraceLevel::#level_ident as u8 }
    } else {
        quote! { TraceLevel::None as u8 }
    };

    let (flags_value, control_index) = if let Some(flag_ident) = flag {
        let flag_name = flag_ident.to_string();
        (
            quote! { { let __flag = WppFlag::by_name(#flag_name).expect("Unknown WppFlag"); (__flag.flag_index() + 1) as u32 } },
            quote! { { let __flag = WppFlag::by_name(#flag_name).expect("Unknown WppFlag"); __flag.control_index() } }
        )
    } else {
        (quote! { 0u32 }, quote! { 0usize })
    };

    quote! {
        {
            // Per-argument bindings keep coerced values alive for the
            // duration of the trace call so their byte slices stay valid.
            #(#arg_bindings)*

            let __trace_level: u8 = #level_value;
            let __trace_flags: u32 = #flags_value;
            let __control_index: usize = #control_index;

            if let Some(__trace_writer) = ::wdf::get_trace_writer() {
                // WPP fires only when the configured flag bit is set (or no
                // flag was specified) AND the message level is at or below
                // the control block's level (lower numeric = more severe).
                let __should_trace_wpp = (__trace_flags == 0 || __trace_writer.is_flag_enabled(__control_index, __trace_flags))
                                            && __trace_writer.is_level_enabled(__control_index, __trace_level);
                // AutoLog always captures messages above Verbose; Verbose is
                // gated by the per-control-block AutoLogVerboseEnabled flag.
                let __should_auto_log = __trace_level < TraceLevel::Verbose as u8 || __trace_writer.is_auto_log_verbose_enabled(__control_index);

                __trace_writer.#method_ident(
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

/// Emits inherent `trace_0`..`trace_8` methods on `wdf::tracing::TraceWriter`.
///
/// # Why pre-generated methods?
///
/// An earlier design synthesised a fresh `TraceWriterExtXX` trait + impl per
/// `trace!` call site, parameterised by the exact argument arity. That worked
/// but produced one anonymous trait per unique combination of format specs,
/// bloated codegen, and forced the macro to invent unique trait names per
/// call. Replacing it with a small fixed family of generic methods keeps the
/// public surface small while still type-checking each argument through
/// [`TraceArgData`].
///
/// # Generated shape
///
/// Each method has the signature:
/// ```ignore
/// pub fn trace_N<A1: TraceArgData, ..., AN: TraceArgData>(
///     &self,
///     level: UCHAR,
///     flags: ULONG,
///     id: USHORT,
///     trace_guid: LPCGUID,
///     should_trace_wpp: bool,
///     should_auto_log: bool,
///     a1: &A1, ..., aN: &AN,
/// );
/// ```
///
/// The body calls `TraceArgData::as_bytes` on each argument and forwards the
/// resulting `(ptr, len)` pairs to `WppTraceMessage` and `WppAutoLogTrace`,
/// terminated by a single `null` sentinel as required by the variadic ABI.
///
/// The macro takes no input — it always emits methods for arities 0..=8.
/// Adding more arities only requires bumping `MAX_ARITY` here. Intended for
/// internal use from inside the `wdf` crate; the emitted code uses
/// `crate::...` paths and assumes the surrounding `impl TraceWriter` block
/// lives in a module where those resolve correctly.
#[proc_macro]
pub fn define_trace_writer_methods(_input: TokenStream) -> TokenStream {
    const MAX_ARITY: usize = 8;
    let mut methods: Vec<proc_macro2::TokenStream> = Vec::with_capacity(MAX_ARITY + 1);

    for n in 0..=MAX_ARITY {
        let method_ident = syn::Ident::new(&format!("trace_{}", n), proc_macro2::Span::call_site());

        let type_params: Vec<syn::Ident> = (0..n)
            .map(|i| syn::Ident::new(&format!("A{}", i + 1), proc_macro2::Span::call_site()))
            .collect();
        let arg_idents: Vec<syn::Ident> = (0..n)
            .map(|i| syn::Ident::new(&format!("a{}", i + 1), proc_macro2::Span::call_site()))
            .collect();
        let bytes_idents: Vec<syn::Ident> = (0..n)
            .map(|i| syn::Ident::new(&format!("__b{}", i + 1), proc_macro2::Span::call_site()))
            .collect();

        let generics = if n == 0 {
            quote! {}
        } else {
            quote! { < #(#type_params : crate::trace_data::TraceArgData),* > }
        };

        let arg_params = arg_idents.iter().zip(type_params.iter()).map(|(a, t)| {
            quote! { #a: & #t }
        });

        let byte_bindings = arg_idents.iter().zip(bytes_idents.iter()).map(|(a, b)| {
            quote! { let #b = crate::trace_data::TraceArgData::as_bytes(#a); }
        });

        let arg_pairs: Vec<proc_macro2::TokenStream> = bytes_idents.iter().map(|b| {
            quote! { #b.as_ptr(), #b.len(), }
        }).collect();

        methods.push(quote! {
            #[doc(hidden)]
            #[inline]
            pub fn #method_ident #generics (
                &self,
                level: ::wdk_sys::UCHAR,
                flags: ::wdk_sys::ULONG,
                id: ::wdk_sys::USHORT,
                trace_guid: ::wdk_sys::LPCGUID,
                should_trace_wpp: bool,
                should_auto_log: bool,
                #(#arg_params),*
            ) {
                // Resolve every arg into its byte representation up front.
                // The borrows live for the entire body so the WPP variadic
                // call below sees stable pointers.
                #(#byte_bindings)*
                unsafe {
                    if should_trace_wpp {
                        // WppTraceMessage takes a variadic list of
                        // (*const u8, usize) pairs terminated by a single
                        // null sentinel; the trailing nullptr below is
                        // required by that ABI contract.
                        let logger = crate::driver::get_wpp_logger().unwrap();
                        let _ = crate::driver::get_wpp_trace_message().unwrap()(
                            logger,
                            crate::tracing::WPP_TRACE_OPTIONS,
                            trace_guid,
                            id,
                            #(#arg_pairs)*
                            core::ptr::null::<core::ffi::c_void>(),
                        );
                    }

                    if should_auto_log {
                        // AutoLog mirrors the same (ptr, len)+nullptr ABI;
                        // it is the in-flight-recorder sink WPP feeds into
                        // crash dumps when verbose tracing is captured.
                        let auto_log_context = crate::driver::get_auto_log_context().unwrap();
                        let _ = crate::tracing::WppAutoLogTrace(
                            auto_log_context,
                            level,
                            flags,
                            trace_guid.cast_mut().cast(),
                            id,
                            #(#arg_pairs)*
                            core::ptr::null::<core::ffi::c_void>(),
                        );
                    }
                }
            }
        });
    }

    let expanded = quote! { #(#methods)* };
    expanded.into()
}
