// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! A collection of macros used for writing WDF-based drivers in safe Rust

use proc_macro::TokenStream;
use quote::quote;
use syn::{Error, Ident, ItemFn, ItemImpl, ItemStruct, Lit, Token, Type, meta::parser, parse_macro_input};
use syn::parse::{Parse, ParseStream};

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

/// A procedural macro used to mark the entry point of a WDF driver
#[proc_macro_attribute]
pub fn driver_entry(args: TokenStream, input: TokenStream) -> TokenStream {
    const TRACING_CONTROL_GUID_ATTR_NAME: &str = "tracing_control_guid";

    let input_clone = input.clone();
    let item_fn = parse_macro_input!(input_clone as ItemFn);
    let safe_driver_entry = item_fn.sig.ident;

    let mut tracing_control_guid_str: Option<String> = None;

    let tracing_control_guid_parser = parser(|meta| {
        if !meta.path.is_ident(TRACING_CONTROL_GUID_ATTR_NAME) {
            return Err(meta.error(format!("Expected `{TRACING_CONTROL_GUID_ATTR_NAME}`")));
        }

        let Lit::Str(guid_str_expr) = meta.value()?.parse()? else {
            return Err(meta.error("Expected tracing control GUID"));
        };

        let guid_str = guid_str_expr.value();

        if !is_valid_guid(&guid_str) {
            return Err(Error::new_spanned(guid_str_expr, "Not a valid GUID"));
        }

        tracing_control_guid_str = Some(guid_str);

        Ok(())
    });

    parse_macro_input!(args with tracing_control_guid_parser);

    let parse_tracing_control_guid = tracing_control_guid_str.clone().map_or_else(
        || {
            quote! {
                None
            }
        },
        |s| {
            quote! {
                Some(wdf::Guid::parse(#s).expect("Not a valid GUID"))
            }
        },
    );

    // Generate codeview annotation if tracing control GUID is provided
    let codeview_annotation = tracing_control_guid_str.as_ref().map(|s| {
        quote! {
            core::hint::codeview_annotation!("TMC:", #s, "CtlGuid");
        }
    });

    let mut wrappers: TokenStream = quote! {
        #[unsafe(link_section = "INIT")]
        #[unsafe(export_name = "DriverEntry")] // WDF expects a symbol with the name DriverEntry
        extern "system" fn __driver_entry(driver: &mut ::wdf::DRIVER_OBJECT, registry_path: ::wdf::PCUNICODE_STRING,) -> ::wdf::NTSTATUS {
            #codeview_annotation  // Only emits if Some
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

/// Parses the trace! macro input and extracts format string and arguments
/// Supports both `trace!("fmt", expr)` and `trace!("fmt", expr: Type)` syntax
fn parse_trace_args(item: TokenStream) -> Result<(String, Vec<TraceArg>), Error> {
    use syn::{Expr, ExprLit, punctuated::Punctuated};
    
    // Parse as comma-separated TraceArgInput items
    let args = syn::parse::Parser::parse(
        Punctuated::<TraceArgInput, Token![,]>::parse_terminated,
        item,
    )?;
    
    let mut iter = args.into_iter();
    
    // First argument must be a format string literal (no type annotation)
    let format_str = match iter.next() {
        Some(TraceArgInput { expr: Expr::Lit(ExprLit { lit: Lit::Str(s), .. }), explicit_type: None }) => s.value(),
        Some(TraceArgInput { expr: Expr::Lit(ExprLit { lit: Lit::Str(_), .. }), explicit_type: Some(_) }) => {
            return Err(Error::new(proc_macro2::Span::call_site(), "Format string should not have a type annotation"));
        }
        Some(TraceArgInput { expr, .. }) => return Err(Error::new_spanned(expr, "First argument must be a string literal")),
        None => return Err(Error::new(proc_macro2::Span::call_site(), "trace! requires at least a format string")),
    };
    
    // Remaining arguments are the trace parameters (with optional type annotations)
    let mut trace_args = Vec::new();
    for (idx, arg_input) in iter.enumerate() {
        let (name, arg_type) = infer_arg_info(&arg_input.expr, idx, arg_input.explicit_type.as_ref())?;
        trace_args.push(TraceArg { name, arg_type, expr: arg_input.expr });
    }
    
    Ok((format_str, trace_args))
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
/// ```ignore
/// trace!("My message with value: {} and string: {}", my_int, my_string);
/// ```
///
/// This generates a `codeview_annotation!` call with WPP trace metadata.
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
    let (format_str, args) = match parse_trace_args(item) {
        Ok(parsed) => parsed,
        Err(e) => return e.to_compile_error().into(),
    };
    
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
    
    // Generate the method call with arguments
    let method_call = generate_trace_method_call(&method_ident, &trait_ident, message_id as u16, &args);
    
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
                #(#param_decls),*
            ) {
                #(#len_calculations)*
                
                unsafe {
                    let logger = ::wdf::__internal::get_wpp_logger().unwrap();
                    let _ = ::wdf::__internal::get_wpp_trace_message().unwrap()(
                        logger,
                        ::wdf::__internal::WPP_TRACE_OPTIONS,
                        trace_guid,
                        id,
                        #(#arg_pairs)*
                        core::ptr::null::<core::ffi::c_void>(), // sentinel
                    );

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

/// Generates the method call to the TraceWriterExt method
fn generate_trace_method_call(
    method_ident: &syn::Ident,
    trait_ident: &syn::Ident,
    message_id: u16,
    args: &[TraceArg],
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
    
    quote! {
        {
            #(#arg_conversions)*
            if let Some(__trace_writer) = ::wdf::get_trace_writer() {
                <::wdf::tracing::TraceWriter as #trait_ident>::#method_ident(
                    __trace_writer,
                    0,  // level
                    0,  // flags
                    #message_id_lit,
                    &::wdf::__internal::TRACE_GUID,
                    #(#arg_values),*
                );
            }
        }
    }
}
