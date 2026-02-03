// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! A collection of macros used for writing WDF-based drivers in safe Rust

use proc_macro::TokenStream;
use quote::quote;
use syn::{Error, Ident, ItemFn, ItemImpl, ItemStruct, Lit, meta::parser, parse_macro_input};

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
    /// Integer types (i32, u32, i64, u64, etc.) - maps to ItemLong
    Long,
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
            TraceArgType::String => "ItemString",
        }
    }

    /// Returns the C-style format specifier
    fn format_specifier(&self) -> &'static str {
        match self {
            TraceArgType::Long => "d",
            TraceArgType::String => "s",
        }
    }
}

/// Represents a parsed trace argument
#[derive(Debug)]
struct TraceArg {
    name: String,
    arg_type: TraceArgType,
}

/// Parses the trace! macro input and extracts format string and arguments
fn parse_trace_args(item: TokenStream) -> Result<(String, Vec<TraceArg>), Error> {
    use syn::{Expr, ExprLit, Token, punctuated::Punctuated};
    
    let args = syn::parse::Parser::parse(
        Punctuated::<Expr, Token![,]>::parse_terminated,
        item,
    )?;
    
    let mut iter = args.into_iter();
    
    // First argument must be a format string literal
    let format_str = match iter.next() {
        Some(Expr::Lit(ExprLit { lit: Lit::Str(s), .. })) => s.value(),
        Some(expr) => return Err(Error::new_spanned(expr, "First argument must be a string literal")),
        None => return Err(Error::new(proc_macro2::Span::call_site(), "trace! requires at least a format string")),
    };
    
    // Remaining arguments are the trace parameters
    let mut trace_args = Vec::new();
    for (idx, expr) in iter.enumerate() {
        let (name, arg_type) = infer_arg_info(&expr, idx)?;
        trace_args.push(TraceArg { name, arg_type });
    }
    
    Ok((format_str, trace_args))
}

/// Infers the argument name and type from an expression
fn infer_arg_info(expr: &syn::Expr, idx: usize) -> Result<(String, TraceArgType), Error> {
    use syn::Expr;
    
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
        _ => format!("arg{}", idx),
    };
    
    // Infer type based on expression structure
    // For now, default to Long for most expressions, String for string literals and references
    let arg_type = match expr {
        Expr::Lit(lit) => {
            match &lit.lit {
                Lit::Str(_) => TraceArgType::String,
                Lit::Int(_) => TraceArgType::Long,
                _ => return Err(Error::new_spanned(lit, "Unsupported literal type for tracing")),
            }
        }
        Expr::Reference(r) => {
            // Match only &str (reference to a string literal)
            match r.expr.as_ref() {
                Expr::Lit(lit) if matches!(&lit.lit, Lit::Str(_)) => TraceArgType::String,
                _ => return Err(Error::new_spanned(expr, "Only &str references are supported for tracing")),
            }
        }
        _ => return Err(Error::new_spanned(expr, "Unsupported expression type for tracing")),
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
    
    let expanded = quote! {
        core::hint::codeview_annotation!(#(#annotation_args),*)
    };
    
    expanded.into()
}
