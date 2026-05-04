// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! C-style format specifier mapping from `defaultwpp.ini`.

use quote::quote;

/// Represents the type of a trace argument for WPP tracing, derived from
/// C-style format specifiers as defined in defaultwpp.ini.
#[derive(Debug, Clone)]
pub(crate) struct CFormatSpec {
    /// The format specifier name as it appears in the format string
    pub name: String,
    /// The WPP/ETW item type name (e.g. "ItemLong", "ItemNTSTATUS")
    pub etw_type: String,
    /// The format spec string to use in the TMF annotation
    pub format_spec: String,
    /// The Rust assertion type for compile-time checking
    pub rust_assert_type: String,
    /// Whether this is a "complex" type that needs special handling
    pub is_complex: bool,
}

/// Returns the mapping from C format specifier names to [`CFormatSpec`].
pub(crate) fn format_spec_map() -> &'static std::collections::HashMap<String, CFormatSpec> {
    static SPEC_MAP: std::sync::OnceLock<std::collections::HashMap<String, CFormatSpec>> =
        std::sync::OnceLock::new();
    SPEC_MAP.get_or_init(build_format_spec_map)
}

fn build_format_spec_map() -> std::collections::HashMap<String, CFormatSpec> {
    let mut map = std::collections::HashMap::new();

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

    // ---- Short-form specifiers ----
    spec!("c", "ItemChar", "c", "u8");
    spec!("hc", "ItemChar", "c", "u8");
    spec!("hi", "ItemShort", "hd", "i16");
    spec!("hd", "ItemShort", "hd", "i16");
    spec!("hu", "ItemShort", "hu", "u16");
    spec!("hx", "ItemShort", "x", "u16");
    spec!("hX", "ItemShort", "X", "u16");
    spec!("ho", "ItemShort", "o", "u16");
    spec!("i", "ItemLong", "d", "i32");
    spec!("d", "ItemLong", "d", "i32");
    spec!("u", "ItemLong", "u", "u32");
    spec!("x", "ItemLong", "x", "u32");
    spec!("X", "ItemLong", "X", "u32");
    spec!("o", "ItemLong", "o", "u32");
    spec!("li", "ItemLong", "d", "i32");
    spec!("ld", "ItemLong", "d", "i32");
    spec!("lu", "ItemLong", "u", "u32");
    spec!("lx", "ItemLong", "x", "u32");
    spec!("lX", "ItemLong", "X", "u32");
    spec!("lo", "ItemLong", "o", "u32");
    spec!("I64d", "ItemLongLong", "I64d", "i64");
    spec!("lld", "ItemLongLong", "I64d", "i64");
    spec!("I64u", "ItemULongLong", "I64u", "u64");
    spec!("llu", "ItemULongLong", "I64u", "u64");
    spec!("I64x", "ItemLongLongX", "I64x", "i64");
    spec!("I64X", "ItemLongLongXX", "I64X", "i64");
    spec!("I64o", "ItemLongLongO", "I64o", "i64");
    spec!("llx", "ItemLongLongX", "I64x", "i64");
    spec!("llX", "ItemLongLongXX", "I64X", "i64");
    spec!("llo", "ItemLongLongO", "I64o", "i64");
    spec!("Id", "ItemPtr", "Id", "isize");
    spec!("Iu", "ItemPtr", "Iu", "usize");
    spec!("Ix", "ItemPtr", "Ix", "usize");
    spec!("IX", "ItemPtr", "IX", "usize");
    spec!("Io", "ItemPtr", "Io", "usize");
    spec!("p", "ItemPtr", "p", "usize");
    spec!("s", "ItemString", "s", "&str", complex);
    spec!("hs", "ItemString", "s", "&str", complex);
    spec!("S", "ItemWString", "s", "&str", complex);
    spec!("e", "ItemDouble", "s", "f64");
    spec!("E", "ItemDouble", "s", "f64");
    spec!("f", "ItemDouble", "s", "f64");
    spec!("g", "ItemDouble", "s", "f64");
    spec!("G", "ItemDouble", "s", "f64");

    // ---- Long-form specifiers (%!NAME!) ----
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
    spec!("PTR", "ItemPtr", "p", "usize");
    spec!("HANDLE", "ItemPtr", "p", "usize");
    spec!("SLONGPTR", "ItemPtr", "Id", "isize");
    spec!("ULONGPTR", "ItemPtr", "Iu", "usize");
    spec!("XLONGPTR", "ItemPtr", "Ix", "isize");
    spec!("OLONGPTR", "ItemPtr", "Io", "isize");
    spec!("STATUS", "ItemNTSTATUS", "s", "NtStatus");
    spec!("status", "ItemNTSTATUS", "s", "NtStatus");
    spec!("HRESULT", "ItemHRESULT", "s", "HResult");
    spec!("hresult", "ItemHRESULT", "s", "HResult");
    spec!("WINERROR", "ItemWINERROR", "s", "u32");
    spec!("winerr", "ItemWINERROR", "s", "u32");
    spec!("NDIS_STATUS", "ItemNDIS_STATUS", "s", "i32");
    spec!("NDIS_OID", "ItemNDIS_OID", "s", "u16");
    spec!("GUID", "ItemGuid", "s", "Guid", complex);
    spec!("guid", "ItemGuid", "s", "Guid", complex);
    spec!("CLSID", "ItemCLSID", "s", "Guid", complex);
    spec!("IID", "ItemIID", "s", "Guid", complex);
    spec!("ASTR", "ItemString", "s", "&str", complex);
    spec!("WSTR", "ItemWString", "s", "&str", complex);
    spec!("IPADDR", "ItemIPAddr", "s", "u32");
    spec!("ipaddr", "ItemIPAddr", "s", "u32");
    spec!("PORT", "ItemPort", "s", "u16");
    spec!("port", "ItemPort", "s", "u16");
    spec!("bool", "ItemListLong(false,true)", "s", "bool");
    spec!("BOOLEAN", "ItemListByte(FALSE,TRUE)", "s", "bool");
    spec!("TIMESTAMP", "ItemTimestamp", "s", "i64");
    spec!("TIME", "ItemTimestamp", "s", "i64");
    spec!("DATE", "ItemTimestamp", "s", "i64");
    spec!("WAITTIME", "ItemTimestamp", "s", "i64");
    spec!("DISPLAY", "ItemString", "s", "Display", complex);
    spec!("display", "ItemString", "s", "Display", complex);

    map
}

/// Parses a C-style format string and extracts format specifiers.
pub(crate) fn parse_c_format_string(format_str: &str) -> Result<Vec<CFormatSpec>, String> {
    let spec_map = format_spec_map();
    let mut specs = Vec::new();
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '%' {
            continue;
        }

        if chars.peek() == Some(&'%') {
            chars.next();
            continue;
        }

        while chars.peek().map_or(false, |c| matches!(c, '-' | '+' | '#' | '0'..='9' | '.')) {
            chars.next();
        }

        if chars.peek() == Some(&'!') {
            chars.next();
            let mut name = String::new();
            while let Some(&ch) = chars.peek() {
                if ch == '!' { chars.next(); break; }
                name.push(ch);
                chars.next();
            }

            if name.is_empty() { continue; }

            match spec_map.get(&name) {
                Some(spec) => specs.push(spec.clone()),
                None => return Err(format!("Unknown format specifier '%!{}!'", name)),
            }
        } else {
            let mut name = String::new();

            if chars.peek() == Some(&'I') {
                name.push('I');
                chars.next();
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

            if let Some(&ch) = chars.peek() {
                if ch.is_alphabetic() {
                    name.push(ch);
                    chars.next();
                }
            }

            if name.is_empty() { continue; }

            match spec_map.get(&name) {
                Some(spec) => specs.push(spec.clone()),
                None => return Err(format!("Unknown format specifier '%{}'", name)),
            }
        }
    }

    Ok(specs)
}

/// Returns the Rust assertion type token stream for compile-time type checking.
pub(crate) fn rust_assert_type_tokens(rust_type: &str) -> proc_macro2::TokenStream {
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

/// Generates the WPP-style format string with numbered placeholders.
pub(crate) fn generate_wpp_format_string(format_str: &str, specs: &[CFormatSpec]) -> String {
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
