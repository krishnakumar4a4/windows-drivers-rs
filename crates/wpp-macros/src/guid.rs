// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! GUID utilities for WPP macro code generation.

use quote::quote;

/// A static counter for generating unique message IDs across trace invocations.
/// This is incremented for each trace! macro expansion.
pub(crate) static TRACE_MESSAGE_ID: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(10);

// TODO: this code is repeated in the wdf::Guid.
// Move it to a common location
pub(crate) fn is_valid_guid(guid_str: &str) -> bool {
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

/// Generates a `proc_macro2::TokenStream` constructing a `::wdf::__internal::GUID`
/// struct literal from a GUID string (e.g. "cb94defb-592a-4509-8f2e-54f204929669").
///
/// The GUID string must have been validated by [`is_valid_guid`] beforehand.
pub(crate) fn guid_str_to_tokens(guid_str: &str) -> proc_macro2::TokenStream {
    let hex = guid_str.replace('-', "");
    let data1 = u32::from_str_radix(&hex[0..8], 16).unwrap();
    let data2 = u16::from_str_radix(&hex[8..12], 16).unwrap();
    let data3 = u16::from_str_radix(&hex[12..16], 16).unwrap();
    let mut data4 = [0u8; 8];
    for i in 0..8 {
        data4[i] = u8::from_str_radix(&hex[16 + i * 2..18 + i * 2], 16).unwrap();
    }
    let d4_0 = data4[0]; let d4_1 = data4[1]; let d4_2 = data4[2]; let d4_3 = data4[3];
    let d4_4 = data4[4]; let d4_5 = data4[5]; let d4_6 = data4[6]; let d4_7 = data4[7];
    quote! {
        ::wdf::__internal::GUID {
            Data1: #data1,
            Data2: #data2,
            Data3: #data3,
            Data4: [#d4_0, #d4_1, #d4_2, #d4_3, #d4_4, #d4_5, #d4_6, #d4_7],
        }
    }
}

/// Generates a deterministic 16-byte GUID from the source file path using SHA-256.
pub(crate) fn generate_deterministic_guid(source_file: &str) -> [u8; 16] {
    use sha2::{Sha256, Digest};

    let normalised = source_file.replace('\\', "/");

    let hash = Sha256::digest(normalised.as_bytes());
    let mut bytes = [0u8; 16];
    bytes.copy_from_slice(&hash[..16]);
    bytes
}

/// Formats 16 raw GUID bytes as a standard `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` string.
pub(crate) fn format_guid(b: &[u8; 16]) -> String {
    format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        b[0], b[1], b[2], b[3],
        b[4], b[5],
        b[6], b[7],
        b[8], b[9],
        b[10], b[11], b[12], b[13], b[14], b[15],
    )
}

/// Generates a `proc_macro2::TokenStream` that constructs a `::wdf::__internal::GUID`
/// struct literal from raw bytes, suitable for embedding in generated code.
pub(crate) fn guid_to_tokens(b: &[u8; 16]) -> proc_macro2::TokenStream {
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
        ::wdf::__internal::GUID {
            Data1: #data1,
            Data2: #data2,
            Data3: #data3,
            Data4: [#d4_0, #d4_1, #d4_2, #d4_3, #d4_4, #d4_5, #d4_6, #d4_7],
        }
    }
}
