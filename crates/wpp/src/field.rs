// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trait implemented by types that can be serialized into ETW event data.

use alloc::ffi::CString;
use core::{cmp::max, ffi::CStr};

/// The associated constants are resolved during monomorphization, allowing
/// `codeview_annotation` to emit the concrete type name into the PDB.
pub trait WppField {
    /// The WPP reader type used to decode this field from the event payload.
    const TYPE_NAME: &'static str;

    /// The WPP printf-style specifier used to render this field.
    const FORMAT_SPEC: &'static str;

    /// Returns the raw event data as a byte slice for ETW serialization.
    fn as_bytes(&self) -> &[u8];
}

macro_rules! impl_wpp_field {
    ($($ty:ty => ($name:literal, $format_spec:literal)),* $(,)?) => {
        $(
            impl WppField for $ty {
                const TYPE_NAME: &'static str = $name;
                const FORMAT_SPEC: &'static str = $format_spec;

                #[inline]
                fn as_bytes(&self) -> &[u8] {
                    unsafe {
                        core::slice::from_raw_parts(
                            self as *const Self as *const u8,
                            core::mem::size_of::<Self>(),
                        )
                    }
                }
            }
        )*
    };
}

impl_wpp_field! {
    i8    => ("ItemChar", "!c!"),
    u8    => ("ItemUChar", "!c!"),
    i16   => ("ItemShort", "!hd!"),
    u16   => ("ItemShort", "!hu!"),
    i32   => ("ItemLong", "!ld!"),
    u32   => ("ItemLong", "!lu!"),
    i64   => ("ItemLongLong", "!I64d!"),
    u64   => ("ItemULongLong", "!I64u!"),
    f32   => ("ItemDouble", "!f!"), // TODO: There is no 32bit equivalent in WPP, so can we use double?
    f64   => ("ItemDouble", "!f!"),
    // bool  => ("ItemUChar", "!d!"), // TODO: boolean is ItemListLong(false,true) in WPP
    usize => ("ItemULongLong", "!I64u!"), // TODO: on 32 bit machines, this will be 4 bytes, handle it accordingly
    isize => ("ItemLongLong", "!I64d!"),
}

impl WppField for &CStr {
    const FORMAT_SPEC: &'static str = "!s!";
    const TYPE_NAME: &'static str = "ItemString";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        CStr::to_bytes_with_nul(self)
    }
}

impl WppField for CString {
    const FORMAT_SPEC: &'static str = "!s!";
    const TYPE_NAME: &'static str = "ItemString";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        CString::as_bytes_with_nul(self)
    }
}

const BOOL_TRUE: &[u8] = &[1];
const BOOL_FALSE: &[u8] = &[0];

impl WppField for bool {
    const TYPE_NAME: &'static str = "ItemListByte(false,true)";
    const FORMAT_SPEC: &'static str = "!s!";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        match self {
            true => BOOL_TRUE,
            false => BOOL_FALSE,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::WppField;

    #[test]
    fn bool_uses_wpp_item_list_format() {
        assert_eq!(<bool as WppField>::TYPE_NAME, "ItemListByte(false,true)");
        assert_eq!(<bool as WppField>::FORMAT_SPEC, "!s!");
    }
}

/// Fixed-capacity string assembled during const evaluation for a PDB
/// annotation.
#[doc(hidden)]
pub struct AnnotationString<const CAPACITY: usize> {
    bytes: [u8; CAPACITY],
    len: usize,
}

impl<const CAPACITY: usize> AnnotationString<CAPACITY> {
    #[doc(hidden)]
    pub const fn concat(prefix: &str, value: &str, suffix: &str) -> Self {
        let mut bytes = [0; CAPACITY];
        let mut len = 0;
        let parts = [prefix.as_bytes(), value.as_bytes(), suffix.as_bytes()];
        let mut part_index = 0;

        while part_index < parts.len() {
            let part = parts[part_index];
            let mut byte_index = 0;
            while byte_index < part.len() {
                assert!(
                    len < CAPACITY,
                    "WPP annotation string exceeds its buffer capacity"
                );
                bytes[len] = part[byte_index];
                len += 1;
                byte_index += 1;
            }
            part_index += 1;
        }

        Self { bytes, len }
    }

    #[doc(hidden)]
    pub const fn concat_parts(parts: &[&str]) -> Self {
        let mut bytes = [0; CAPACITY];
        let mut len = 0;
        let mut part_index = 0;

        while part_index < parts.len() {
            let part = parts[part_index].as_bytes();
            let mut byte_index = 0;
            while byte_index < part.len() {
                assert!(
                    len < CAPACITY,
                    "WPP annotation string exceeds its buffer capacity"
                );
                bytes[len] = part[byte_index];
                len += 1;
                byte_index += 1;
            }
            part_index += 1;
        }

        Self { bytes, len }
    }

    #[doc(hidden)]
    pub const fn as_str(&self) -> &str {
        let bytes = unsafe { core::slice::from_raw_parts(self.bytes.as_ptr(), self.len) };
        unsafe { core::str::from_utf8_unchecked(bytes) }
    }
}

/// Converts a value into a type that implements [`WppField`].
///
/// For `&str` and `String`, this produces a null-terminated `CString`.
/// For types that already implement `WppField`, this is a no-op wrapper.
pub trait IntoWppField {
    type Output: WppField;
    fn into_wpp_field(self) -> Self::Output;
}

impl<'a> IntoWppField for &'a str {
    type Output = CString;

    #[inline]
    fn into_wpp_field(self) -> CString {
        CString::new(self).unwrap_or_default()
    }
}

impl IntoWppField for alloc::string::String {
    type Output = CString;

    #[inline]
    fn into_wpp_field(self) -> CString {
        CString::new(self).unwrap_or_default()
    }
}

impl<'a> IntoWppField for &'a CStr {
    type Output = &'a CStr;

    #[inline]
    fn into_wpp_field(self) -> &'a CStr {
        self
    }
}

impl IntoWppField for CString {
    type Output = CString;

    #[inline]
    fn into_wpp_field(self) -> CString {
        self
    }
}

macro_rules! impl_into_wpp_field_passthrough {
    ($($ty:ty),* $(,)?) => {
        $(
            impl IntoWppField for $ty {
                type Output = Self;

                #[inline]
                fn into_wpp_field(self) -> Self {
                    self
                }
            }

            impl<'a> IntoWppField for &'a $ty {
                type Output = $ty;

                #[inline]
                fn into_wpp_field(self) -> $ty {
                    *self
                }
            }
        )*
    };
}

impl_into_wpp_field_passthrough! {
    i8, u8, i16, u16, i32, u32, i64, u64,
    f32, f64, bool, usize, isize, 
}
