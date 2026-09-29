// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trait implemented by types that can be serialized into ETW event data.

use alloc::ffi::CString;
use core::{ffi::CStr, marker::PhantomData};

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

/// Contract for external types converted to a WPP wire representation.
///
/// # Safety
///
/// `Wire` must have no uninitialized padding bytes because its complete object
/// representation is copied into the trace payload.
pub unsafe trait WppValue: Copy {
    /// The initialized wire representation stored in the trace payload.
    type Wire: Copy;

    /// The WPP reader type used to decode the value.
    const TYPE_NAME: &'static str;

    /// The WPP printf-style specifier used to render the decoded value.
    const FORMAT_SPEC: &'static str;

    /// Converts this value into its WPP wire representation.
    fn into_wire(self) -> Self::Wire;
}

/// Owned WPP field produced from an external value type.
pub struct WppValueField<T: WppValue> {
    wire: T::Wire,
    marker: PhantomData<T>,
}

impl<T: WppValue> WppField for WppValueField<T> {
    const TYPE_NAME: &'static str = T::TYPE_NAME;
    const FORMAT_SPEC: &'static str = T::FORMAT_SPEC;

    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                (&raw const self.wire).cast::<u8>(),
                core::mem::size_of::<T::Wire>(),
            )
        }
    }
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
    f64   => ("ItemDouble", "!s!"),
    // bool  => ("ItemUChar", "!d!"), // TODO: boolean is ItemListLong(false,true) in WPP
    usize => ("ItemULongLong", "!I64u!"), // TODO: on 32 bit machines, this will be 4 bytes, handle it accordingly
    isize => ("ItemLongLong", "!I64d!"),
    // &[i8] => ("ItemListByte", "!s!"),
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

    // TODO: implicit conversion from Rust type to C++ type possible as both are single byte size types, 
    // change this
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
    use super::{AnnotationString, IntoWppField, WppField, compute_event_id};

    #[test]
    fn bool_uses_wpp_item_list_format() {
        assert_eq!(<bool as WppField>::TYPE_NAME, "ItemListByte(false,true)");
        assert_eq!(<bool as WppField>::FORMAT_SPEC, "!s!");
    }

    #[test]
    fn floats_use_wpp_double_format() {
        assert_eq!(<f64 as WppField>::TYPE_NAME, "ItemDouble");
        assert_eq!(<f64 as WppField>::FORMAT_SPEC, "!s!");
        assert_eq!(3.25_f32.into_wpp_field(), 3.25_f64);
        assert_eq!((&3.25_f32).into_wpp_field(), 3.25_f64);
    }

    #[test]
    fn event_id_includes_field_types() {
        let format = "Generic values: first={}, second={}";
        let integer = compute_event_id(format, &["ItemLong", "ItemString"]);
        let irql = compute_event_id(
            format,
            &["ItemListByte(Low,APC,DPC)", "ItemString"],
        );
        assert_ne!(integer, irql);
    }

    #[test]
    fn event_format_line_contains_schema_id() {
        const LINE: AnnotationString<128> = AnnotationString::event_format_line(
            "Provider",
            12345,
            &["%0value=%10", "!ld!", "\" //   LEVEL=INFO"],
        );
        assert_eq!(
            LINE.as_str(),
            "#typev Provider_12345 12345 \"%0value=%10!ld!\" //   LEVEL=INFO"
        );
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
    pub const fn event_format_line(
        provider_name: &str,
        event_id: u16,
        format_parts: &[&str],
    ) -> Self {
        let mut result = Self {
            bytes: [0; CAPACITY],
            len: 0,
        };

        result.push_str("#typev ");
        result.push_str(provider_name);
        result.push_byte(b'_');
        result.push_u16(event_id);
        result.push_byte(b' ');
        result.push_u16(event_id);
        result.push_str(" \"");

        let mut index = 0;
        while index < format_parts.len() {
            result.push_str(format_parts[index]);
            index += 1;
        }

        result
    }

    const fn push_str(&mut self, value: &str) {
        let bytes = value.as_bytes();
        let mut index = 0;
        while index < bytes.len() {
            self.push_byte(bytes[index]);
            index += 1;
        }
    }

    const fn push_byte(&mut self, value: u8) {
        assert!(
            self.len < CAPACITY,
            "WPP annotation string exceeds its buffer capacity"
        );
        self.bytes[self.len] = value;
        self.len += 1;
    }

    const fn push_u16(&mut self, value: u16) {
        let divisor = if value >= 10_000 {
            10_000
        } else if value >= 1_000 {
            1_000
        } else if value >= 100 {
            100
        } else if value >= 10 {
            10
        } else {
            1
        };
        let mut remaining = value;
        let mut current_divisor = divisor;

        loop {
            self.push_byte(b'0' + (remaining / current_divisor) as u8);
            remaining %= current_divisor;
            if current_divisor == 1 {
                break;
            }
            current_divisor /= 10;
        }
    }

    #[doc(hidden)]
    pub const fn as_str(&self) -> &str {
        let bytes = unsafe { core::slice::from_raw_parts(self.bytes.as_ptr(), self.len) };
        unsafe { core::str::from_utf8_unchecked(bytes) }
    }
}

/// Computes a stable event ID from a format string and its monomorphized WPP
/// field types.
#[doc(hidden)]
pub const fn compute_event_id(format: &str, type_names: &[&str]) -> u16 {
    let mut hash = fnv1a_64_update(0xCBF29CE484222325, format.as_bytes());
    let mut index = 0;
    while index < type_names.len() {
        hash = fnv1a_64_update(hash, &[0]);
        hash = fnv1a_64_update(hash, type_names[index].as_bytes());
        index += 1;
    }
    ((hash % 65_534) + 1) as u16
}

const fn fnv1a_64_update(mut hash: u64, bytes: &[u8]) -> u64 {
    let mut index = 0;
    while index < bytes.len() {
        hash ^= bytes[index] as u64;
        hash = hash.wrapping_mul(0x100000001B3);
        index += 1;
    }
    hash
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

impl IntoWppField for f32 {
    type Output = f64;

    #[inline]
    fn into_wpp_field(self) -> f64 {
        self.into()
    }
}

impl IntoWppField for &f32 {
    type Output = f64;

    #[inline]
    fn into_wpp_field(self) -> f64 {
        (*self).into()
    }
}

impl<T: WppValue> IntoWppField for T {
    type Output = WppValueField<T>;

    fn into_wpp_field(self) -> Self::Output {
        WppValueField {
            wire: self.into_wire(),
            marker: PhantomData,
        }
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
    f64, bool, usize, isize,
}
