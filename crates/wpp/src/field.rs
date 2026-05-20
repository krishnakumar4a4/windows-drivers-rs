// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trait implemented by types that can be serialized into ETW event data.

use alloc::ffi::CString;
use core::ffi::CStr;

/// The associated constants are resolved during monomorphization, allowing
/// `codeview_annotation` to emit the concrete type name into the PDB.
pub trait WppField {
    /// The Rust type name as a string, e.g. `"u32"`, `"i64"`, `"bool"`.
    const TYPE_NAME: &'static str;

    /// Returns the raw event data as a byte slice for ETW serialization.
    fn as_bytes(&self) -> &[u8];
}

macro_rules! impl_wpp_field {
    ($($ty:ty => $name:literal),* $(,)?) => {
        $(
            impl WppField for $ty {
                const TYPE_NAME: &'static str = $name;

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
    i8    => "i8",
    u8    => "u8",
    i16   => "i16",
    u16   => "u16",
    i32   => "i32",
    u32   => "u32",
    i64   => "i64",
    u64   => "u64",
    f32   => "f32",
    f64   => "f64",
    bool  => "bool",
    usize => "usize",
    isize => "isize",
}

impl WppField for &CStr {
    const TYPE_NAME: &'static str = "&CStr";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        CStr::to_bytes_with_nul(self)
    }
}

impl WppField for CString {
    const TYPE_NAME: &'static str = "CString";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        CString::as_bytes_with_nul(self)
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
