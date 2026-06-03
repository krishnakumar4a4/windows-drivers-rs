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

// ---------------------------------------------------------------------------
// TraceFmtBuf — Display-to-bytes formatter for custom types
// ---------------------------------------------------------------------------

/// A formatting buffer that collects `Display` output into a `Vec<u8>` and
/// produces a null-terminated byte slice for WPP tracing.
pub struct TraceFmtBuf {
    buf: alloc::vec::Vec<u8>,
}

impl TraceFmtBuf {
    /// Creates a new empty buffer with a small pre-allocation.
    #[inline]
    pub fn new() -> Self {
        Self {
            buf: alloc::vec::Vec::with_capacity(128),
        }
    }

    /// Appends a null terminator. Must be called after writing is complete.
    #[inline]
    pub fn finalize(&mut self) {
        // Truncate at any interior null byte
        if let Some(pos) = self.buf.iter().position(|&b| b == 0) {
            self.buf.truncate(pos);
        }
        self.buf.push(0); // null terminator
    }
}

impl core::fmt::Write for TraceFmtBuf {
    #[inline]
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        self.buf.extend_from_slice(s.as_bytes());
        Ok(())
    }
}

impl WppField for TraceFmtBuf {
    const TYPE_NAME: &'static str = "CString";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        &self.buf
    }
}

// ---------------------------------------------------------------------------
// Autoref-based dispatch: IntoWppField (preferred) vs Display (fallback)
// ---------------------------------------------------------------------------

/// Wrapper used by the `trace!` macro for autoref specialization.
///
/// Method resolution prefers the **inherent** `convert(self)` (for types that
/// implement [`IntoWppField`]) over the **trait** `convert(&self)` from
/// [`WppDisplayFallback`] (for types that implement `Display`).
pub struct WppConvert<T>(pub T);

// Higher priority: inherent method for IntoWppField types.
// Rust method resolution finds this first (by-value before by-ref).
impl<T: IntoWppField> WppConvert<T> {
    #[inline]
    pub fn convert(self) -> T::Output {
        self.0.into_wpp_field()
    }
}

/// Fallback trait for types that implement `Display` but not `IntoWppField`.
/// Found by method resolution via auto-ref (`&self`) when no inherent
/// `convert` exists.
pub trait WppDisplayFallback {
    fn convert(&self) -> TraceFmtBuf;
}

impl<T: core::fmt::Display> WppDisplayFallback for WppConvert<T> {
    #[inline]
    fn convert(&self) -> TraceFmtBuf {
        use core::fmt::Write;
        let mut buf = TraceFmtBuf::new();
        let _ = write!(buf, "{}", self.0);
        buf.finalize();
        buf
    }
}
