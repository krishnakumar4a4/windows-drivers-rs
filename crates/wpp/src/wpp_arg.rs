// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Sealed trait-based type system for WPP tracing.
//!
//! This module provides a sealed trait ([`WppArgData`]) that maps Rust types
//! to their byte representation for WPP trace message arguments. Each
//! supported type implements [`as_bytes`](WppArgData::as_bytes) to return a
//! byte slice that the `trace!` macro expands into variadic `(pointer, length)`
//! FFI argument pairs.
//!
//! The `sealed` module is `#[doc(hidden)] pub` so that the `wdf` crate can
//! implement `WppArgData` for its own types (e.g. `NtStatus`, `HResult`, `Guid`).

/// Sealed trait module. Public but hidden so `wdf` can implement `WppArgData`
/// on its own types without exposing the sealing mechanism to end users.
#[doc(hidden)]
pub mod sealed {
    /// Sealed trait to prevent arbitrary external implementations of
    /// [`super::WppArgData`].
    pub trait Sealed {}
}

/// Trait for types that can be used as WPP trace message arguments.
///
/// This is a sealed trait — only types in this module and approved downstream
/// crates (via `sealed::Sealed`) can implement it.
///
/// Each implementation provides [`as_bytes`](WppArgData::as_bytes) which returns
/// a byte slice pointing to the value's data. The `trace!` macro calls this
/// and passes the result's `.as_ptr()` and `.len()` as variadic FFI arguments.
pub trait WppArgData: sealed::Sealed {
    /// The ETW type name for this traceable type (e.g., "ItemLong", "ItemString").
    const ETW_TYPE: &'static str;

    /// The C-style format specifier for this type (e.g., "d", "I64u", "s").
    const FORMAT_SPEC: &'static str;

    /// Returns the byte representation of this value for trace logging.
    fn as_bytes(&self) -> &[u8];
}

// ---------------------------------------------------------------------------
// Integer type implementations
// ---------------------------------------------------------------------------

macro_rules! impl_wpp_arg_data_int {
    ($ty:ty, $etw:expr, $fmt:expr) => {
        impl sealed::Sealed for $ty {}
        impl WppArgData for $ty {
            const ETW_TYPE: &'static str = $etw;
            const FORMAT_SPEC: &'static str = $fmt;

            #[inline]
            fn as_bytes(&self) -> &[u8] {
                unsafe {
                    core::slice::from_raw_parts(
                        self as *const $ty as *const u8,
                        core::mem::size_of::<$ty>(),
                    )
                }
            }
        }
    };
}

// Signed integers
impl_wpp_arg_data_int!(i8, "ItemChar", "c");
impl_wpp_arg_data_int!(i16, "ItemShort", "hd");
impl_wpp_arg_data_int!(i32, "ItemLong", "d");
impl_wpp_arg_data_int!(i64, "ItemLongLong", "I64d");

// Unsigned integers
impl_wpp_arg_data_int!(u8, "ItemChar", "c");
impl_wpp_arg_data_int!(u16, "ItemShort", "hu");
impl_wpp_arg_data_int!(u32, "ItemLong", "u");
impl_wpp_arg_data_int!(u64, "ItemULongLong", "I64u");

// Pointer-sized integers — target-dependent ETW type
impl sealed::Sealed for isize {}
impl WppArgData for isize {
    const ETW_TYPE: &'static str = if core::mem::size_of::<isize>() == 8 {
        "ItemLongLong"
    } else {
        "ItemLong"
    };
    const FORMAT_SPEC: &'static str = if core::mem::size_of::<isize>() == 8 {
        "I64d"
    } else {
        "d"
    };

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self as *const isize as *const u8,
                core::mem::size_of::<isize>(),
            )
        }
    }
}

impl sealed::Sealed for usize {}
impl WppArgData for usize {
    const ETW_TYPE: &'static str = if core::mem::size_of::<usize>() == 8 {
        "ItemULongLong"
    } else {
        "ItemLong"
    };
    const FORMAT_SPEC: &'static str = if core::mem::size_of::<usize>() == 8 {
        "I64u"
    } else {
        "u"
    };

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self as *const usize as *const u8,
                core::mem::size_of::<usize>(),
            )
        }
    }
}

// ---------------------------------------------------------------------------
// Floating-point type implementations
// ---------------------------------------------------------------------------

impl sealed::Sealed for f64 {}
impl WppArgData for f64 {
    const ETW_TYPE: &'static str = "ItemDouble";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self as *const f64 as *const u8,
                core::mem::size_of::<f64>(),
            )
        }
    }
}

// ---------------------------------------------------------------------------
// Boolean — traced as an i32 (1 = true, 0 = false)
// ---------------------------------------------------------------------------

impl sealed::Sealed for bool {}
impl WppArgData for bool {
    const ETW_TYPE: &'static str = "ItemListByte(false,true)";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        static TRUE_BYTES: [u8; core::mem::size_of::<i32>()] = 1i32.to_ne_bytes();
        static FALSE_BYTES: [u8; core::mem::size_of::<i32>()] = 0i32.to_ne_bytes();
        if *self {
            &TRUE_BYTES
        } else {
            &FALSE_BYTES
        }
    }
}

// ---------------------------------------------------------------------------
// String type implementations
// ---------------------------------------------------------------------------

impl sealed::Sealed for alloc::ffi::CString {}
impl WppArgData for alloc::ffi::CString {
    const ETW_TYPE: &'static str = "ItemString";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.as_bytes_with_nul()
    }
}

// ---------------------------------------------------------------------------
// TraceFmtBuf — single-allocation Display-to-bytes formatter for custom types
// ---------------------------------------------------------------------------

/// A formatting buffer that collects `Display` output into a `Vec<u8>` and
/// produces a null-terminated byte slice for WPP tracing.
pub struct TraceFmtBuf {
    buf: alloc::vec::Vec<u8>,
    has_interior_null: bool,
}

impl TraceFmtBuf {
    /// Creates a new empty buffer with a small pre-allocation.
    #[inline]
    pub fn new() -> Self {
        Self {
            buf: alloc::vec::Vec::with_capacity(128),
            has_interior_null: false,
        }
    }

    /// Finalizes formatting: truncates at any interior null byte and appends a
    /// trailing null terminator.
    #[inline]
    pub fn finalize(&mut self) {
        if self.has_interior_null {
            if let Some(pos) = self.buf.iter().position(|&b| b == 0) {
                self.buf.truncate(pos);
            }
        }
        self.buf.push(0); // null terminator
    }
}

impl core::fmt::Write for TraceFmtBuf {
    #[inline]
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        let bytes = s.as_bytes();
        if !self.has_interior_null && bytes.contains(&0) {
            self.has_interior_null = true;
        }
        self.buf.extend_from_slice(bytes);
        Ok(())
    }
}

impl sealed::Sealed for TraceFmtBuf {}
impl WppArgData for TraceFmtBuf {
    const ETW_TYPE: &'static str = "ItemString";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        &self.buf
    }
}
