// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Sealed trait-based type system for WPP tracing.
//!
//! This module provides a sealed trait ([`TraceArgData`]) that maps Rust types
//! to their byte representation for WPP trace message arguments. Each
//! supported type implements [`as_bytes`](TraceArgData::as_bytes) to return a
//! byte slice that the `trace!` macro expands into variadic `(pointer, length)`
//! FFI argument pairs.

use super::result::{HResult, NtStatus, NtStatusError, NtStatusNonError};

mod sealed {
    /// Sealed trait to prevent external implementations of
    /// [`super::TraceArgData`].
    pub trait Sealed {}
}

/// Trait for types that can be used as WPP trace message arguments.
///
/// This is a sealed trait — only types defined in this module can implement it.
///
/// Each implementation provides [`as_bytes`](TraceArgData::as_bytes) which returns
/// a byte slice pointing to the value's data. The `trace!` macro calls this
/// and passes the result's `.as_ptr()` and `.len()` as variadic FFI arguments.
///
/// # Supported Types
///
/// | Rust Type          | ETW Type           | C Format Spec |
/// |--------------------|--------------------|---------------|
/// | `i8`               | ItemChar           | c             |
/// | `i16`              | ItemShort          | hd            |
/// | `i32`              | ItemLong           | d             |
/// | `i64`              | ItemLongLong       | I64d          |
/// | `u8`               | ItemChar           | c             |
/// | `u16`              | ItemShort          | hu            |
/// | `u32`              | ItemLong           | u             |
/// | `u64`              | ItemULongLong      | I64u          |
/// | `isize`            | ItemLongLong/Long  | I64d/d        |
/// | `usize`            | ItemULongLong/Long | I64u/u        |
/// | `bool`             | ItemListByte       | s             |
/// | `f64`              | ItemDouble         | s             |
/// | `CString`          | ItemString         | s             |
/// | `NtStatus`         | ItemNTSTATUS       | s             |
/// | `NtStatusError`    | ItemNTSTATUS       | s             |
/// | `NtStatusNonError` | ItemNTSTATUS       | s             |
/// | `HResult`          | ItemHRESULT        | s             |
/// | `Guid`             | ItemGuid           | s             |
pub trait TraceArgData: sealed::Sealed {
    /// The ETW type name for this traceable type (e.g., "ItemLong", "ItemString").
    const ETW_TYPE: &'static str;

    /// The C-style format specifier for this type (e.g., "d", "I64u", "s").
    const FORMAT_SPEC: &'static str;

    /// Returns the byte representation of this value for trace logging.
    ///
    /// The returned slice must remain valid for the duration of the trace
    /// call. The `trace!` macro uses `.as_ptr()` and `.len()` to pass the
    /// data as variadic FFI arguments.
    fn as_bytes(&self) -> &[u8];
}

// ---------------------------------------------------------------------------
// Integer type implementations
// ---------------------------------------------------------------------------

macro_rules! impl_trace_arg_data_int {
    ($ty:ty, $etw:expr, $fmt:expr) => {
        impl sealed::Sealed for $ty {}
        impl TraceArgData for $ty {
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
impl_trace_arg_data_int!(i8, "ItemChar", "c");
impl_trace_arg_data_int!(i16, "ItemShort", "hd");
impl_trace_arg_data_int!(i32, "ItemLong", "d");
impl_trace_arg_data_int!(i64, "ItemLongLong", "I64d");

// Unsigned integers
impl_trace_arg_data_int!(u8, "ItemChar", "c");
impl_trace_arg_data_int!(u16, "ItemShort", "hu");
impl_trace_arg_data_int!(u32, "ItemLong", "u");
impl_trace_arg_data_int!(u64, "ItemULongLong", "I64u");

// Pointer-sized integers — target-dependent ETW type
impl sealed::Sealed for isize {}
impl TraceArgData for isize {
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
impl TraceArgData for usize {
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

/// `f64` implementation for WPP double-precision floats.
///
/// Maps to `ItemDouble` in ETW. WPP traces doubles as 8-byte values.
/// Used by format specifiers `%e`, `%f`, `%g`, `%E`, `%G`, and `%!DOUBLE!`.
impl sealed::Sealed for f64 {}
impl TraceArgData for f64 {
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
impl TraceArgData for bool {
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
// NTSTATUS implementations (NtStatus, NtStatusError, NtStatusNonError)
// ---------------------------------------------------------------------------

impl sealed::Sealed for NtStatus {}
impl TraceArgData for NtStatus {
    const ETW_TYPE: &'static str = "ItemNTSTATUS";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self.code_ref() as *const i32 as *const u8,
                core::mem::size_of::<i32>(),
            )
        }
    }
}

impl sealed::Sealed for NtStatusError {}
impl TraceArgData for NtStatusError {
    const ETW_TYPE: &'static str = "ItemNTSTATUS";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self.code_ref() as *const i32 as *const u8,
                core::mem::size_of::<i32>(),
            )
        }
    }
}

impl sealed::Sealed for NtStatusNonError {}
impl TraceArgData for NtStatusNonError {
    const ETW_TYPE: &'static str = "ItemNTSTATUS";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self.code_ref() as *const i32 as *const u8,
                core::mem::size_of::<i32>(),
            )
        }
    }
}

// ---------------------------------------------------------------------------
// String type implementations
//
// NOTE: The `trace!` macro does NOT call `TraceArgData` on `&str` directly.
// Instead, for `%s` (and other string format specifiers), the macro first
// converts the `&str` argument into a `CString` via
// `::wdf::__internal::CString::new(expr).unwrap()`, and then calls
// `TraceArgData::as_bytes()` on the resulting `CString`. This ensures the
// string is null-terminated as required by WPP/ETW string arguments.
//
// There is intentionally no `impl TraceArgData for &str` — all string
// arguments flow through `CString` at the macro expansion site.
// ---------------------------------------------------------------------------

impl sealed::Sealed for alloc::ffi::CString {}
impl TraceArgData for alloc::ffi::CString {
    const ETW_TYPE: &'static str = "ItemString";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.as_bytes_with_nul()
    }
}

// ---------------------------------------------------------------------------
// HRESULT implementation
// ---------------------------------------------------------------------------

impl sealed::Sealed for HResult {}
impl TraceArgData for HResult {
    const ETW_TYPE: &'static str = "ItemHRESULT";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self.code_ref() as *const i32 as *const u8,
                core::mem::size_of::<i32>(),
            )
        }
    }
}

// ---------------------------------------------------------------------------
// GUID implementation
// ---------------------------------------------------------------------------

use super::guid::Guid;

impl sealed::Sealed for Guid {}
impl TraceArgData for Guid {
    const ETW_TYPE: &'static str = "ItemGuid";
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                self.as_lpcguid() as *const u8,
                core::mem::size_of::<wdk_sys::GUID>(),
            )
        }
    }
}

// ---------------------------------------------------------------------------
// TraceFmtBuf — single-allocation Display-to-bytes formatter for custom types
//
// Used by the `trace!` macro to serialize any `Display`-implementing type
// into a null-terminated byte buffer suitable for WPP `ItemString` arguments.
//
// Flow: format_args!("{}", &val) → core::fmt::write → Vec<u8> → validate → &[u8]
//
// One allocation (Vec growth), one validation pass, zero copies.
// ---------------------------------------------------------------------------

/// A formatting buffer that collects `Display` output into a `Vec<u8>` and
/// produces a null-terminated byte slice for WPP tracing.
///
/// # Usage (generated by `trace!` macro for custom types)
/// ```ignore
/// let mut buf = TraceFmtBuf::new();
/// core::fmt::write(&mut buf, format_args!("{}", &my_struct)).unwrap();
/// let bytes = buf.finish();  // validates + appends \0, returns &[u8]
/// ```
pub struct TraceFmtBuf {
    buf: alloc::vec::Vec<u8>,
    /// Set to true if an interior null byte was detected during writing.
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

    /// Finishes formatting: appends a null terminator and returns the byte
    /// slice including the null. If interior null bytes were written by the
    /// `Display` impl, the buffer is truncated at the first null to produce
    /// a valid C string.
    #[inline]
    pub fn finish(&mut self) -> &[u8] {
        if self.has_interior_null {
            // Truncate at first interior null to produce a valid C string
            if let Some(pos) = self.buf.iter().position(|&b| b == 0) {
                self.buf.truncate(pos);
            }
        }
        self.buf.push(0); // null terminator
        &self.buf
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
