// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Sealed trait-based type system for WPP tracing.
//!
//! This module provides a sealed trait ([`TraceData`]) that maps Rust types
//! to their byte representation for WPP trace message arguments. Each
//! supported type implements [`as_bytes`](TraceData::as_bytes) to return a
//! byte slice that the `trace!` macro expands into variadic `(pointer, length)`
//! FFI argument pairs.

use super::result::{NtStatus, NtStatusError, NtStatusNonError};

mod sealed {
    /// Sealed trait to prevent external implementations of [`super::TraceData`].
    pub trait Sealed {}
}

/// Trait for types that can be used as WPP trace message arguments.
///
/// This is a sealed trait — only types defined in this module can implement it.
/// Each implementation provides [`as_bytes`](TraceData::as_bytes) which returns
/// a byte slice pointing to the value's data. The `trace!` macro calls this
/// and passes the result's `.as_ptr()` and `.len()` as variadic FFI arguments.
///
/// # Supported Types
///
/// | Rust Type          | Byte Representation          |
/// |--------------------|------------------------------|
/// | `i8`, `i16`, `i32` | Native-endian bytes          |
/// | `u8`, `u16`, `u32` | Native-endian bytes          |
/// | `i64`, `isize`     | Native-endian bytes          |
/// | `u64`, `usize`     | Native-endian bytes          |
/// | `CString`          | Bytes with null terminator   |
/// | `NtStatus`         | Inner `i32` code bytes       |
/// | `NtStatusError`    | Inner `i32` code bytes       |
/// | `NtStatusNonError` | Inner `i32` code bytes       |
pub trait TraceData: sealed::Sealed {
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

macro_rules! impl_trace_data_int {
    ($ty:ty) => {
        impl sealed::Sealed for $ty {}
        impl TraceData for $ty {
            #[inline]
            fn as_bytes(&self) -> &[u8] {
                unsafe { // TODO: This would be an extra runtime overhead constructing the slice.
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
impl_trace_data_int!(i8);
impl_trace_data_int!(i16);
impl_trace_data_int!(i32);
impl_trace_data_int!(i64);
impl_trace_data_int!(isize);

// Unsigned integers
impl_trace_data_int!(u8);
impl_trace_data_int!(u16);
impl_trace_data_int!(u32);
impl_trace_data_int!(u64);
impl_trace_data_int!(usize);

// Boolean — traced as an i32 (1 = true, 0 = false)
impl sealed::Sealed for bool {}
impl TraceData for bool {
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        static TRUE_BYTES: [u8; core::mem::size_of::<i32>()] = 1i32.to_ne_bytes();
        static FALSE_BYTES: [u8; core::mem::size_of::<i32>()] = 0i32.to_ne_bytes();
        if *self { &TRUE_BYTES } else { &FALSE_BYTES }
    }
}

// ---------------------------------------------------------------------------
// NTSTATUS implementations (NtStatus, NtStatusError, NtStatusNonError)
// ---------------------------------------------------------------------------

/// [`NtStatus`] implementation — the top-level NTSTATUS enum.
///
/// Returns the bytes of the inner `i32` status code via [`NtStatus::code_ref`].
impl sealed::Sealed for NtStatus {}
impl TraceData for NtStatus {
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

/// [`NtStatusError`] implementation — the error subset of NTSTATUS.
impl sealed::Sealed for NtStatusError {}
impl TraceData for NtStatusError {
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

/// [`NtStatusNonError`] implementation — the success/informational/warning
/// subset of NTSTATUS.
impl sealed::Sealed for NtStatusNonError {}
impl TraceData for NtStatusNonError {
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
// ---------------------------------------------------------------------------

/// `CString` implementation for pre-converted null-terminated strings.
///
/// String arguments in `trace!` are first converted to `CString` by the macro,
/// then `as_bytes()` returns the bytes including the null terminator.
impl sealed::Sealed for alloc::ffi::CString {}
impl TraceData for alloc::ffi::CString {
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.as_bytes_with_nul()
    }
}
