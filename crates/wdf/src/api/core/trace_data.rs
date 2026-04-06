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
    /// Sealed trait to prevent external implementations of
    /// [`super::TraceData`].
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
    /// A compile-time string describing the WPP format specifier for this type.
    /// Used by the `trace!` macro to pass type metadata into
    /// `codeview_annotation!`.
    const FORMAT_SPEC: &'static str;

    /// Returns the byte representation of this value for trace logging.
    ///
    /// The returned slice must remain valid for the duration of the trace
    /// call. The `trace!` macro uses `.as_ptr()` and `.len()` to pass the
    /// data as variadic FFI arguments.
    fn as_bytes(&self) -> &[u8];
}

/// Const helper to resolve [`TraceData::FORMAT_SPEC`] from a value reference.
///
/// This is a `const fn` so the compiler can evaluate it at compile time,
/// returning the `&'static str` backed by each type's global static.
#[rustc_force_inline]
pub const fn format_spec_of<T: TraceData>(_: &T) -> &'static str {
    T::FORMAT_SPEC
}

// Compile time reflection using nightly rust features
use core::any::TypeId;
use core::mem::type_info::{Type, TypeKind};

// #[inline(always)]
// const fn primitive_name_from_info<T: 'static>() -> &'static str {
//     let ty = TypeId::of::<T>().info();

//     match ty.kind {
//         TypeKind::Bool(_) => "bool",
//         TypeKind::Char(_) => "char",
//         TypeKind::Float(f) => match f.bits {
//             16 => "f16",
//             32 => "f32",
//             64 => "f64",
//             128 => "f128",
//             _ => "unknown-float",
//         },
//         TypeKind::Int(_) => primitive_int_name::<T>(),
//         _ => "non-primitive",
//     }
// }

// #[inline(always)]
// const fn primitive_int_name<T: 'static>() -> &'static str {
//     // Current nightly docs clearly expose TypeKind::Int(Int),
//     // but the fetched docs here don't expose Int's exact public fields.
//     // So for a compileable sample, dispatch by concrete type parameter.
//     if TypeId::of::<T>() == TypeId::of::<u8>() {
//         "u8"
//     } else if TypeId::of::<T>() == TypeId::of::<u16>() {
//         "u16"
//     } else if TypeId::of::<T>() == TypeId::of::<u32>() {
//         "u32"
//     } else if TypeId::of::<T>() == TypeId::of::<u64>() {
//         "u64"
//     } else if TypeId::of::<T>() == TypeId::of::<u128>() {
//         "u128"
//     } else if TypeId::of::<T>() == TypeId::of::<usize>() {
//         "usize"
//     } else if TypeId::of::<T>() == TypeId::of::<i8>() {
//         "i8"
//     } else if TypeId::of::<T>() == TypeId::of::<i16>() {
//         "i16"
//     } else if TypeId::of::<T>() == TypeId::of::<i32>() {
//         "i32"
//     } else if TypeId::of::<T>() == TypeId::of::<i64>() {
//         "i64"
//     } else if TypeId::of::<T>() == TypeId::of::<i128>() {
//         "i128"
//     } else if TypeId::of::<T>() == TypeId::of::<isize>() {
//         "isize"
//     } else {
//         "unknown-int"
//     }
// }

#[inline(always)]
pub const fn primitive_name_of_val<T: 'static>(_: &T) -> &'static str {
    const {     
        let ty = TypeId::of::<T>().info();
        match ty.kind {
            TypeKind::Bool(_) => "bool",
            TypeKind::Char(_) => "char",
            TypeKind::Float(f) => match f.bits {
                16 => "f16",
                32 => "f32",
                64 => "f64",
                128 => "f128",
                _ => "unknown-float",
            },
            TypeKind::Int(_) => {
                // Current nightly docs clearly expose TypeKind::Int(Int),
                // but the fetched docs here don't expose Int's exact public fields.
                // So for a compileable sample, dispatch by concrete type parameter.
                if TypeId::of::<T>() == TypeId::of::<u8>() {
                    "u8"
                } else if TypeId::of::<T>() == TypeId::of::<u16>() {
                    "u16"
                } else if TypeId::of::<T>() == TypeId::of::<u32>() {
                    "u32"
                } else if TypeId::of::<T>() == TypeId::of::<u64>() {
                    "u64"
                } else if TypeId::of::<T>() == TypeId::of::<u128>() {
                    "u128"
                } else if TypeId::of::<T>() == TypeId::of::<usize>() {
                    "usize"
                } else if TypeId::of::<T>() == TypeId::of::<i8>() {
                    "i8"
                } else if TypeId::of::<T>() == TypeId::of::<i16>() {
                    "i16"
                } else if TypeId::of::<T>() == TypeId::of::<i32>() {
                    "i32"
                } else if TypeId::of::<T>() == TypeId::of::<i64>() {
                    "i64"
                } else if TypeId::of::<T>() == TypeId::of::<i128>() {
                    "i128"
                } else if TypeId::of::<T>() == TypeId::of::<isize>() {
                    "isize"
                } else {
                    "unknown-int"
                }
            },
            _ => "non-primitive",
        } 
    }
}


// ---------------------------------------------------------------------------
// Integer type implementations
// ---------------------------------------------------------------------------

macro_rules! impl_trace_data_int {
    ($ty:ty, $spec:expr) => {
        ::paste::paste! {
            #[doc(hidden)]
            static [<__FORMAT_SPEC_ $ty:upper>]: &str = $spec;

            impl sealed::Sealed for $ty {}
            impl TraceData for $ty {
                const FORMAT_SPEC: &'static str = [<__FORMAT_SPEC_ $ty:upper>];

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
        }
    };
}

// Signed integers
impl_trace_data_int!(i8, "ItemLong");
impl_trace_data_int!(i16, "ItemLong");
impl_trace_data_int!(i32, "ItemLong");
impl_trace_data_int!(i64, "ItemLongLong");

// Unsigned integers
impl_trace_data_int!(u8, "ItemULong");
impl_trace_data_int!(u16, "ItemULong");
impl_trace_data_int!(u32, "ItemULong");
impl_trace_data_int!(u64, "ItemULongLong");

// Pointer-sized integers — target-dependent ETW type
impl sealed::Sealed for isize {}
impl TraceData for isize {
    const FORMAT_SPEC: &'static str = if core::mem::size_of::<isize>() == 8 {
        "ItemLongLong"
    } else {
        "ItemLong"
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
impl TraceData for usize {
    const FORMAT_SPEC: &'static str = if core::mem::size_of::<usize>() == 8 {
        "ItemULongLong"
    } else {
        "ItemULong"
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

// Boolean — traced as an i32 (1 = true, 0 = false)
#[doc(hidden)]
static __FORMAT_SPEC_BOOL: &str = "ItemListLong(false,true)";

impl sealed::Sealed for bool {}
impl TraceData for bool {
    const FORMAT_SPEC: &'static str = __FORMAT_SPEC_BOOL;

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
#[doc(hidden)]
static __FORMAT_SPEC_NTSTATUS: &str = "ItemNTSTATUS";

impl sealed::Sealed for NtStatus {}
impl TraceData for NtStatus {
    const FORMAT_SPEC: &'static str = __FORMAT_SPEC_NTSTATUS;

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
    const FORMAT_SPEC: &'static str = __FORMAT_SPEC_NTSTATUS;

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
    const FORMAT_SPEC: &'static str = __FORMAT_SPEC_NTSTATUS;

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
#[doc(hidden)]
static __FORMAT_SPEC_CSTRING: &str = "ItemString";

impl sealed::Sealed for alloc::ffi::CString {}
impl TraceData for alloc::ffi::CString {
    const FORMAT_SPEC: &'static str = __FORMAT_SPEC_CSTRING;

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.as_bytes_with_nul()
    }
}
