// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Sealed trait-based type system for WPP tracing.
//!
//! This module provides a sealed trait ([`TraceData`]) that maps Rust types
//! to their byte representation for WPP trace message arguments. Each
//! supported type implements [`as_bytes`](TraceData::as_bytes) to return a
//! byte slice that the `trace!` macro expands into variadic `(pointer, length)`
//! FFI argument pairs.

use super::result::{NtStatus, NtStatusError, NtStatusNonError, HResult};

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
    /// The ETW type name for this traceable type (e.g., "ItemLong", "ItemString").
    /// Used by the `trace!` macro's generic annotation function to emit
    /// the correct type metadata in the PDB codeview annotation body.
    const ETW_TYPE: &'static str;

    const FORMAT_SPEC: &'static str;

    
    /// Returns the byte representation of this value for trace logging.
    ///
    /// The returned slice must remain valid for the duration of the trace
    /// call. The `trace!` macro uses `.as_ptr()` and `.len()` to pass the
    /// data as variadic FFI arguments.
    fn as_bytes(&self) -> &[u8];
}

// /// Const helper — no longer needed; the `trace!` macro now uses a generic
// /// function with `T::ETW_TYPE` directly in the codeview annotation.
// #[rustc_force_inline]
// pub const fn format_spec_of<T: TraceData>(_: &T) -> &'static str {
//     T::ETW_TYPE
// }

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
    ($ty:ty, $etw:expr, $fmt:expr) => {
        ::paste::paste! {
            #[doc(hidden)]
            static [<__ETW_TYPE_ $ty:upper>]: &str = $etw;

            #[doc(hidden)]
            static [<__FORMAT_SPEC_ $ty:upper>]: &str = $fmt;

            impl sealed::Sealed for $ty {}
            impl TraceData for $ty {
                const ETW_TYPE: &'static str = [<__ETW_TYPE_ $ty:upper>];
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
impl_trace_data_int!(i8, "ItemChar", "c");
impl_trace_data_int!(i16, "ItemShort", "hd");
impl_trace_data_int!(i32, "ItemLong", "d");
impl_trace_data_int!(i64, "ItemLongLong", "I64d");

// Unsigned integers
impl_trace_data_int!(u8, "ItemChar", "c");
impl_trace_data_int!(u16, "ItemShort", "hu");
impl_trace_data_int!(u32, "ItemLong", "u");
impl_trace_data_int!(u64, "ItemULongLong", "I64u");

// Pointer-sized integers — target-dependent ETW type
impl sealed::Sealed for isize {}
impl TraceData for isize {
    const ETW_TYPE: &'static str = if core::mem::size_of::<isize>() == 8 {
        "ItemLongLong"
    } else {
        "ItemLong"
    };
    const FORMAT_SPEC: &'static str = if core::mem::size_of::<usize>() == 8 {
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
impl TraceData for usize {
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

// Boolean — traced as an i32 (1 = true, 0 = false)
#[doc(hidden)]
static __ETW_TYPE_BOOL: &str = "ItemListByte(false,true)";

impl sealed::Sealed for bool {}
impl TraceData for bool {
    const ETW_TYPE: &'static str = __ETW_TYPE_BOOL;
    const FORMAT_SPEC: &'static str = "s";

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
static __ETW_TYPE_NTSTATUS: &str = "ItemNTSTATUS";

impl sealed::Sealed for NtStatus {}
impl TraceData for NtStatus {
    const ETW_TYPE: &'static str = __ETW_TYPE_NTSTATUS;
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

/// [`NtStatusError`] implementation — the error subset of NTSTATUS.
impl sealed::Sealed for NtStatusError {}
impl TraceData for NtStatusError {
    const ETW_TYPE: &'static str = __ETW_TYPE_NTSTATUS;
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

/// [`NtStatusNonError`] implementation — the success/informational/warning
/// subset of NTSTATUS.
impl sealed::Sealed for NtStatusNonError {}
impl TraceData for NtStatusNonError {
    const ETW_TYPE: &'static str = __ETW_TYPE_NTSTATUS;
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
// ---------------------------------------------------------------------------

/// `CString` implementation for pre-converted null-terminated strings.
///
/// String arguments in `trace!` are first converted to `CString` by the macro,
/// then `as_bytes()` returns the bytes including the null terminator.
#[doc(hidden)]
static __ETW_TYPE_CSTRING: &str = "ItemString";

impl sealed::Sealed for alloc::ffi::CString {}
impl TraceData for alloc::ffi::CString {
    const ETW_TYPE: &'static str = __ETW_TYPE_CSTRING;
    const FORMAT_SPEC: &'static str = "s";

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.as_bytes_with_nul()
    }
}

// ---------------------------------------------------------------------------
// HRESULT implementation
// ---------------------------------------------------------------------------
#[doc(hidden)]
static __ETW_TYPE_HRESULT: &str = "ItemHRESULT";

impl sealed::Sealed for HResult {}
impl TraceData for HResult {
    const ETW_TYPE: &'static str = __ETW_TYPE_HRESULT;
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

#[doc(hidden)]
static __ETW_TYPE_GUID: &str = "ItemGuid";

impl sealed::Sealed for Guid {}
impl TraceData for Guid {
    const ETW_TYPE: &'static str = __ETW_TYPE_GUID;
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
