// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WppArgData implementations for wdf-specific types.
//!
//! The `WppArgData` sealed trait and primitive implementations live in
//! `wpp::wpp_arg`. This module adds implementations for wdf types that
//! cannot live in the `wpp` crate due to the dependency direction.

use wpp::wpp_arg::{sealed::Sealed, WppArgData};

use super::result::{HResult, NtStatus, NtStatusError, NtStatusNonError};
use super::guid::Guid;

// ---------------------------------------------------------------------------
// NTSTATUS implementations (NtStatus, NtStatusError, NtStatusNonError)
// ---------------------------------------------------------------------------

impl Sealed for NtStatus {}
impl WppArgData for NtStatus {
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

impl Sealed for NtStatusError {}
impl WppArgData for NtStatusError {
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

impl Sealed for NtStatusNonError {}
impl WppArgData for NtStatusNonError {
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
// HRESULT implementation
// ---------------------------------------------------------------------------

impl Sealed for HResult {}
impl WppArgData for HResult {
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

impl Sealed for Guid {}
impl WppArgData for Guid {
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
