// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Idiomatic Rust wrappers for the Windows Driver Foundation (WDF) APIs

#![no_std]
#![allow(non_snake_case, non_camel_case_types)]

extern crate alloc;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
mod api;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub use api::*;

/// Internal module for macro-generated code. Not for public use.
#[doc(hidden)]
#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub mod __internal {
    pub use wpp::writer::{strlen, WppAutoLogTrace};
    pub use wpp::wpp_arg::WppArgData;
    pub use wpp::wpp_arg::TraceFmtBuf;
    pub use wdk_sys::{GUID, LPCGUID, LPCSTR, LPGUID, UCHAR, ULONG, USHORT, TRACEHANDLE, PVOID};
    pub use alloc::ffi::CString;
}

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
extern crate wdk_panic;

#[cfg(driver_model__driver_type = "KMDF")]
use wdk_alloc::WdkAllocator;

#[cfg(driver_model__driver_type = "KMDF")]
#[global_allocator]
static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;
