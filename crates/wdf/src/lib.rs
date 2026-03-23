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
    pub use crate::tracing::{strlen, WppAutoLogTrace, TRACE_GUID, WPP_TRACE_OPTIONS};
    pub use crate::driver::{get_auto_log_context, get_wpp_logger, get_wpp_trace_message};
    pub use crate::trace_manifest::{
        fixed_str, str_from_fixed, TraceArgRecord, TraceManifestRecord,
        TRACE_MANIFEST_MAGIC, TRACE_MANIFEST_VERSION,
        TRACE_MANIFEST_SENTINEL_START, TRACE_MANIFEST_SENTINEL_END,
        MAX_ARG_NAME, MAX_ARGS, MAX_CRATE_NAME,
        MAX_FILE_NAME, MAX_FLAG_NAME, MAX_FORMAT_STR, MAX_ITEM_TYPE, MAX_LEVEL_NAME,
    };
    pub use wdk_sys::{LPCGUID, LPCSTR, LPGUID, UCHAR, ULONG, USHORT, TRACEHANDLE, PVOID};
}

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
extern crate wdk_panic;

#[cfg(driver_model__driver_type = "KMDF")]
use wdk_alloc::WdkAllocator;

#[cfg(driver_model__driver_type = "KMDF")]
#[global_allocator]
static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;
