// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Idiomatic Rust wrappers for the Windows Driver Foundation (WDF) APIs

#![no_std]
#![allow(non_snake_case, non_camel_case_types)]
#![feature(rustc_attrs)]
#![feature(generic_const_exprs)]
#![allow(incomplete_features)]
// Nightly features to get compile time reflection
#![feature(type_info)]
#![feature(const_cmp)]
#![feature(const_trait_impl)]

extern crate alloc;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
mod api;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub use api::*;

/// Internal module for macro-generated code. Not for public use.
#[doc(hidden)]
#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub mod __internal {
    pub use wdk_sys::{LPCGUID, LPCSTR, LPGUID, PVOID, TRACEHANDLE, UCHAR, ULONG, USHORT};

    pub use crate::{
        driver::{get_auto_log_context, get_wpp_logger, get_wpp_trace_message},
        trace_data::TraceData,
        // format_spec_of and primitive_name_of_val removed — the trace! macro
        // now uses a generic function with T::ETW_TYPE directly.
        tracing::{TRACE_GUID, WPP_TRACE_OPTIONS, WppAutoLogTrace, strlen},
    };
}

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
extern crate wdk_panic;

#[cfg(driver_model__driver_type = "KMDF")]
use wdk_alloc::WdkAllocator;

#[cfg(driver_model__driver_type = "KMDF")]
#[global_allocator]
static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;
