// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Idiomatic Rust wrappers for the Windows Driver Foundation (WDF) APIs

#![cfg_attr(driver_model__driver_type = "KMDF", no_std)]
#![allow(non_snake_case, non_camel_case_types)]

extern crate alloc;

#[cfg(all(driver_model__driver_type = "KMDF", not(feature = "kernel_mode")))]
compile_error!("KMDF drivers require the `kernel_mode` feature of the `wdf` crate (enabled by default)");

#[cfg(all(driver_model__driver_type = "UMDF", feature = "kernel_mode"))]
compile_error!(
    "UMDF drivers must depend on the `wdf` crate with `default-features = false`"
);

pub extern crate wpp;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
mod api;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub use api::*;

#[cfg(driver_model__driver_type = "KMDF")]
extern crate wdk_panic;

#[cfg(driver_model__driver_type = "KMDF")]
use wdk_alloc::WdkAllocator;

#[cfg(driver_model__driver_type = "KMDF")]
#[global_allocator]
static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;
