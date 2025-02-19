// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Idiomatic Rust wrappers for the Windows Driver Foundation (WDF) APIs

#![no_std]

#![allow(non_snake_case, non_camel_case_types)]

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
mod api;
#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
mod shim_layer;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub use api::*;
#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub use shim_layer::*;

#[doc(hidden)]
#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub use wdk_sys::{DRIVER_OBJECT, NTSTATUS, PCUNICODE_STRING};
#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub use wdk::println;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
extern crate wdk_panic;


// extern crate alloc;

// use alloc::{vec::Vec, boxed::Box, string::String};
// use core::{mem, ptr};

#[cfg(driver_model__driver_type = "KMDF")]
use wdk_alloc::WdkAllocator;

#[cfg(driver_model__driver_type = "KMDF")]
#[global_allocator]
static GLOBAL_ALLOCATOR: WdkAllocator = WdkAllocator;
