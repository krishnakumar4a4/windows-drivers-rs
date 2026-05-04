// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP tracing runtime for Windows drivers.
//!
//! This crate provides the `WppWriter` type for WPP trace initialization,
//! cleanup, and trace method dispatch, plus the `WppArgData` sealed trait
//! for mapping Rust types to WPP byte representations.
//!
//! Macros (`trace!`, `wpp_init!`, `define_trace_writer_methods!`) are
//! re-exported from the `wpp-macros` proc-macro crate.

#![no_std]
#![allow(non_snake_case, non_camel_case_types)]

extern crate alloc;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub mod writer;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub mod wpp_arg;

// Re-export proc macros from wpp-macros
pub use wpp_macros::define_trace_writer_methods;
pub use wpp_macros::trace;
pub use wpp_macros::wpp_init;
