// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP software tracing runtime for Rust drivers.
//!
//! Provides the runtime types and ETW bindings needed by the code generated
//! by `wpp_control_guids!` and `trace!` macros, plus the legacy `WppWriter`
//! type for WPP trace initialization with auto-log/IFR support.
//!
//! Macros (`trace!`, `wpp_init!`, `wpp_control_guids!`,
//! `define_trace_writer_methods!`) are re-exported from the `wpp-macros`
//! proc-macro crate.

#![no_std]
#![allow(non_snake_case, non_camel_case_types)]

extern crate alloc;

// ── New native ETW modules ──────────────────────────────────────────────────

#[cfg(any(
    driver_model__driver_type = "WDM",
    driver_model__driver_type = "KMDF",
    driver_model__driver_type = "UMDF"
))]
pub mod etw;

pub mod field;
pub mod provider;

pub use field::WppField;
pub use provider::ProviderState;

/// GUID layout matching the Windows GUID structure.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct GUID {
    pub data1: u32,
    pub data2: u16,
    pub data3: u16,
    pub data4: [u8; 8],
}

// ── Legacy writer modules (auto-log / IFR support) ──────────────────────────

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub mod writer;

#[cfg(any(driver_model__driver_type = "KMDF", driver_model__driver_type = "UMDF"))]
pub mod wpp_arg;

// ── Re-export proc macros from wpp-macros ───────────────────────────────────

pub use wpp_macros::define_trace_writer_methods;
pub use wpp_macros::trace;
pub use wpp_macros::wpp_init;

// New native-style macros
pub use wpp_macros::__wpp_trace_impl;
pub use wpp_macros::wpp_control_guids;
