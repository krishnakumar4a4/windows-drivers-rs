// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP software tracing runtime for Rust drivers.
//!
//! Provides the runtime types and ETW bindings needed by the code generated
//! by `wpp_control_guids!` and `trace!` macros.

#![no_std]

pub mod etw;
pub mod field;
pub mod provider;
#[cfg(feature = "kernel_mode")]
pub mod ifr;

pub use field::WppField;
pub use provider::ProviderState;
#[cfg(feature = "kernel_mode")]
pub use ifr::IFRState;
pub use wpp_macros::__wpp_trace_impl;
pub use wpp_macros::wpp_control_guids;

/// GUID layout matching the Windows GUID structure.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct GUID {
    pub data1: u32,
    pub data2: u16,
    pub data3: u16,
    pub data4: [u8; 8],
}
