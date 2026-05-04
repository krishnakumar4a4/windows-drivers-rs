// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Procedural macros for WPP tracing in Windows drivers.
//!
//! This crate provides the `trace!`, `define_trace_writer_methods!`, and
//! `wpp_init!` proc macros used by the `wpp` runtime crate and driver code.
//!
//! # Modules
//!
//! - [`guid`] — GUID utilities (validation, token generation, deterministic hashing)
//! - [`format_spec`] — C-style format specifier mapping from `defaultwpp.ini`
//! - [`trace_args`] — Trace argument parsing for the `trace!` macro
//! - [`trace_macro`] — `trace!` macro implementation
//! - [`writer_methods`] — `define_trace_writer_methods!` macro implementation
//! - [`wpp_init`] — `wpp_init!` macro implementation (control blocks, flags, levels)

mod guid;
mod format_spec;
mod trace_args;
mod trace_macro;
mod writer_methods;
mod wpp_init;

use proc_macro::TokenStream;

/// A procedural macro for WPP-style tracing with C-style format specifiers.
///
/// # Usage
///
/// ```ignore
/// trace!("Value: %d, Hex: %x, Str: %s", my_int, my_hex, my_str);
/// trace!(FLAG_ONE, Information, "Status: %!STATUS!", status);
/// ```
#[proc_macro]
pub fn trace(item: TokenStream) -> TokenStream {
    trace_macro::trace_impl(item)
}

/// Emits inherent `trace_0`..`trace_8` methods on `wpp::writer::WppWriter`.
///
/// Intended for internal use from inside the `wpp` crate.
#[proc_macro]
pub fn define_trace_writer_methods(_input: TokenStream) -> TokenStream {
    writer_methods::define_trace_writer_methods_impl()
}

/// Initializes WPP tracing infrastructure for a driver.
///
/// Generates `WppFlag` enum, `TraceLevel` enum, control block array, and
/// a `__wpp_driver_init()` helper called from `driver_entry`.
#[proc_macro]
pub fn wpp_init(item: TokenStream) -> TokenStream {
    wpp_init::wpp_init_impl(item)
}
