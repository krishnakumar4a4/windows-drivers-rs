// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Proc macros for WPP software tracing.

mod control_guids;
mod trace_impl;

use proc_macro::TokenStream;

/// Declare WPP trace providers with control GUIDs and keywords.
///
/// Must be invoked at the crate root. Generates a provider module and a
/// `trace!` macro for emitting ETW events with compile-time PDB annotations.
///
/// # Example
///
/// ```ignore
/// wpp_control_guids!(
///     MyProvider 84bdb2e9-829e-41b3-b891-02f454bc2bd7 {
///         GENERAL,
///         IO,
///         CONFIG,
///     }
/// );
/// ```
#[proc_macro]
pub fn wpp_control_guids(input: TokenStream) -> TokenStream {
    match control_guids::generate(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Internal proc macro called by the generated `trace!` macro.
#[doc(hidden)]
#[proc_macro]
pub fn __wpp_trace_impl(input: TokenStream) -> TokenStream {
    match trace_impl::generate(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
