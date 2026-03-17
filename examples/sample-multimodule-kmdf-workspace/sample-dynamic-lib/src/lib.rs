// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! # Sample Dynamic Library
//!
//! This library crate is part of the driver workspace and is configured
//! with `crate-type = ["dylib"]` to be dynamically linked. It demonstrates
//! trace macros in a dynamically linked library dependency.

#![no_std]

extern crate alloc;

use wdf::trace;

/// Standard WPP trace levels (must be at crate root for trace! macro).
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TraceLevel {
    /// No tracing
    None = 0,
    /// Critical errors
    Critical = 1,
    /// Non-critical errors
    Error = 2,
    /// Warnings
    Warning = 3,
    /// Informational messages
    Information = 4,
    /// Verbose debug messages
    Verbose = 5,
    /// Reserved level 6
    Reserved6 = 6,
    /// Reserved level 7
    Reserved7 = 7,
    /// Reserved level 8
    Reserved8 = 8,
    /// Reserved level 9
    Reserved9 = 9,
}

impl TraceLevel {
    /// Returns the numeric value of this trace level.
    #[inline]
    pub const fn value(&self) -> u8 {
        *self as u8
    }

    /// Returns true if this level is Verbose or below.
    #[inline]
    pub const fn is_verbose_or_below(&self) -> bool {
        (*self as u8) <= (TraceLevel::Verbose as u8)
    }
}

/// A helper function that adds 50 to the input value and emits a trace.
pub fn dynamic_helper(value: i32) -> i32 {
    trace!(Information, "Trace: dynamic_helper called with value {}", value: i32);
    value + 50
}

/// Multiplies two values together, emitting a trace.
pub fn dynamic_compute(a: i32, b: i32) -> i32 {
    trace!("Trace: dynamic_compute called with a={}, b={}", a: i32, b: i32);
    a * b
}

/// Returns a dynamic configuration value, emitting a verbose trace.
pub fn dynamic_get_config() -> i32 {
    trace!(Verbose, "Trace: dynamic_get_config called");
    128
}
