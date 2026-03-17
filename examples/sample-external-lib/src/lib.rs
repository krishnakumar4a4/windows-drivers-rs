// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! # Sample External Library
//!
//! This library crate lives outside the driver workspace and is consumed
//! as a path dependency. It is statically linked (rlib) into the driver.
//! It demonstrates that trace macros work correctly in external path
//! dependencies.

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

/// A helper function that doubles the input value and emits a trace.
pub fn external_helper(value: i32) -> i32 {
    trace!(Information, "Trace: external_helper called with value {}", value: i32);
    value * 2
}

/// Computes the sum of two values plus one, emitting a trace.
pub fn external_compute(a: i32, b: i32) -> i32 {
    trace!("Trace: external_compute called with a={}, b={}", a: i32, b: i32);
    a + b + 1
}

/// Returns a fixed configuration value, emitting a verbose trace.
pub fn external_get_config() -> i32 {
    trace!(Verbose, "Trace: external_get_config called");
    256
}

/// A new function added for incremental compilation test.
pub fn external_incremental_test(val: i32) -> i32 {
    trace!(Information, "Trace: external_incremental_test with val={}", val: i32);
    val * 3
}
