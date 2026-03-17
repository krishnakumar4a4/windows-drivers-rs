// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Build script for the `sample-static-lib` crate.

fn main() -> Result<(), wdk_build::ConfigError> {
    wdk_build::configure_wdk_library_build()
}
