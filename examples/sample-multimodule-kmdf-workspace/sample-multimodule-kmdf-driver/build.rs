// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Build script for the `sample-multimodule-kmdf-driver` crate.

fn main() -> Result<(), wdk_build::ConfigError> {
    wdk_build::configure_wdk_binary_build()
}
