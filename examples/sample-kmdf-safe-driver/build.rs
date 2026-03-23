// // Copyright (c) Microsoft Corporation
// // License: MIT OR Apache-2.0

// //! Build script for the `sample-kmdf-safe-driver` crate.
// //!
// //! Based on the [`wdk_build::Config`] parsed from the build tree, this build
// //! script will provide `Cargo` with the necessary information to build the
// //! driver binary (ex. linker flags)

fn main() -> Result<(), wdk_build::ConfigError> {
    // let obj_path = generate_wpp_annotation_coff();

    // Tell the linker to include the annotation object so that
    // S_ANNOTATION records end up in the final PDB.
    // println!("cargo::rustc-cdylib-link-arg={}", obj_path.display());
    wdk_build::configure_wdk_binary_build()
}
