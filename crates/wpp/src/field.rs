// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trait implemented by types that can be serialized into ETW event data.

/// The associated constants are resolved during monomorphization, allowing
/// `codeview_annotation` to emit the concrete type name into the PDB.
pub trait WppField {
    /// The Rust type name as a string, e.g. `"u32"`, `"i64"`, `"bool"`.
    const TYPE_NAME: &'static str;

    /// Fixed serialization size in bytes.
    const FIXED_SIZE: usize;
}

macro_rules! impl_wpp_field {
    ($($ty:ty => $name:literal),* $(,)?) => {
        $(
            impl WppField for $ty {
                const TYPE_NAME: &'static str = $name;
                const FIXED_SIZE: usize = core::mem::size_of::<$ty>();
            }
        )*
    };
}

impl_wpp_field! {
    i8    => "i8",
    u8    => "u8",
    i16   => "i16",
    u16   => "u16",
    i32   => "i32",
    u32   => "u32",
    i64   => "i64",
    u64   => "u64",
    f32   => "f32",
    f64   => "f64",
    bool  => "bool",
    usize => "usize",
    isize => "isize",
}
