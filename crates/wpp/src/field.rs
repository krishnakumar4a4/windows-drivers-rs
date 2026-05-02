// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trait implemented by types that can be serialized into ETW event data.

/// The associated constants are resolved during monomorphization, allowing
/// `codeview_annotation` to emit the concrete type name into the PDB.
pub trait WppField {
    /// The Rust type name as a string, e.g. `"u32"`, `"i64"`, `"bool"`.
    const TYPE_NAME: &'static str;

    /// Returns the raw event data as a byte slice for ETW serialization.
    fn as_trace_bytes(&self) -> &[u8];
}

macro_rules! impl_wpp_field {
    ($($ty:ty => $name:literal),* $(,)?) => {
        $(
            impl WppField for $ty {
                const TYPE_NAME: &'static str = $name;

                #[inline]
                fn as_trace_bytes(&self) -> &[u8] {
                    unsafe {
                        core::slice::from_raw_parts(
                            self as *const Self as *const u8,
                            core::mem::size_of::<Self>(),
                        )
                    }
                }
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

impl WppField for &str {
    const TYPE_NAME: &'static str = "&str";

    #[inline]
    fn as_trace_bytes(&self) -> &[u8] {
        self.as_bytes()
    }
}
