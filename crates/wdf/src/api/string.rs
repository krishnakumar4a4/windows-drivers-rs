extern crate alloc;

use alloc::{string::String, vec::Vec, boxed::Box};
use wdk_sys::UNICODE_STRING;

pub fn to_unicode_string(buf: &[u16]) -> UNICODE_STRING {
    let byte_len = (buf.len() * 2) as u16;
    UNICODE_STRING {
        Length: byte_len - 2, // Length excluding the null terminator
        MaximumLength: byte_len,
        Buffer: buf.as_ptr() as *mut _,
    }
}

pub fn to_rust_str(unicode_str: UNICODE_STRING) -> String {
    let unicode_slice = unsafe { core::slice::from_raw_parts(unicode_str.Buffer, unicode_str.Length as usize / 2) };
    String::from_utf16_lossy(unicode_slice)
}

pub fn to_utf16_buf(rust_str: &str) -> Box<[u16]> {
    let utf16_vec = rust_str.encode_utf16()
        .chain(core::iter::once(0)) // Append null terminator
        .collect::<Vec<_>>();
    utf16_vec.into_boxed_slice()
}