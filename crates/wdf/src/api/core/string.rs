use alloc::{boxed::Box, string::String, vec::Vec};

use wdk_sys::{
    call_unsafe_wdf_function_binding,
    NT_SUCCESS,
    UNICODE_STRING,
    WDFOBJECT,
    WDFSTRING,
    WDF_NO_OBJECT_ATTRIBUTES,
};

use super::{object::Handle, result::NtResult};

// TODO: We assume that WDFSTRING always owns
// the underlying buffer. If that's not the case
// we need to change the implementation.
/// Represents a framework string object corresponding to WDFSTRING
///
/// Implements RAII to ensure proper resource management.
#[derive(Debug)]
#[repr(transparent)]
pub struct WString(WDFSTRING);

impl Handle for WString {
    #[inline(always)]
    fn as_ptr(&self) -> WDFOBJECT {
        self.0 as WDFOBJECT
    }

    fn type_name() -> String {
        String::from("WString")
    }
}

impl WString {
    pub fn create() -> NtResult<Self> {
        let mut raw_string: WDFSTRING = core::ptr::null_mut();
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfStringCreate,
                core::ptr::null_mut(),
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut raw_string
            )
        };

        if NT_SUCCESS(status) {
            Ok(Self(raw_string))
        } else {
            Err(status.into())
        }
    }

    pub fn get_unicode_string(&self) -> UNICODE_STRING {
        let mut unicode_string = UNICODE_STRING::default();

        // SAFETY: The contract of the `Wstring` constructor
        // requires that the underlying pointer is a valid WDFOBJECT.
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfStringGetUnicodeString,
                self.0,
                &mut unicode_string
            )
        }

        unicode_string
    }

    pub fn to_rust_string(&self) -> Option<String> {
        let unicode_string = self.get_unicode_string();

        if unicode_string.Buffer.is_null() {
            return None;
        }

        Some(to_rust_str(unicode_string))
    }
}

impl Drop for WString {
    fn drop(&mut self) {
        // SAFETY: The contract of the FwString type constructor
        // requires that the underlying pointer is a valid WDFOBJECT.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfObjectDelete, self.as_ptr());
        }
    }
}

/// A Rust representation of a UNICODE_STRING with an owned buffer.
pub struct UnicodeString {
    _buf: Box<[u16]>, // `_buf` exists only to keep the buffer alive
    unicode_str: UNICODE_STRING,
}

impl UnicodeString {
    pub fn from_rust_str(rust_str: &str) -> Self {
        let buf = Self::to_utf16_buf(rust_str);
        let unicode_str = Self::create_raw_unicode_string_from(&buf);
        Self {
            _buf: buf,
            unicode_str,
        }
    }

    pub unsafe fn from_raw(unicode_str: UNICODE_STRING) -> Self {
        let buf = unsafe { core::slice::from_raw_parts(
            unicode_str.Buffer,
            (unicode_str.MaximumLength / 2) as usize,
        ) }
        .to_vec()
        .into_boxed_slice();
        Self {
            _buf: buf,
            unicode_str,
        }
    }

    pub fn to_rust_string(&self) -> String {
        to_rust_str(self.unicode_str)
    }

    pub fn as_raw(&self) -> &UNICODE_STRING {
        &self.unicode_str
    }

    fn create_raw_unicode_string_from(buf: &[u16]) -> UNICODE_STRING {
        let byte_len = (buf.len() * 2) as u16;
        UNICODE_STRING {
            Length: byte_len - 2, // Length excluding the null terminator
            MaximumLength: byte_len,
            Buffer: buf.as_ptr().cast_mut().cast(),
        }
    }

    fn to_utf16_buf(rust_str: &str) -> Box<[u16]> {
        let utf16_vec = rust_str
            .encode_utf16()
            .chain(core::iter::once(0)) // Append null terminator
            .collect::<Vec<_>>();
        utf16_vec.into_boxed_slice()
    }
}

pub fn to_rust_str(unicode_str: UNICODE_STRING) -> String {
    let unicode_slice =
        unsafe { core::slice::from_raw_parts(unicode_str.Buffer, unicode_str.Length as usize / 2) };
    String::from_utf16_lossy(unicode_slice)
}
