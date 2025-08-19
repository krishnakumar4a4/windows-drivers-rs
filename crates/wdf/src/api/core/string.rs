use alloc::{boxed::Box, string::String, vec::Vec};

use wdk_sys::{
    call_unsafe_wdf_function_binding,
    NT_SUCCESS,
    UNICODE_STRING,
    WDFOBJECT,
    WDFSTRING,
    WDF_NO_OBJECT_ATTRIBUTES,
};

use super::{error::NtResult, object::Handle};

pub(crate) struct WString(WDFSTRING);

impl Handle for WString {
    #[inline(always)]
    fn as_ptr(&self) -> WDFOBJECT {
        self.0 as WDFOBJECT
    }

    fn type_name() -> String {
        String::from("FwString")
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

    pub fn to_rust_string(&self) -> String {
        let mut unicode_string = UNICODE_STRING::default();

        // SAFETY: The contract of the FwString type constructor
        // requires that the underlying pointer is a valid WDFOBJECT.
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfStringGetUnicodeString,
                self.0,
                &mut unicode_string
            )
        };

        to_rust_str(unicode_string)
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

pub fn to_unicode_string(buf: &[u16]) -> UNICODE_STRING {
    let byte_len = (buf.len() * 2) as u16;
    UNICODE_STRING {
        Length: byte_len - 2, // Length excluding the null terminator
        MaximumLength: byte_len,
        Buffer: buf.as_ptr() as *mut _,
    }
}

pub fn to_rust_str(unicode_str: UNICODE_STRING) -> String {
    let unicode_slice =
        unsafe { core::slice::from_raw_parts(unicode_str.Buffer, unicode_str.Length as usize / 2) };
    String::from_utf16_lossy(unicode_slice)
}

pub fn to_utf16_buf(rust_str: &str) -> Box<[u16]> {
    let utf16_vec = rust_str
        .encode_utf16()
        .chain(core::iter::once(0)) // Append null terminator
        .collect::<Vec<_>>();
    utf16_vec.into_boxed_slice()
}
