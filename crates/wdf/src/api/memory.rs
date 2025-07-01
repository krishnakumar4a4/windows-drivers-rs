use crate::api::{error::NtError, object::{Handle, impl_handle}};
use wdk_sys::{
    call_unsafe_wdf_function_binding, NT_SUCCESS,
};

impl_handle!(Memory);

impl Memory {
    pub fn copy_from_buffer(
        &mut self,
        offset: usize,
        buffer: &[u8],
    ) -> Result<(), NtError> {
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCopyFromBuffer,
                self.as_ptr() as *mut _,
                offset,
                buffer.as_ptr() as *mut _,
                buffer.len()
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    pub fn copy_to_buffer(
        &self,
        offset: usize,
        buffer: &mut [u8],
    ) -> Result<(), NtError> {
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCopyToBuffer,
                self.as_ptr() as *mut _,
                offset,
                buffer.as_mut_ptr() as *mut _,
                buffer.len()
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }
}