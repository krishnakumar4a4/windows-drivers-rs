use wdk_sys::{call_unsafe_wdf_function_binding, NT_SUCCESS};

use super::{
    error::NtError,
    object::{impl_handle, Handle},
};

impl_handle!(Memory);

// TODO: the copy_*_buffer methods are not thread safe.
// However at this moment the only way the driver can get
// access to a Memory object is by calling retrieve_*_memory
// methods on a request object and request objects always
// requires a lock to interact with it if it's reachable
// from multiple threads. So _currently_ these methods are
// being called in a thread safe way, but that may not remain
// the case if we support creation of independent Memory objects
// tomorrow. Fix this!
impl Memory {
    pub fn copy_from_buffer(&mut self, offset: usize, buffer: &[u8]) -> Result<(), NtError> {
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

    pub fn copy_to_buffer(&self, offset: usize, buffer: &mut [u8]) -> Result<(), NtError> {
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
