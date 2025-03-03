use wdk_sys::{WDFREQUEST, WDFOBJECT, call_unsafe_wdf_function_binding};
use super::{io_queue::IoQueue, error::NtStatus};
use crate::{WdfObject, WdfRc};

pub struct Request(WdfRc);

impl Request {
    pub unsafe fn new(request: WDFREQUEST) -> Self {
        Self(unsafe { WdfRc::new(request as WDFOBJECT) })
    }

    pub fn complete(&mut self, status: NtStatus) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestComplete, self.as_ptr() as *mut _, status.nt_status())
        };
    }

    pub fn get_io_queue(&self) -> IoQueue {
        unsafe  {
            let queue = call_unsafe_wdf_function_binding!(WdfRequestGetIoQueue, self.as_ptr() as *mut _);
            IoQueue::new(queue)
        }
    }
}

impl WdfObject for Request {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0.inner() as *mut _
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for Request {}