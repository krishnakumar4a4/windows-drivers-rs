use wdk_sys::{WDFREQUEST, WDFOBJECT, call_unsafe_wdf_function_binding};
use super::{io_queue::IoQueue, error::NtStatus};
use crate::{FrameworkObject, Rc};

pub struct Request(Rc);

impl Request {
    pub(crate) unsafe fn new(request: WDFREQUEST) -> Self {
        Self(unsafe { Rc::new(request as WDFOBJECT) })
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

impl FrameworkObject for Request {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0.inner() as *mut _
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for Request {}