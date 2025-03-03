use wdk_sys::{WDFOBJECT, call_unsafe_wdf_function_binding};
use super::{error::NtStatus, io_queue::IoQueue, object::FrameworkObjectType};
use crate::{FrameworkObject, Rc};

pub struct Request(Rc);

impl Request {
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
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self(unsafe { Rc::new(inner) })
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0.inner() as *mut _
    }

    fn object_type() -> FrameworkObjectType {
        FrameworkObjectType::Request
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for Request {}