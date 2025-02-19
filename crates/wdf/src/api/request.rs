use wdk_sys::{WDFREQUEST, WDFOBJECT, call_unsafe_wdf_function_binding};

use crate::{OwnedRef, WdfObject};

pub struct Request(WDFREQUEST);

impl Request {
    pub fn new(request: WDFREQUEST) -> Self {
        Self(request)
    }

    pub fn complete(&mut self, _completion_status: RequestCompletionStatus) -> Result<(), RequestCompletionError>  {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestComplete, self.as_ptr() as *mut _, 0);
        }

        Ok(())
    }

    // pub fn complete(self: OwnedRef<Request>, _completion_status: RequestCompletionStatus) {
    //     unsafe {
    //         call_unsafe_wdf_function_binding!(WdfRequestComplete, self.as_ptr() as WDFREQUEST, 0);
    //     }
    // }
}

impl WdfObject for Request {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self::new(inner as *mut _)
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0 as *mut _
    }
}

pub enum RequestCompletionError {
    AlreadyCompleted,
    CancelUnmarkNotCalled
}

pub enum RequestCompletionStatus {
    Success,
    Canceled,
}
