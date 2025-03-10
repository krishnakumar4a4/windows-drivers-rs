use wdk_sys::{WDFOBJECT, WDFREQUEST, call_unsafe_wdf_function_binding};
use super::{error::NtStatus, io_queue::IoQueue, object::FrameworkObjectType, NtResult};
use crate::{FrameworkObject, Rc};
use wdf_macros::object_context;

pub struct Request(Rc);

impl Request {
    pub fn Id(&self) -> usize {
        self.0.inner() as usize
    }

    pub fn complete(self, status: NtStatus) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestComplete, self.as_ptr() as *mut _, status.nt_status())
        };
    }

    pub fn mark_cancellable(mut self, cancel_fn: fn(RequestId)) -> NtResult<CancellableMarkedRequest> {
        // TODO: check for the race where another thread
        // could call this method method and thay might
        // attach the context before us.
        RequestContext::attach(&mut self, RequestContext { evt_request_cancel: cancel_fn })?;
        
        unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestMarkCancelable, self.as_ptr() as *mut _, Some(__evt_request_cancel));
        };

        Ok(CancellableMarkedRequest(self))
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


#[object_context(Request)]
struct RequestContext {
    evt_request_cancel: fn(RequestId),
}

pub extern "C" fn __evt_request_cancel(request: WDFREQUEST) {
    if let Some(context) = RequestContext::get(unsafe { &Request::from_ptr(request as _) }) {
        (context.evt_request_cancel)(RequestId(request as _));
    }
}


pub struct CancellableMarkedRequest(Request);

impl CancellableMarkedRequest {
    pub fn unmark_cancellable(self) -> NtResult<Request> {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestUnmarkCancelable, self.as_ptr() as *mut _);
        }

        Ok(self.0)
    }
}

impl FrameworkObject for CancellableMarkedRequest {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self(unsafe { Request::from_ptr(inner) })
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0.as_ptr()
    }

    fn object_type() -> FrameworkObjectType {
        FrameworkObjectType::Request
    }
}


/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for CancellableMarkedRequest {}

pub struct RequestId(WDFOBJECT);

impl RequestId {
    unsafe fn new(inner: WDFOBJECT) -> Self {
        Self(inner)
    }

    pub fn Id(&self) -> usize {
        self.0 as usize
    }
}


/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for RequestId {}
unsafe impl Sync for RequestId {}