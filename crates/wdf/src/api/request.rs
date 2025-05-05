use super::{error::NtStatus, io_queue::IoQueue, object::FrameworkHandleType, NtResult};
use crate::{FrameworkHandle, Rc};
use wdf_macros::object_context;
use wdk_sys::{call_unsafe_wdf_function_binding, WDFOBJECT, WDFREQUEST};

pub struct Request(Rc);

impl Request {
    pub fn id(&self) -> usize {
        self.0.inner() as usize
    }

    pub fn complete(self, status: NtStatus) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestComplete,
                self.as_ptr() as *mut _,
                status.nt_status()
            )
        };
    }

    pub fn mark_cancellable(
        mut self,
        cancel_fn: fn(&RequestCancellationToken),
    ) -> NtResult<CancellableMarkedRequest> {
        // TODO: check for the race where another thread
        // could call this method method and thay might
        // attach the context before us.
        RequestContext::attach(
            &mut self,
            RequestContext {
                evt_request_cancel: cancel_fn,
            },
        )?;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestMarkCancelable,
                self.as_ptr() as *mut _,
                Some(__evt_request_cancel)
            );
        };

        Ok(CancellableMarkedRequest(self))
    }

    pub fn get_io_queue(&self) -> IoQueue {
        unsafe {
            let queue =
                call_unsafe_wdf_function_binding!(WdfRequestGetIoQueue, self.as_ptr() as *mut _);
            IoQueue::new(queue)
        }
    }
}

impl FrameworkHandle for Request {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self(unsafe { Rc::new(inner) })
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0.inner()
    }

    fn object_type() -> FrameworkHandleType {
        FrameworkHandleType::Request
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for Request {}

#[object_context(Request)]
struct RequestContext {
    evt_request_cancel: fn(&RequestCancellationToken),
}

pub extern "C" fn __evt_request_cancel(request: WDFREQUEST) {
    if let Some(context) = RequestContext::get(unsafe { &Request::from_ptr(request as _) }) {
        (context.evt_request_cancel)(unsafe { &RequestCancellationToken::new(request as _) });
    }
}

pub struct CancellableMarkedRequest(Request);

impl CancellableMarkedRequest {
    pub fn complete(self, status: NtStatus) {
        // Ignoring the return value because the call to this method can
        // come from both the request cancellation, event where unmarking
        // is not required, and from other places, where unmarking is
        // required, and we know that it will fail in the former case.
        // At this point know where the call came from so we just ignore
        // the return value.
        // TODO: Redesign this and make sure we can handle genuine errors
        let _ = unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestUnmarkCancelable, self.as_ptr() as *mut _)
        };

        self.0.complete(status);
    }
}

impl FrameworkHandle for CancellableMarkedRequest {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self(unsafe { Request::from_ptr(inner) })
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0.as_ptr()
    }

    fn object_type() -> FrameworkHandleType {
        FrameworkHandleType::Request
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for CancellableMarkedRequest {}

pub struct RequestCancellationToken(Request);

impl RequestCancellationToken {
    unsafe fn new(inner: WDFOBJECT) -> Self {
        Self(unsafe { Request::from_ptr(inner) })
    }

    pub fn RequestId(&self) -> usize {
        self.0.id()
    }

    pub fn get_io_queue(&self) -> IoQueue {
        self.0.get_io_queue()
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for RequestCancellationToken {}
unsafe impl Sync for RequestCancellationToken {}
