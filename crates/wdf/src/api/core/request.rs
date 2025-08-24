use alloc::string::String;

use wdf_macros::object_context;
use wdk_sys::{call_unsafe_wdf_function_binding, NT_SUCCESS, WDFMEMORY, WDFOBJECT, WDFREQUEST};

use super::{
    error::{NtError, NtResult, NtStatus},
    io_queue::IoQueue,
    memory::Memory,
    object::Handle,
};

#[derive(Debug)]
#[repr(transparent)]
pub struct Request(WDFREQUEST);

impl Request {
    pub(crate) unsafe fn from_raw(inner: WDFREQUEST) -> Self {
        Self(inner)
    }

    pub fn id(&self) -> usize {
        self.0 as usize
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

    pub fn complete_with_information(self, status: NtStatus, information: usize) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestCompleteWithInformation,
                self.as_ptr() as *mut _,
                status.nt_status(),
                information as core::ffi::c_ulonglong
            )
        };
    }

    pub fn set_information(&self, information: usize) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestSetInformation,
                self.as_ptr() as *mut _,
                information as core::ffi::c_ulonglong
            )
        };
    }

    pub fn mark_cancellable(
        mut self,
        cancel_fn: fn(&RequestCancellationToken),
    ) -> Result<CancellableMarkedRequest, (NtError, Request)> {
        // TODO: check for the race where another thread
        // could call this method method and thay might
        // attach the context before us.
        if let Err(e) = RequestContext::attach(
            &mut self,
            RequestContext {
                evt_request_cancel: cancel_fn,
            },
        ) {
            return Err((e, self));
        }

        // TODO: we are ignoring the status returned by this function
        // because we feel we are fine for all the reasons it can fail
        // However, this needs to be looked into carefully.
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestMarkCancelable,
                self.as_ptr() as *mut _,
                Some(__evt_request_cancel)
            );
        };

        Ok(CancellableMarkedRequest(self))
    }

    pub fn get_io_queue(&self) -> &IoQueue {
        unsafe {
            let queue =
                call_unsafe_wdf_function_binding!(WdfRequestGetIoQueue, self.as_ptr() as *mut _);
            &*queue.cast::<IoQueue>()
        }
    }

    pub fn retrieve_input_memory(&self) -> NtResult<&Memory> {
        let mut raw_memory: WDFMEMORY = core::ptr::null_mut();

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveInputMemory,
                self.as_ptr() as *mut _,
                &mut raw_memory
            )
        };

        if NT_SUCCESS(status) {
            Ok(unsafe { &*(raw_memory as *const Memory) })
        } else {
            Err(status.into())
        }
    }

    pub fn retrieve_output_memory(&mut self) -> NtResult<&mut Memory> {
        let mut raw_memory: WDFMEMORY = core::ptr::null_mut();

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveOutputMemory,
                self.as_ptr() as *mut _,
                &mut raw_memory
            )
        };

        if NT_SUCCESS(status) {
            Ok(unsafe { &mut *(raw_memory as *mut Memory) })
        } else {
            Err(status.into())
        }
    }
}

impl Handle for Request {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0 as *mut _
    }

    fn type_name() -> String {
        String::from("Request")
    }
}


/// Although `Request` carries a raw pointer type, `WDFREQUEST`,
/// it is still `Sync` because all the C methods on `WDFREQUEST`
/// are thread-safe and therefore all the `Request` methods which
/// call these C methods are also thread-safe
unsafe impl Sync for Request {}

/// Although `Request` carries a raw pointer type, `WDFREQUEST`,
/// it is still `Send` because all the C methods on `WDFREQUEST`
/// are thread-safe and therefore all the `Request` methods which
/// call these C methods are also thread-safe
unsafe impl Send for Request {}

#[object_context(Request)]
struct RequestContext {
    evt_request_cancel: fn(&RequestCancellationToken),
}

pub extern "C" fn __evt_request_cancel(request: WDFREQUEST) {
    if let Some(context) = RequestContext::get(unsafe { &Request::from_raw(request as _) }) {
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

    pub fn complete_with_information(self, status: NtStatus, information: usize) {
        // Ignoring the return value for the same reason as in `complete`
        let _ = unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestUnmarkCancelable, self.as_ptr() as *mut _)
        };

        self.0.complete_with_information(status, information);
    }
}

impl Handle for CancellableMarkedRequest {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0.as_ptr()
    }

    fn type_name() -> String {
        String::from("CancellableMarkedRequest")
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for CancellableMarkedRequest {}

pub struct RequestCancellationToken(Request);

impl RequestCancellationToken {
    unsafe fn new(inner: WDFREQUEST) -> Self {
        Self(unsafe { Request::from_raw(inner) })
    }

    pub fn RequestId(&self) -> usize {
        self.0.id()
    }

    pub fn get_io_queue(&self) -> &IoQueue {
        self.0.get_io_queue()
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for RequestCancellationToken {}
unsafe impl Sync for RequestCancellationToken {}
