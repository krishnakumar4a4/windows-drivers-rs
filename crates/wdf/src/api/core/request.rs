use alloc::{string::String, vec::Vec};

use wdf_macros::object_context;
use wdk_sys::{call_unsafe_wdf_function_binding, WDFMEMORY, WDFOBJECT, WDFREQUEST, WDF_REQUEST_TYPE};

use super::{
    enum_mapping,
    io_queue::IoQueue,
    memory::{Memory, OwnedMemory},
    object::Handle,
    result::{NtResult, NtStatus, NtStatusError, StatusCodeExt},
    sync::SpinLock,
};

#[derive(Debug)]
#[repr(transparent)]
pub struct Request(WDFREQUEST);

// Removed the generic trait and its functions; the macro below generates
// per-buffer context structs and the Request methods, so the trait is not
// needed. (Remove the entire `trait UserMemoryContextLike { ... }` block here)

impl Request {
    pub(crate) unsafe fn from_raw(inner: WDFREQUEST) -> Self {
        Self(inner)
    }

    pub fn id(&self) -> RequestId {
        RequestId(self.0 as usize)
    }

    pub fn complete(self, status: NtStatus) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestComplete,
                self.as_ptr() as *mut _,
                status.code()
            )
        };
    }

    pub fn complete_with_information(self, status: NtStatus, information: usize) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestCompleteWithInformation,
                self.as_ptr() as *mut _,
                status.code(),
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

    pub fn mark_cancellable<S: CancellableRequestStore>(
        mut self,
        cancel_fn: fn(&RequestCancellationToken),
        cancellable_request_store: &SpinLock<S>,
    ) -> Result<(), (NtStatusError, Request)> {
        if let Err(e) = self.set_cancel_callback_in_context(cancel_fn) {
            return Err((e, self));
        }

        let req_ptr = self.as_ptr() as WDFREQUEST;

        // Save cancellable request in store before
        // we set the cancel callback to avoid
        // race condition
        let request_id = self.id();
        let mut store = cancellable_request_store.lock();
        store.add(CancellableRequest(self));

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestMarkCancelableEx,
                req_ptr,
                Some(__evt_request_cancel)
            )
        };

        if !status.is_success() {
            // Remove cancellable request from store
            // since we failed to set cancellable
            let _ = store
                .take(request_id)
                .expect("CancellableRequest not found although it was just added");
            Err((NtStatusError::from(status), unsafe {
                Request::from_raw(req_ptr)
            }))
        } else {
            Ok(())
        }
    }

    fn set_cancel_callback_in_context(
        &mut self,
        cancel_fn: fn(&RequestCancellationToken),
    ) -> NtResult<()> {
        if let Some(context) = RequestContext::get_mut(self) {
            context.evt_request_cancel = cancel_fn;
            Ok(())
        } else {
            RequestContext::attach(
                self,
                RequestContext {
                    evt_request_cancel: cancel_fn,
                },
            )
        }
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

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveInputMemory,
                self.as_ptr() as *mut _,
                &mut raw_memory
            )
        }
        .map(|| unsafe { &*(raw_memory as *const Memory) })
    }

    pub fn retrieve_output_memory(&mut self) -> NtResult<&mut Memory> {
        let mut raw_memory: WDFMEMORY = core::ptr::null_mut();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveOutputMemory,
                self.as_ptr() as *mut _,
                &mut raw_memory
            )
        }
        .map(|| unsafe { &mut *(raw_memory as *mut Memory) })
    }

    pub fn stop_acknowledge_requeue(self) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestStopAcknowledge,
                self.as_ptr() as *mut _,
                1
            );
        }
    }

    pub fn stop_acknowledge_no_requeue(&self) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestStopAcknowledge,
                self.as_ptr() as *mut _,
                0
            );
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
/// it is still `Send` because it uniquely owns the `WDFREQUEST`
/// pointer
unsafe impl Send for Request {}

pub trait CancellableRequestStore {
    fn add(&mut self, request: CancellableRequest);
    fn take(&mut self, id: RequestId) -> Option<CancellableRequest>;
}

impl CancellableRequestStore for Option<CancellableRequest> {
    fn add(&mut self, request: CancellableRequest) {
        *self = Some(request);
    }

    fn take(&mut self, id: RequestId) -> Option<CancellableRequest> {
        if let Some(request) = self.take() {
            if request.id() == id {
                return Some(request);
            } else {
                *self = Some(request);
            }
        }
        None
    }
}

impl CancellableRequestStore for Vec<CancellableRequest> {
    fn add(&mut self, request: CancellableRequest) {
        self.push(request);
    }

    fn take(&mut self, id: RequestId) -> Option<CancellableRequest> {
        if let Some(position) = self.iter().position(|r| r.id() == id) {
            Some(self.remove(position))
        } else {
            None
        }
    }
}

#[object_context(Request)]
struct RequestContext {
    evt_request_cancel: fn(&RequestCancellationToken),
}

/// Macro that defines input and output memory contexts
/// and methods to retrieve and set them
macro_rules! define_user_memory_context {
    (input) => {
        define_user_memory_context!(@impl UserInputMemoryContext, retrieve_user_input_memory, set_user_input_memory);
    };
    (output) => {
        define_user_memory_context!(@impl UserOutputMemoryContext, retrieve_user_output_memory, set_user_output_memory);
    };

    // helper: contains the common implementation for any per-request-memory context
    (@impl $ctx_name:ident, $retrieve_fn:ident, $set_fn:ident) => {
        #[object_context(Request)]
        struct $ctx_name {
            memory: Option<OwnedMemory>,
        }

        impl Request {
            pub(crate) fn $retrieve_fn(&mut self) -> Option<OwnedMemory> {
                let context = $ctx_name::get_mut(self)?;
                context.memory.take()
            }

            pub(crate) fn $set_fn(&mut self, memory: OwnedMemory) -> NtResult<()> {
                match $ctx_name::get_mut(self) {
                    Some(context) => {
                        context.memory = Some(memory);
                        Ok(())
                    }
                    None => $ctx_name::attach(self, $ctx_name { memory: Some(memory) }),
                }
            }
        }
    };
}

define_user_memory_context!(input);
define_user_memory_context!(output);

pub extern "C" fn __evt_request_cancel(request: WDFREQUEST) {
    if let Some(context) = RequestContext::get(unsafe { &Request::from_raw(request as _) }) {
        (context.evt_request_cancel)(unsafe { &RequestCancellationToken::new(request as _) });
    }
}

pub struct CancellableRequest(Request);

impl CancellableRequest {
    pub fn id(&self) -> RequestId {
        self.0.id()
    }

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

impl Handle for CancellableRequest {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0.as_ptr()
    }

    fn type_name() -> String {
        String::from("CancellableRequest")
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFREQUEST do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for CancellableRequest {}

pub struct RequestCancellationToken(Request);

impl RequestCancellationToken {
    unsafe fn new(inner: WDFREQUEST) -> Self {
        Self(unsafe { Request::from_raw(inner) })
    }

    pub fn request_id(&self) -> RequestId {
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

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RequestId(usize);

enum_mapping!{
    infallible;
    pub enum RequestType: WDF_REQUEST_TYPE {
        Create = WdfRequestTypeCreate,
        CreateNamedPipe = WdfRequestTypeCreateNamedPipe,
        Close = WdfRequestTypeClose,
        Read = WdfRequestTypeRead,
        Write = WdfRequestTypeWrite,
        QueryInformation = WdfRequestTypeQueryInformation,
        SetInformation = WdfRequestTypeSetInformation,
        QueryEA = WdfRequestTypeQueryEA,
        SetEA = WdfRequestTypeSetEA,
        FlushBuffers = WdfRequestTypeFlushBuffers,
        QueryVolumeInformation = WdfRequestTypeQueryVolumeInformation,
        SetVolumeInformation = WdfRequestTypeSetVolumeInformation,
        DirectoryControl = WdfRequestTypeDirectoryControl,
        FileSystemControl = WdfRequestTypeFileSystemControl,
        DeviceControl = WdfRequestTypeDeviceControl,
        DeviceControlInternal = WdfRequestTypeDeviceControlInternal,
        Shutdown = WdfRequestTypeShutdown,
        LockControl = WdfRequestTypeLockControl,
        Cleanup = WdfRequestTypeCleanup,
        CreateMailSlot = WdfRequestTypeCreateMailSlot,
        QuerySecurity = WdfRequestTypeQuerySecurity,
        SetSecurity = WdfRequestTypeSetSecurity,
        Power = WdfRequestTypePower,
        SystemControl = WdfRequestTypeSystemControl,
        DeviceChange = WdfRequestTypeDeviceChange,
        QueryQuota = WdfRequestTypeQueryQuota,
        SetQuota = WdfRequestTypeSetQuota,
        Pnp = WdfRequestTypePnp,
        Other = WdfRequestTypeOther,
        Usb = WdfRequestTypeUsb,
        NoFormat = WdfRequestTypeNoFormat,
        Max = WdfRequestTypeMax
    }
}
