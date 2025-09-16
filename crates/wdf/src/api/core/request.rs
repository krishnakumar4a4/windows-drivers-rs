use alloc::{string::String, vec::Vec};
use core::{ptr, slice};

use bitflags::bitflags;
use wdf_macros::object_context;
use wdk_sys::{
    call_unsafe_wdf_function_binding,
    IO_STATUS_BLOCK,
    WDFMEMORY,
    WDFOBJECT,
    WDFREQUEST,
    WDF_REQUEST_COMPLETION_PARAMS,
    WDF_REQUEST_TYPE,
};

use super::{
    enum_mapping,
    init_wdf_struct,
    io_queue::IoQueue,
    io_target::IoTarget,
    memory::{Memory, OwnedMemory},
    object::Handle,
    result::{NtResult, NtStatus, NtStatusError, StatusCodeExt},
    sync::SpinLock,
};
use crate::usb::UsbRequestCompletionParams;

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
                self.as_ptr().cast(),
                status.code()
            )
        };
    }

    pub fn complete_with_information(self, status: NtStatus, information: usize) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestCompleteWithInformation,
                self.as_ptr().cast(),
                status.code(),
                information as core::ffi::c_ulonglong
            )
        };
    }

    pub fn set_information(&self, information: usize) {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestSetInformation,
                self.as_ptr().cast(),
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
        if let Some(context) = RequestContext::try_get_mut(self) {
            context.evt_request_cancel = cancel_fn;
            Ok(())
        } else {
            RequestContext::attach(
                self,
                RequestContext {
                    evt_request_cancel: cancel_fn,
                    evt_request_completion_routine: None,
                },
            )
        }
    }

    pub fn get_io_queue(&self) -> &IoQueue {
        unsafe {
            let queue =
                call_unsafe_wdf_function_binding!(WdfRequestGetIoQueue, self.as_ptr().cast());
            &*queue.cast::<IoQueue>()
        }
    }

    pub fn retrieve_input_memory(&self) -> NtResult<&Memory> {
        let mut raw_memory: WDFMEMORY = ptr::null_mut();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveInputMemory,
                self.as_ptr().cast(),
                &mut raw_memory
            )
        }
        .map(|| unsafe { &*(raw_memory.cast::<Memory>()) })
    }

    pub fn retrieve_output_memory(&mut self) -> NtResult<&mut Memory> {
        let mut raw_memory: WDFMEMORY = ptr::null_mut();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveOutputMemory,
                self.as_ptr().cast(),
                &mut raw_memory
            )
        }
        .map(|| unsafe { &mut *(raw_memory.cast::<Memory>()) })
    }

    pub fn retrieve_input_buffer(&self, minimum_required_size: usize) -> NtResult<&[u8]> {
        let mut buffer_ptr: *mut core::ffi::c_void = ptr::null_mut();
        let mut buffer_size: usize = 0;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveInputBuffer,
                self.as_ptr().cast(),
                minimum_required_size,
                &mut buffer_ptr,
                &mut buffer_size
            )
            .and_then(|| Ok(slice::from_raw_parts(buffer_ptr.cast::<u8>(), buffer_size)))
        }
    }

    pub fn retrieve_output_buffer(&mut self, minimum_required_size: usize) -> NtResult<&mut [u8]> {
        let mut buffer_ptr: *mut core::ffi::c_void = ptr::null_mut();
        let mut buffer_size: usize = 0;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestRetrieveOutputBuffer,
                self.as_ptr().cast(),
                minimum_required_size,
                &mut buffer_ptr,
                &mut buffer_size
            )
            .and_then(|| {
                Ok(slice::from_raw_parts_mut(
                    buffer_ptr.cast::<u8>(),
                    buffer_size,
                ))
            })
        }
    }

    pub fn set_completion_routine(
        &mut self,
        completion_routine: fn(RequestCompletionToken, &IoTarget),
    ) {
        let context = RequestContext::get_mut(self);
        context.evt_request_completion_routine = Some(completion_routine);
    }

    pub fn send_asynchronously(self, io_target: &IoTarget) -> Result<SentRequest, Request> {
        let res = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestSend,
                self.as_ptr().cast(),
                io_target.as_ptr().cast(),
                ptr::null_mut(), // Null options means asynchronous send
            )
        };

        if res != 0 {
            Ok(SentRequest(self))
        } else {
            Err(self)
        }
    }

    pub fn forward_to_io_queue(&self, queue: &IoQueue) -> NtResult<()> {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestForwardToIoQueue,
                self.as_ptr().cast(),
                queue.as_ptr().cast()
            )
        }
        .ok()
    }

    pub fn get_completion_params<'a>(&'a self) -> RequestCompletionParams<'a> {
        let mut raw_params = init_wdf_struct!(WDF_REQUEST_COMPLETION_PARAMS);
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfRequestGetCompletionParams,
                self.as_ptr().cast(),
                &mut raw_params
            )
        };

        RequestCompletionParams::from(&raw_params)
    }

    pub fn get_status(&self) -> NtStatus {
        let status = unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestGetStatus, self.as_ptr().cast(),)
        };

        status.into()
    }

    pub fn stop_acknowledge_requeue(self) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestStopAcknowledge, self.as_ptr().cast(), 1);
        }
    }

    pub fn stop_acknowledge_no_requeue(request_id: RequestId) {
        let request_ptr = request_id.0 as WDFREQUEST;
        unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestStopAcknowledge, request_ptr, 0);
        }
    }

    pub fn cancel_sent_request(sent_request: SentRequest) -> bool {
        let request_ptr = sent_request.as_ptr() as WDFREQUEST;
        let res =
            unsafe { call_unsafe_wdf_function_binding!(WdfRequestCancelSentRequest, request_ptr) };

        res != 0
    }
}

impl Handle for Request {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0.cast()
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
    evt_request_completion_routine: Option<fn(RequestCompletionToken, &IoTarget)>,
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
                let context = $ctx_name::get_mut(self);
                context.memory.take()
            }

            pub(crate) fn $set_fn(&mut self, memory: OwnedMemory) -> NtResult<()> {
                match $ctx_name::try_get_mut(self) {
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
    let safe_request = unsafe { &Request::from_raw(request as _) };
    let context = RequestContext::get(safe_request);
    (context.evt_request_cancel)(unsafe { &RequestCancellationToken::new(request as _) });
}

pub extern "C" fn __evt_request_read_completion_routine(
    request: WDFREQUEST,
    target: WDFOBJECT,
    _params: *const WDF_REQUEST_COMPLETION_PARAMS,
    _context: WDFOBJECT,
) {
    let safe_req = unsafe { Request::from_raw(request as _) };
    let context = RequestContext::get(&safe_req);
    if let Some(callback) = context.evt_request_completion_routine {
        callback(unsafe { RequestCompletionToken::new(request) }, unsafe {
            &*(target.cast::<IoTarget>())
        });
    }
}

#[derive(Debug)]
pub struct RequestCompletionParams<'a> {
    pub request_type: RequestType,
    pub io_status: IoStatusBlock,
    pub parameters: RequestCompletionParamDetails<'a>,
}

impl<'a> From<&WDF_REQUEST_COMPLETION_PARAMS> for RequestCompletionParams<'a> {
    fn from(raw: &WDF_REQUEST_COMPLETION_PARAMS) -> Self {
        let request_type = RequestType::from(raw.Type);
        let io_status = IoStatusBlock::from(raw.IoStatus);

        let parameters = match request_type {
            RequestType::Write => {
                let write = unsafe { &raw.Parameters.Write };
                RequestCompletionParamDetails::Write {
                    buffer: unsafe { &*(write.Buffer.cast::<Memory>()) },
                    length: write.Length as usize,
                    offset: write.Offset as usize,
                }
            }
            RequestType::Read => {
                let read = unsafe { &raw.Parameters.Read };
                RequestCompletionParamDetails::Read {
                    buffer: unsafe { &*(read.Buffer.cast::<Memory>()) },
                    length: read.Length as usize,
                    offset: read.Offset as usize,
                }
            }
            RequestType::DeviceControl | RequestType::DeviceControlInternal => {
                let ioctl = unsafe { &raw.Parameters.Ioctl };
                RequestCompletionParamDetails::Ioctl {
                    io_control_code: ioctl.IoControlCode,
                    input_buffer: unsafe { &*(ioctl.Input.Buffer.cast::<Memory>()) },
                    input_offset: ioctl.Input.Offset as usize,
                    output_buffer: unsafe { &*(ioctl.Output.Buffer.cast::<Memory>()) },
                    output_offset: ioctl.Output.Offset as usize,
                    output_length: ioctl.Output.Length as usize,
                }
            }
            RequestType::Usb => RequestCompletionParamDetails::Usb {
                completion: UsbRequestCompletionParams::from(unsafe {
                    &*raw.Parameters.Usb.Completion
                }),
            },
            _ => unsafe {
                let others = &raw.Parameters.Others;
                RequestCompletionParamDetails::Others {
                    argument1: others.Argument1.Value as usize,
                    argument2: others.Argument2.Value as usize,
                    argument3: others.Argument3.Value as usize,
                    argument4: others.Argument4.Value as usize,
                }
            },
        };

        Self {
            request_type,
            io_status,
            parameters,
        }
    }
}

#[derive(Debug)]
pub struct IoStatusBlock {
    pub status: NtStatus,
    pub information: usize,
}

impl From<IO_STATUS_BLOCK> for IoStatusBlock {
    fn from(raw: IO_STATUS_BLOCK) -> Self {
        Self {
            status: NtStatus::from(unsafe { raw.__bindgen_anon_1.Status }),
            information: raw.Information as usize,
        }
    }
}

#[derive(Debug)]
pub enum RequestCompletionParamDetails<'a> {
    Write {
        buffer: &'a Memory,
        length: usize,
        offset: usize,
    },
    Read {
        buffer: &'a Memory,
        length: usize,
        offset: usize,
    },
    Ioctl {
        io_control_code: u32,
        input_buffer: &'a Memory,
        input_offset: usize,
        output_buffer: &'a Memory,
        output_offset: usize,
        output_length: usize,
    },
    Others {
        argument1: usize,
        argument2: usize,
        argument3: usize,
        argument4: usize,
    },
    Usb {
        completion: UsbRequestCompletionParams<'a>,
    },
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
            call_unsafe_wdf_function_binding!(WdfRequestUnmarkCancelable, self.as_ptr().cast())
        };

        self.0.complete(status);
    }

    pub fn complete_with_information(self, status: NtStatus, information: usize) {
        // Ignoring the return value for the same reason as in `complete`
        let _ = unsafe {
            call_unsafe_wdf_function_binding!(WdfRequestUnmarkCancelable, self.as_ptr().cast())
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

/// A request that has been sent to an I/O target.
pub struct SentRequest(Request);

impl SentRequest {
    pub fn id(&self) -> RequestId {
        self.0.id()
    }

    pub fn into_request(self, _token: RequestCompletionToken) -> Request {
        // _token is required only to ensure that
        // caller is calling this from either evt_request_cancel
        // or evt_request_completion_routine.
        self.0
    }
}

impl Handle for SentRequest {
    fn as_ptr(&self) -> WDFOBJECT {
        self.0.as_ptr()
    }

    fn type_name() -> String {
        String::from("SentRequest")
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct RequestCompletionToken(Request);

impl RequestCompletionToken {
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

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RequestId(usize);

enum_mapping! {
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

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct RequestStopActionFlags: u32 {
        const SUSPEND = 0x00000001;
        const PURGE = 0x00000002;
        const CANCELABLE = 0x1000_0000;
    }
}
