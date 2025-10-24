use core::sync::atomic::AtomicUsize;

use wdf_macros::object_context_with_ref_count_check;
use wdk::nt_success;
use wdk_sys::{
    _WDF_IO_QUEUE_DISPATCH_TYPE,
    WDF_IO_QUEUE_CONFIG,
    WDF_IO_QUEUE_DISPATCH_TYPE,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDFDEVICE,
    WDFQUEUE,
    WDFREQUEST,
    call_unsafe_wdf_function_binding,
};

use super::{
    TriState,
    device::Device,
    init_wdf_struct,
    object::{GetDevice, Handle, impl_ref_counted_handle},
    request::{Request, RequestId, RequestStopActionFlags},
    result::{NtResult, NtStatusError, StatusCodeExt, status_codes},
    sync::Arc,
};

impl_ref_counted_handle!(IoQueue, IoQueueContext);

impl IoQueue {
    pub fn create(device: &Device, queue_config: &IoQueueConfig) -> NtResult<Arc<Self>> {
        let mut config: WDF_IO_QUEUE_CONFIG = queue_config.into();
        let mut queue: WDFQUEUE = core::ptr::null_mut();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoQueueCreate,
                device.as_ptr().cast(),
                &mut config,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut queue,
            )
        }
        .and_then(|| {
            let ctxt = IoQueueContext {
                ref_count: AtomicUsize::new(0),
                evt_io_default: queue_config.evt_io_default,
                evt_io_read: queue_config.evt_io_read,
                evt_io_write: queue_config.evt_io_write,
                evt_io_device_control: queue_config.evt_io_device_control,
                evt_io_stop: queue_config.evt_io_stop,
            };

            IoQueueContext::attach(unsafe { &*(queue.cast()) }, ctxt)?;

            let queue = unsafe { Arc::from_raw(queue.cast()) };

            Ok(queue)
        })
    }

    pub fn get_device(&self) -> &Device {
        self.get_device_safely()
    }

    pub fn start(&self) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfIoQueueStart, self.as_ptr().cast());
        }
    }

    pub fn stop_synchronously(&self) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfIoQueueStopSynchronously, self.as_ptr().cast());
        }
    }

    pub fn retrieve_next_request(&self) -> NtResult<Option<Request>> {
        let mut request: WDFREQUEST = core::ptr::null_mut();
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoQueueRetrieveNextRequest,
                self.as_ptr().cast(),
                &mut request
            )
        };

        if nt_success(status) || status == status_codes::STATUS_NO_MORE_ENTRIES {
            let request = if !request.is_null() {
                Some(unsafe { Request::from_raw(request) })
            } else {
                None
            };

            Ok(request)
        } else {
            Err(NtStatusError::from(status))
        }
    }
}

impl GetDevice for IoQueue {
    fn get_device_ptr(&self) -> WDFDEVICE {
        unsafe { call_unsafe_wdf_function_binding!(WdfIoQueueGetDevice, self.as_ptr().cast()) }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum IoQueueDispatchType {
    Sequential,
    Parallel {
        presented_requests_limit: Option<u32>,
    },
    Manual,
}

impl Into<WDF_IO_QUEUE_DISPATCH_TYPE> for IoQueueDispatchType {
    fn into(self) -> WDF_IO_QUEUE_DISPATCH_TYPE {
        match self {
            IoQueueDispatchType::Sequential => {
                _WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchSequential
            }
            IoQueueDispatchType::Parallel { .. } => {
                _WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchParallel
            }
            IoQueueDispatchType::Manual => _WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchManual,
        }
    }
}

pub struct IoQueueConfig {
    pub dispatch_type: IoQueueDispatchType,
    pub power_managed: TriState,
    pub allow_zero_length_requests: bool,
    pub default_queue: bool,
    pub evt_io_default: Option<fn(&IoQueue, Request)>,
    pub evt_io_read: Option<fn(&IoQueue, Request, usize)>,
    pub evt_io_write: Option<fn(&IoQueue, Request, usize)>,
    pub evt_io_device_control: Option<fn(&IoQueue, Request, usize, usize, u32)>,
    pub evt_io_stop: Option<fn(&IoQueue, RequestId, RequestStopActionFlags)>,
}

impl IoQueueConfig {
    /// Create a new `IoQueueConfig` with the specified dispatch type
    pub fn new(dispatch_type: IoQueueDispatchType) -> Self {
        Self {
            dispatch_type,
            power_managed: TriState::UseDefault,
            allow_zero_length_requests: false,
            default_queue: false,
            evt_io_default: None,
            evt_io_read: None,
            evt_io_write: None,
            evt_io_device_control: None,
            evt_io_stop: None,
        }
    }

    /// Create a new `IoQueueConfig` with the specified dispatch type
    /// and with `default_queue` set to `true`
    pub fn new_default(dispatch_type: IoQueueDispatchType) -> Self {
        let mut queue = Self::new(dispatch_type);
        queue.default_queue = true;
        queue
    }
}

impl From<&IoQueueConfig> for WDF_IO_QUEUE_CONFIG {
    fn from(config: &IoQueueConfig) -> Self {
        let mut raw_config = init_wdf_struct!(WDF_IO_QUEUE_CONFIG);
        raw_config.PowerManaged = config.power_managed as i32;
        raw_config.DispatchType = config.dispatch_type.into();
        raw_config.AllowZeroLengthRequests = config.allow_zero_length_requests as u8;
        raw_config.DefaultQueue = config.default_queue as u8;

        if config.evt_io_default.is_some() {
            raw_config.EvtIoDefault = Some(__evt_io_default);
        }

        if config.evt_io_read.is_some() {
            raw_config.EvtIoRead = Some(__evt_io_read);
        }

        if config.evt_io_write.is_some() {
            raw_config.EvtIoWrite = Some(__evt_io_write);
        }

        if config.evt_io_device_control.is_some() {
            raw_config.EvtIoDeviceControl = Some(__evt_io_device_control);
        }

        if config.evt_io_stop.is_some() {
            raw_config.EvtIoStop = Some(__evt_io_stop);
        }

        if let IoQueueDispatchType::Parallel {
            presented_requests_limit,
        } = config.dispatch_type
        {
            raw_config.Settings.Parallel.NumberOfPresentedRequests = match presented_requests_limit
            {
                Some(limit) => limit,
                None => u32::MAX,
            };
        }

        raw_config
    }
}

#[object_context_with_ref_count_check(IoQueue)]
struct IoQueueContext {
    ref_count: AtomicUsize,
    evt_io_default: Option<fn(&IoQueue, Request)>,
    evt_io_read: Option<fn(&IoQueue, Request, usize)>,
    evt_io_write: Option<fn(&IoQueue, Request, usize)>,
    evt_io_device_control: Option<fn(&IoQueue, Request, usize, usize, u32)>,
    evt_io_stop: Option<fn(&IoQueue, RequestId, RequestStopActionFlags)>,
}

macro_rules! unsafe_request_handler {
    // Default variant
    ($handler_name:ident $(, $arg_name:ident: $arg_type:ty $(=> $arg_transform:expr)? )* ) => {
        unsafe_request_handler!(@impl $handler_name, |r| r $(, $arg_name: $arg_type $(=> $arg_transform)? )* );
    };

    // Variant to allow transforming request
    ($handler_name:ident, $req_name:ident => $req_transform:expr $(, $arg_name:ident: $arg_type:ty $(=> $arg_transform:expr)? )* ) => {
        unsafe_request_handler!(@impl $handler_name, |request: Request| { let $req_name = request; $req_transform } $(, $arg_name: $arg_type $(=> $arg_transform)? )* );
    };

    // Internal implementation
    (@impl $handler_name:ident, $req_transform:expr $(, $arg_name:ident: $arg_type:ty $(=> $arg_transform:expr)? )* ) => {
        paste::paste! {
            pub extern "C" fn [<__ $handler_name>](queue: WDFQUEUE, request: WDFREQUEST $(, $arg_name: $arg_type )* ) {
                let queue = unsafe { &*queue.cast::<IoQueue>() };
                let request = unsafe { Request::from_raw(request as WDFREQUEST) };

                let handlers = IoQueueContext::get(&queue);
                if let Some(handler) = handlers.$handler_name {
                    handler(queue, ($req_transform)(request) $(, unsafe_request_handler!(@transform $arg_name $(=> $arg_transform)? ) )*);
                    return;
                }

                request.complete(status_codes::STATUS_ABANDONED.into());
            }
        }
    };

    // Helpers: if caller provided a conversion, use it; otherwise pass the original arg
    (@transform $arg_name:ident => $arg_transform:expr) => { $arg_transform };
    (@transform $arg_name:ident) => { $arg_name };
}

unsafe_request_handler!(evt_io_default);
unsafe_request_handler!(evt_io_read, length: usize);
unsafe_request_handler!(evt_io_write, length: usize);
unsafe_request_handler!(evt_io_device_control, OutputBufferLength: usize, InputBufferLength: usize, IoControlCode: u32);
unsafe_request_handler!(evt_io_stop, req => req.id(), ActionFlags: u32 => RequestStopActionFlags::from_bits_retain(ActionFlags));
