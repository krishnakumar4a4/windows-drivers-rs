use core::sync::atomic::AtomicUsize;
use crate::api::{
    device::Device,
    error::NtError,
    object::{impl_ref_counted_handle, wdf_struct_size, Handle},
    object_context::RefCount,
    request::Request,
};
use wdf_macros::primary_object_context;
use wdk_sys::{
    call_unsafe_wdf_function_binding, NT_SUCCESS, WDFOBJECT, WDFQUEUE, WDFREQUEST,
    WDF_IO_QUEUE_CONFIG, WDF_IO_QUEUE_DISPATCH_TYPE, _WDF_IO_QUEUE_DISPATCH_TYPE,
    WDF_OBJECT_ATTRIBUTES, WDF_NO_OBJECT_ATTRIBUTES
};

impl_ref_counted_handle!(
    IoQueue,
    WDFQUEUE,
    IoQueueContext
);

impl IoQueue {
    pub(crate) unsafe fn new(inner: WDFQUEUE) -> Self {
        Self::from_raw(inner as WDFOBJECT)
    }

    pub fn create(device: &Device, queue_config: &IoQueueConfig) -> Result<Self, NtError> {
        unsafe { Self::create_with_attributes(device, queue_config, WDF_NO_OBJECT_ATTRIBUTES) }
    }

    unsafe fn create_with_attributes(device: &Device, queue_config: &IoQueueConfig, attributes:*mut WDF_OBJECT_ATTRIBUTES) -> Result<Self, NtError> {
        let mut config = to_unsafe_config(&queue_config);
        let mut queue: WDFQUEUE = core::ptr::null_mut();

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoQueueCreate,
                device.as_raw() as *mut _,
                &mut config as *mut _,
                attributes,
                &mut queue,
            )
        };

        if NT_SUCCESS(status) {
            let ctxt = IoQueueContext {
                ref_count: AtomicUsize::new(0),
                evt_io_default: queue_config.evt_io_default,
                evt_io_read: queue_config.evt_io_read,
                evt_io_write: queue_config.evt_io_write,
                evt_io_device_control: queue_config.evt_io_device_control,
            };

            let mut queue = unsafe { IoQueue::new(queue) };

            IoQueueContext::attach(&mut queue, ctxt)?;

            Ok(queue)
        } else {
            Err(status.into())
        }
    }

    pub fn get_device(&self) -> Device {
        unsafe {
            let device =
                call_unsafe_wdf_function_binding!(WdfIoQueueGetDevice, self.as_raw() as *mut _);
            Device::from_raw(device as *mut _)
        }
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

#[derive(Copy, Clone, Debug)]
pub enum TriState {
    False = 0,
    True = 1,
    UseDefault = 2,
}

pub struct IoQueueConfig {
    pub dispatch_type: IoQueueDispatchType,
    pub power_managed: TriState,
    pub allow_zero_length_requests: bool,
    pub default_queue: bool,
    pub evt_io_default: Option<fn(&mut IoQueue, Request)>,
    pub evt_io_read: Option<fn(&mut IoQueue, Request, usize)>,
    pub evt_io_write: Option<fn(&mut IoQueue, Request, usize)>,
    pub evt_io_device_control: Option<fn(&mut IoQueue, Request, usize, usize, u32)>,
}

impl Default for IoQueueConfig {
    fn default() -> Self {
        Self {
            dispatch_type: IoQueueDispatchType::Sequential,
            power_managed: TriState::UseDefault,
            allow_zero_length_requests: false,
            default_queue: false,
            evt_io_default: None,
            evt_io_read: None,
            evt_io_write: None,
            evt_io_device_control: None,
        }
    }
}

fn to_unsafe_config(safe_config: &IoQueueConfig) -> WDF_IO_QUEUE_CONFIG {
    let mut config =
        unsafe { core::mem::MaybeUninit::<WDF_IO_QUEUE_CONFIG>::zeroed().assume_init() };

    let size = wdf_struct_size!(WDF_IO_QUEUE_CONFIG);

    config.Size = size as u32;
    config.PowerManaged = safe_config.power_managed as i32;
    config.DispatchType = safe_config.dispatch_type.into();
    config.AllowZeroLengthRequests = safe_config.allow_zero_length_requests as u8;
    config.DefaultQueue = safe_config.default_queue as u8;

    if safe_config.evt_io_default.is_some() {
        config.EvtIoDefault = Some(__evt_io_default);
    }

    if safe_config.evt_io_read.is_some() {
        config.EvtIoRead = Some(__evt_io_read);
    }

    if safe_config.evt_io_write.is_some() {
        config.EvtIoWrite = Some(__evt_io_write);
    }

    if safe_config.evt_io_device_control.is_some() {
        config.EvtIoDeviceControl = Some(__evt_io_device_control);
    }

    if let IoQueueDispatchType::Parallel {
        presented_requests_limit,
    } = safe_config.dispatch_type
    {
        config.Settings.Parallel.NumberOfPresentedRequests = match presented_requests_limit {
            Some(limit) => limit,
            None => u32::MAX,
        };
    }

    config
}

#[primary_object_context(IoQueue)]
struct IoQueueContext {
    ref_count: AtomicUsize,
    evt_io_default: Option<fn(&mut IoQueue, Request)>,
    evt_io_read: Option<fn(&mut IoQueue, Request, usize)>,
    evt_io_write: Option<fn(&mut IoQueue, Request, usize)>,
    evt_io_device_control: Option<fn(&mut IoQueue, Request, usize, usize, u32)>,
}

impl RefCount for IoQueueContext {
    fn get(&self) -> &AtomicUsize {
        &self.ref_count
    }
}

macro_rules! unsafe_request_handler {
    ($handler_name:ident $(, $arg_name:ident: $arg_type:ty)*) => {
        paste::paste! {
            pub extern "C" fn [<__ $handler_name>](queue: WDFQUEUE, request: WDFREQUEST $(, $arg_name: $arg_type)*) {
                let mut queue = unsafe { IoQueue::from_raw(queue as *mut _) };
                let request = unsafe { Request::from_raw(request as *mut _) };
                if let Some(handlers) = IoQueueContext::get(&queue) {
                    if let Some(handler) = handlers.$handler_name {
                        handler(&mut queue, request $(, $arg_name)*);
                        return;
                    }
                }

                request.complete(super::NtStatus::Error(NtError::from(1))); // TODO: pass proper error status e.g. unsupported request
            }
        }
    };
}

unsafe_request_handler!(evt_io_default);
unsafe_request_handler!(evt_io_read, length: usize);
unsafe_request_handler!(evt_io_write, length: usize);
unsafe_request_handler!(evt_io_device_control,  OutputBufferLength: usize, InputBufferLength: usize, IoControlCode: u32);
