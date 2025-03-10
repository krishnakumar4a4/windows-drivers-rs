use crate::api::{object::{FrameworkObject, FrameworkObjectType, Rc, wdf_struct_size}, device::Device, error::NtError, request::Request};
use wdk_sys::{WDFQUEUE, WDFREQUEST, call_unsafe_wdf_function_binding, STATUS_SUCCESS, WDF_IO_QUEUE_CONFIG, _WDF_IO_QUEUE_DISPATCH_TYPE, WDF_IO_QUEUE_DISPATCH_TYPE, WDFOBJECT};
use wdf_macros::object_context;

pub struct IoQueue(Rc);

impl IoQueue {
    pub(crate) unsafe fn new(inner: WDFQUEUE) -> Self {
        Self(unsafe { Rc::new(inner as *mut _) })
    }

    pub fn as_ptr(&self) -> WDFOBJECT {
        self.0.inner() as *mut _
    }

    pub fn create(device: &Device, queue_config: &IoQueueConfig) -> Result<Self, NtError> {
        let mut config = to_unsafe_config(&queue_config);
        let mut queue: WDFQUEUE = core::ptr::null_mut();
        let status = unsafe {
            call_unsafe_wdf_function_binding!(WdfIoQueueCreate,
                device.as_ptr() as *mut _,
                &mut config as *mut _,
                wdk_sys::WDF_NO_OBJECT_ATTRIBUTES,
                &mut queue,
            )
        };

        if status == STATUS_SUCCESS {
            let handlers = RequestHandlers {
                evt_io_default: queue_config.evt_io_default,
                evt_io_read: queue_config.evt_io_read,
                evt_io_write: queue_config.evt_io_write,
                evt_io_device_control: queue_config.evt_io_device_control,
            };

            let mut queue = unsafe { IoQueue::new(queue) };

            RequestHandlers::attach(&mut queue, handlers)?;

            Ok(queue)
        } else {
            Err(status.into())
        }
    }

    pub fn get_device(&self) -> Device {
        unsafe {
            let device = call_unsafe_wdf_function_binding!(WdfIoQueueGetDevice, self.as_ptr() as *mut _);
            Device::from_ptr(device as *mut _)
        }
    }
}

impl FrameworkObject for IoQueue {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self(unsafe { Rc::new(inner) })
    }

    fn as_ptr(&self) -> WDFOBJECT {
        self.0.inner() as *mut _
    }

    fn object_type() -> FrameworkObjectType {
        FrameworkObjectType::IoQueue
    }
}

#[derive(Copy, Clone, Debug)]
pub enum IoQueueDispatchType {
    Sequential,
    Parallel { presented_requests_limit : Option<u32> },
    Manual
}

impl Into<WDF_IO_QUEUE_DISPATCH_TYPE> for IoQueueDispatchType {
    fn into(self) -> WDF_IO_QUEUE_DISPATCH_TYPE {
        match self {
            IoQueueDispatchType::Sequential => _WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchSequential,
            IoQueueDispatchType::Parallel { .. } => _WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchParallel,
            IoQueueDispatchType::Manual => _WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchManual
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TriState {
    False = 0,
    True = 1,
    UseDefault = 2
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
            evt_io_device_control: None
        }
    }
}

fn to_unsafe_config(safe_config: &IoQueueConfig) -> WDF_IO_QUEUE_CONFIG {
    let mut config = unsafe { core::mem::MaybeUninit::<WDF_IO_QUEUE_CONFIG>::zeroed().assume_init() };

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


    if let IoQueueDispatchType::Parallel { presented_requests_limit} = safe_config.dispatch_type {
        config.Settings.Parallel.NumberOfPresentedRequests = match presented_requests_limit {
            Some(limit) => limit,
            None => u32::MAX 
        };
    } 

    config
}

#[object_context(IoQueue)]
struct RequestHandlers {
    evt_io_default: Option<fn(&mut IoQueue, Request)>,
    evt_io_read: Option<fn(&mut IoQueue, Request, usize)>,
    evt_io_write: Option<fn(&mut IoQueue, Request, usize)>,
    evt_io_device_control: Option<fn(&mut IoQueue, Request, usize, usize, u32)>,
}


macro_rules! extern_request_handler {
    ($handler_name:ident $(, $arg_name:ident: $arg_type:ty)*) => {
        paste::paste! {
            pub extern "C" fn [<__ $handler_name>](queue: WDFQUEUE, request: WDFREQUEST $(, $arg_name: $arg_type)*) {
                let mut queue = unsafe { IoQueue::from_ptr(queue as *mut _) };
                let request = unsafe { Request::from_ptr(request as *mut _) };
                if let Some(handlers) = RequestHandlers::get(&queue) {
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

extern_request_handler!(evt_io_default);
extern_request_handler!(evt_io_read, length: usize);
extern_request_handler!(evt_io_write, length: usize);
extern_request_handler!(evt_io_device_control,  OutputBufferLength: usize, InputBufferLength: usize, IoControlCode: u32);