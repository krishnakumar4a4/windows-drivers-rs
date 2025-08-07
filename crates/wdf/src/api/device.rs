use core::{default::Default, sync::atomic::AtomicUsize};

use wdf_macros::internal_object_context;
use wdk_sys::{
    call_unsafe_wdf_function_binding,
    NTSTATUS,
    NT_SUCCESS,
    WDFDEVICE,
    WDFDEVICE_INIT,
    WDF_NO_HANDLE,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDF_PNPPOWER_EVENT_CALLBACKS,
};

use crate::api::{
    error::NtResult,
    guid::Guid,
    io_queue::IoQueue,
    object::{impl_ref_counted_handle, wdf_struct_size, Handle},
    string::{to_unicode_string, to_utf16_buf},
};

impl_ref_counted_handle!(Device, DeviceContext);

impl Device {
    pub fn create(
        device_init: &mut DeviceInit,
        pnp_power_callbacks: Option<PnpPowerEventCallbacks>,
    ) -> NtResult<&Self> {
        if let Some(ref pnp_power_callbacks) = pnp_power_callbacks {
            let mut pnp_power_callbacks = to_unsafe_pnp_power_callbacks(pnp_power_callbacks);

            unsafe {
                call_unsafe_wdf_function_binding!(
                    WdfDeviceInitSetPnpPowerEventCallbacks,
                    device_init.as_ptr_mut(),
                    &mut pnp_power_callbacks
                );
            }
        }

        let mut device: WDFDEVICE = WDF_NO_HANDLE.cast();
        let mut device_init_ptr: *mut WDFDEVICE_INIT = device_init.as_ptr_mut();

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDeviceCreate,
                &mut device_init_ptr,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut device,
            )
        };

        if NT_SUCCESS(status) {
            let device = unsafe { &*(device as *mut _) };
            DeviceContext::attach(
                device,
                DeviceContext {
                    ref_count: AtomicUsize::new(0),
                    pnp_power_callbacks,
                },
            )?;
            Ok(device)
        } else {
            Err(status.into())
        }
    }

    pub fn create_interface(
        &self,
        interaface_class_guid: &Guid,
        reference_string: Option<&str>,
    ) -> NtResult<()> {
        let ref_str_buf = reference_string.map(to_utf16_buf);
        let unicode_ref_str = ref_str_buf.map(|b| to_unicode_string(b.as_ref()));

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDeviceCreateDeviceInterface,
                self.as_ptr() as *mut _,
                interaface_class_guid.as_lpcguid(),
                unicode_ref_str.map_or(core::ptr::null(), |s| &s)
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    pub fn get_default_queue(&self) -> Option<&IoQueue> {
        let queue = unsafe {
            call_unsafe_wdf_function_binding!(WdfDeviceGetDefaultQueue, self.as_ptr() as *mut _,)
        };

        if !queue.is_null() {
            Some(unsafe { &*(queue as *mut IoQueue) })
        } else {
            None
        }
    }
}

pub struct DeviceInit(*mut WDFDEVICE_INIT);

impl DeviceInit {
    pub unsafe fn from(inner: *mut WDFDEVICE_INIT) -> Self {
        Self(inner)
    }

    pub fn as_ptr_mut(&self) -> *mut WDFDEVICE_INIT {
        self.0
    }
}

#[internal_object_context(Device)]
struct DeviceContext {
    ref_count: AtomicUsize,
    pnp_power_callbacks: Option<PnpPowerEventCallbacks>,
}

pub struct PnpPowerEventCallbacks {
    // PFN_WDF_DEVICE_D0_ENTRY                 EvtDeviceD0Entry;
    // PFN_WDF_DEVICE_D0_ENTRY_POST_INTERRUPTS_ENABLED EvtDeviceD0EntryPostInterruptsEnabled;
    // PFN_WDF_DEVICE_D0_EXIT                  EvtDeviceD0Exit;
    // PFN_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED EvtDeviceD0ExitPreInterruptsDisabled;
    // PFN_WDF_DEVICE_PREPARE_HARDWARE         EvtDevicePrepareHardware;
    // PFN_WDF_DEVICE_RELEASE_HARDWARE         EvtDeviceReleaseHardware;
    // PFN_WDF_DEVICE_SELF_MANAGED_IO_CLEANUP  EvtDeviceSelfManagedIoCleanup;
    // PFN_WDF_DEVICE_SELF_MANAGED_IO_FLUSH    EvtDeviceSelfManagedIoFlush;
    pub evt_device_self_managed_io_init: Option<fn(&Device) -> NtResult<()>>,
    pub evt_device_self_managed_io_suspend: Option<fn(&Device) -> NtResult<()>>,
    pub evt_device_self_managed_io_restart: Option<fn(&Device) -> NtResult<()>>,
    // PFN_WDF_DEVICE_SURPRISE_REMOVAL         EvtDeviceSurpriseRemoval;
    // PFN_WDF_DEVICE_QUERY_REMOVE             EvtDeviceQueryRemove;
    // PFN_WDF_DEVICE_QUERY_STOP               EvtDeviceQueryStop;
    // PFN_WDF_DEVICE_USAGE_NOTIFICATION       EvtDeviceUsageNotification;
    // PFN_WDF_DEVICE_RELATIONS_QUERY          EvtDeviceRelationsQuery;
    // PFN_WDF_DEVICE_USAGE_NOTIFICATION_EX    EvtDeviceUsageNotificationEx;
}

impl Default for PnpPowerEventCallbacks {
    fn default() -> Self {
        Self {
            evt_device_self_managed_io_init: None,
            evt_device_self_managed_io_suspend: None,
            evt_device_self_managed_io_restart: None,
        }
    }
}

fn to_unsafe_pnp_power_callbacks(
    pnp_power_callbacks: &PnpPowerEventCallbacks,
) -> WDF_PNPPOWER_EVENT_CALLBACKS {
    let mut unsafe_callbacks = WDF_PNPPOWER_EVENT_CALLBACKS::default();
    unsafe_callbacks.Size = wdf_struct_size!(WDF_PNPPOWER_EVENT_CALLBACKS);

    if pnp_power_callbacks
        .evt_device_self_managed_io_init
        .is_some()
    {
        unsafe_callbacks.EvtDeviceSelfManagedIoInit = Some(__evt_device_self_managed_io_init);
    }

    if pnp_power_callbacks
        .evt_device_self_managed_io_suspend
        .is_some()
    {
        unsafe_callbacks.EvtDeviceSelfManagedIoSuspend = Some(__evt_device_self_managed_io_suspend);
    }

    if pnp_power_callbacks
        .evt_device_self_managed_io_restart
        .is_some()
    {
        unsafe_callbacks.EvtDeviceSelfManagedIoRestart = Some(__evt_device_self_managed_io_restart);
    }

    unsafe_callbacks
}

macro_rules! unsafe_pnp_power_callback {
    ($callback_name:ident) => {
        paste::paste! {
            pub extern "C" fn [<__ $callback_name>](device: WDFDEVICE) -> NTSTATUS {
                let device = unsafe { &*(device as *mut Device) };
                if let Some(ctxt) = DeviceContext::get(&device) {
                    if let Some(callbacks) = &ctxt.pnp_power_callbacks {
                        if let Some(callback) = callbacks.$callback_name {
                            return match callback(device) {
                                Ok(_) => 0,
                                Err(err) => err.nt_status(),
                            };
                        }
                    }
                }

                panic!("User did not provide callback {} but we subscribed to it", stringify!($callback_name));
            }
        }
    };
}

unsafe_pnp_power_callback!(evt_device_self_managed_io_init);
unsafe_pnp_power_callback!(evt_device_self_managed_io_suspend);
unsafe_pnp_power_callback!(evt_device_self_managed_io_restart);
