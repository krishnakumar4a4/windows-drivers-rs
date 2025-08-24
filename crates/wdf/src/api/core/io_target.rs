use core::sync::atomic::AtomicUsize;
use wdf_macros::internal_object_context;
use wdk_sys::{
    call_unsafe_wdf_function_binding,
    NT_SUCCESS,
    WDFIOTARGET,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDF_IO_TARGET_SENT_IO_ACTION,
};

use super::{
    device::Device,
    error::NtResult,
    object::{impl_ref_counted_handle, Handle},
    sync::Arc,
    enum_mapping,
};

impl_ref_counted_handle!(IoTarget, IoTargetContext);

impl IoTarget {
    /// Create an `IoTarget` 
    pub fn create(device: &Device) -> NtResult<Arc<Self>> {
        let mut io_target: WDFIOTARGET = core::ptr::null_mut();
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoTargetCreate,
                device.as_ptr() as *mut _,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut io_target,
            )
        };

        if NT_SUCCESS(status) {
            let ctxt = IoTargetContext {
                ref_count: AtomicUsize::new(0),
            };

            IoTargetContext::attach(unsafe { &*(io_target as *mut _) }, ctxt)?;

            let io_target = unsafe { Arc::from_raw(io_target as *mut _) };

            Ok(io_target)
        } else {
            Err(status.into())
        }
    }

    // pub fn open(&self) -> NtResult<()> {
    //     let status = unsafe { call_unsafe_wdf_function_binding!(WdfIoTargetOpen, self.as_ptr() as *mut _,) };
    //     if NT_SUCCESS(status) {
    //         Ok(())
    //     } else {
    //         Err(status.into())
    //     }
    // }

    // pub fn close(&self) -> NtResult<()> {
    //     let status = unsafe { call_unsafe_wdf_function_binding!(WdfIoTargetClose, self.as_ptr() as *mut _,) };
    //     if NT_SUCCESS(status) {
    //         Ok(())
    //     } else {
    //         Err(status.into())
    //     }
    // }

    // TODO: start and stop are not thread-safe. They
    // cannot be called concurrently with each other. Fix that!
    pub fn start(&self) -> NtResult<()> {
        let status = unsafe { call_unsafe_wdf_function_binding!(WdfIoTargetStart, self.as_ptr() as *mut _) };
        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    // TODO: start and stop are not thread-safe. They
    // cannot be called concurrently with each other. Fix that!
    pub fn stop(&self, action: IoTargetSentIoAction) {
        let action_val: wdk_sys::WDF_IO_TARGET_SENT_IO_ACTION = action.into();
        unsafe { call_unsafe_wdf_function_binding!(WdfIoTargetStop, self.as_ptr() as *mut _, action_val) }
    }

    pub fn get_device(&self) -> &Device {
        unsafe {
            let device = call_unsafe_wdf_function_binding!(WdfIoTargetGetDevice, self.as_ptr() as *mut _,);
            &*(device as *mut Device)
        }
    }
}

#[internal_object_context(IoTarget)]
struct IoTargetContext {
    ref_count: AtomicUsize,
}

enum_mapping! {
    pub enum IoTargetSentIoAction: WDF_IO_TARGET_SENT_IO_ACTION {
        CancelSentIo = WdfIoTargetCancelSentIo,
        WaitForSentIoToComplete = WdfIoTargetWaitForSentIoToComplete,
        LeaveSentIoPending = WdfIoTargetLeaveSentIoPending,
    }
}