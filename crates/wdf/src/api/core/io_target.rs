use core::{ptr, sync::atomic::AtomicUsize};

use wdf_macros::object_context_with_ref_count_check;
use wdk_sys::{
    call_unsafe_wdf_function_binding,
    NT_SUCCESS,
    PWDFMEMORY_OFFSET,
    WDFIOTARGET,
    WDFMEMORY,
    WDFMEMORY_OFFSET,
    WDF_IO_TARGET_SENT_IO_ACTION,
    WDF_NO_OBJECT_ATTRIBUTES,
};

use super::{
    device::Device,
    enum_mapping,
    result::NtResult,
    memory::MemoryWithOffset,
    object::{impl_ref_counted_handle, Handle},
    request::Request,
    sync::Arc,
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

    // TODO: start and stop are not thread-safe. They
    // cannot be called concurrently with each other. Fix that!
    pub fn start(&self) -> NtResult<()> {
        let status =
            unsafe { call_unsafe_wdf_function_binding!(WdfIoTargetStart, self.as_ptr() as *mut _) };
        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    // TODO: start and stop are not thread-safe. They
    // cannot be called concurrently with each other. Fix that!
    pub fn stop(&self, action: IoTargetSentIoAction) {
        let action_val: WDF_IO_TARGET_SENT_IO_ACTION = action.into();
        unsafe {
            call_unsafe_wdf_function_binding!(WdfIoTargetStop, self.as_ptr() as *mut _, action_val)
        }
    }

    pub fn get_device(&self) -> &Device {
        unsafe {
            let device =
                call_unsafe_wdf_function_binding!(WdfIoTargetGetDevice, self.as_ptr() as *mut _,);
            &*(device as *mut Device)
        }
    }

    pub fn format_request_for_read(
        &self,
        request: &mut Request,
        output_buffer: Option<MemoryWithOffset>,
        device_offset: Option<i64>,
    ) -> NtResult<()> {
        let mut output_buffer_offset = WDFMEMORY_OFFSET::default();
        let (output_buffer_ptr, output_buffer_offset_ptr, device_offset_ptr) =
            Self::save_memory_and_get_ptrs(
                request,
                output_buffer,
                &mut output_buffer_offset,
                false,
                device_offset,
            )?;

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoTargetFormatRequestForRead,
                self.as_ptr() as *mut _,
                request.as_ptr() as *mut _,
                output_buffer_ptr as *mut _,
                output_buffer_offset_ptr,
                device_offset_ptr
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    pub fn format_request_for_write(
        &self,
        request: &mut Request,
        input_buffer: Option<MemoryWithOffset>,
        device_offset: Option<i64>,
    ) -> NtResult<()> {
        let mut output_buffer_offset = WDFMEMORY_OFFSET::default();
        let (input_buffer_ptr, input_buffer_offset_ptr, device_offset_ptr) =
            Self::save_memory_and_get_ptrs(
                request,
                input_buffer,
                &mut output_buffer_offset,
                true,
                device_offset,
            )?;

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoTargetFormatRequestForWrite,
                self.as_ptr() as *mut _,
                request.as_ptr() as *mut _,
                input_buffer_ptr as *mut _,
                input_buffer_offset_ptr,
                device_offset_ptr
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    fn save_memory_and_get_ptrs(
        request: &mut Request,
        memory: Option<MemoryWithOffset>,
        c_memory_offset: &mut WDFMEMORY_OFFSET,
        is_input_memory: bool,
        device_offset: Option<i64>,
    ) -> NtResult<(WDFMEMORY, PWDFMEMORY_OFFSET, *mut i64)> {
        let (memory_ptr, memory_offset_ptr) = match memory {
            Some(mem) => {
                let ptr = mem.memory().as_ptr();
                *c_memory_offset = mem.offset().into();

                // IMPORTANT: Save the buffer in the request
                // so that it stays alive while the request
                // is being processed
                let mem = mem.into_memory();
                if is_input_memory {
                    request.set_user_input_memory(mem)?;
                } else {
                    request.set_user_output_memory(mem)?;
                }

                (ptr as *mut _, c_memory_offset as *mut _)
            }
            None => (ptr::null_mut(), ptr::null_mut()),
        };

        let device_offset_ptr = device_offset
            .map(|mut offset| &mut offset as *mut _)
            .unwrap_or(ptr::null_mut());

        Ok((memory_ptr, memory_offset_ptr, device_offset_ptr))
    }
}

#[object_context_with_ref_count_check(IoTarget)]
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
