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
    memory::{Memory, MemoryOffset},
    object::{impl_ref_counted_handle, Handle},
    request::Request,
    result::{NtResult, status_codes},
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
        output_buffer: RequestFormatBuffer,
        device_offset: Option<i64>,
    ) -> NtResult<()> {
        let mut buffer_offset = WDFMEMORY_OFFSET::default();
        let (buffer_ptr, buffer_offset_ptr, device_offset_ptr) =
            save_buffer_and_get_ptrs(
                request,
                output_buffer,
                &mut buffer_offset,
                false,
                device_offset,
            )?;


        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoTargetFormatRequestForRead,
                self.as_ptr() as *mut _,
                request.as_ptr() as *mut _,
                buffer_ptr as *mut _,
                buffer_offset_ptr,
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
        input_buffer: RequestFormatBuffer,
        device_offset: Option<i64>,
    ) -> NtResult<()> {
        let mut buffer_offset = WDFMEMORY_OFFSET::default();
        let (buffer_ptr, buffer_offset_ptr, device_offset_ptr) =
            save_buffer_and_get_ptrs(
                request,
                input_buffer,
                &mut buffer_offset,
                true,
                device_offset,
            )?;

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoTargetFormatRequestForWrite,
                self.as_ptr() as *mut _,
                request.as_ptr() as *mut _,
                buffer_ptr as *mut _,
                buffer_offset_ptr,
                device_offset_ptr
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }
}

fn save_buffer_and_get_ptrs(
    request: &mut Request,
    buffer: RequestFormatBuffer,
    c_buffer_offset: &mut WDFMEMORY_OFFSET,
    is_input_buffer: bool,
    device_offset: Option<i64>,
) -> NtResult<(WDFMEMORY, PWDFMEMORY_OFFSET, *mut i64)> {
    let (buffer_ptr, buffer_len, buffer_offset) = match buffer {
        RequestFormatBuffer::None => (ptr::null_mut(), 0, None),
        RequestFormatBuffer::RequestBuffer(offset) => {
            let (ptr, len) = get_request_buf_ptr_and_len(request, is_input_buffer)?;
            (ptr, len, offset)
        },
        RequestFormatBuffer::UserBuffer(mut mem, offset) => {
            let (ptr, len) = get_memory_buf_ptr_and_len(&mut mem);

            // IMPORTANT: Save the buffer in the request
            // so that it stays alive while the request
            // is being processed
            set_request_user_buffer(request, mem, is_input_buffer)?;

            (ptr, len, offset)
        }
    };

    if !is_valid_offset(buffer_len, &buffer_offset) {
        return Err(status_codes::STATUS_INVALID_PARAMETER.into());
    }

    let c_buffer_offset_ptr = if let Some(ref offset) = buffer_offset {
        *c_buffer_offset = offset.into();
        c_buffer_offset as PWDFMEMORY_OFFSET
    } else {
        ptr::null_mut()
    };

    let device_offset_ptr = device_offset
        .map(|mut offset| &mut offset as *mut _)
        .unwrap_or(ptr::null_mut());
    
    Ok((buffer_ptr as *mut _, c_buffer_offset_ptr, device_offset_ptr))
}

fn set_request_user_buffer(request: &mut Request, buffer: Memory, is_input_buffer: bool) -> NtResult<()> {
    if is_input_buffer {
        request.set_user_input_memory(buffer)?;
    } else {
        request.set_user_output_memory(buffer)?;
    }
    Ok(())
}

fn get_request_buf_ptr_and_len(request: &mut Request, is_input_buffer: bool) -> NtResult<(*mut u8, usize)> {
    let memory: &Memory = if is_input_buffer {
        request.retrieve_input_memory()?
    } else {
        request.retrieve_output_memory()?
    };

    Ok(get_memory_buf_ptr_and_len(memory))
}

fn get_memory_buf_ptr_and_len(memory: &Memory) -> (*mut u8, usize) {
    let buffer = memory.get_buffer();
    (buffer.as_ptr() as *mut _, buffer.len())
}

fn is_valid_offset(buffer_len: usize, offset: &Option<MemoryOffset>) -> bool {
    let Some(offset) = offset else {
        return true;
    };

    let offset_end = offset.buffer_offset.checked_add(offset.buffer_length);
    match offset_end {
        Some(offset_end) if offset_end <= buffer_len => true,
        _ => false,
    }
}

/// Specifies the buffer used while formatting
/// a request
pub enum RequestFormatBuffer {
    /// Do not use any buffer
    None,

    /// The buffer associated with the request
    RequestBuffer(Option<MemoryOffset>),

    /// An independent buffer provided by user
    UserBuffer(Memory, Option<MemoryOffset>),
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
