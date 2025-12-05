use core::{ptr, sync::atomic::AtomicUsize};

use wdf_macros::object_context_with_ref_count_check;
use wdk_sys::{
    NT_SUCCESS,
    PWDFMEMORY_OFFSET,
    WDF_IO_TARGET_SENT_IO_ACTION,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDFDEVICE,
    WDFIOTARGET,
    WDFMEMORY,
    WDFMEMORY_OFFSET,
    call_unsafe_wdf_function_binding,
};

use super::{
    device::Device,
    enum_mapping,
    memory::{Memory, MemoryOffset, OwnedMemory},
    object::{GetDevice, Handle, impl_ref_counted_handle},
    request::Request,
    result::{NtResult, StatusCodeExt, status_codes},
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
                device.as_ptr().cast(),
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut io_target,
            )
        };

        if NT_SUCCESS(status) {
            let ctxt = IoTargetContext {
                ref_count: AtomicUsize::new(0),
            };

            IoTargetContext::attach(unsafe { &*(io_target.cast()) }, ctxt)?;

            let io_target = unsafe { Arc::from_raw(io_target.cast()) };

            Ok(io_target)
        } else {
            Err(status.into())
        }
    }

    // TODO: start and stop are not thread-safe. They
    // cannot be called concurrently with each other. Fix that!
    pub fn start(&self) -> NtResult<()> {
        unsafe { call_unsafe_wdf_function_binding!(WdfIoTargetStart, self.as_ptr().cast()) }.ok()
    }

    // TODO: start and stop are not thread-safe. They
    // cannot be called concurrently with each other. Fix that!
    pub fn stop(&self, action: IoTargetSentIoAction) {
        let action_val: WDF_IO_TARGET_SENT_IO_ACTION = action.into();
        unsafe {
            call_unsafe_wdf_function_binding!(WdfIoTargetStop, self.as_ptr().cast(), action_val)
        }
    }

    pub fn get_device(&self) -> &Device {
        self.get_device_safely()
    }

    pub fn format_request_for_read(
        &self,
        request: &mut Request,
        output_memory: RequestFormatMemory,
        device_offset: Option<i64>,
    ) -> NtResult<()> {
        let mut memory_offset = WDFMEMORY_OFFSET::default();
        let (memory_ptr, memory_offset_ptr) =
            to_memory_ptrs(request, output_memory, &mut memory_offset, false)?;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoTargetFormatRequestForRead,
                self.as_ptr().cast(),
                request.as_ptr().cast(),
                memory_ptr.cast(),
                memory_offset_ptr,
                to_device_offset_ptr(device_offset)
            )
        }
        .ok()
    }

    pub fn format_request_for_write(
        &self,
        request: &mut Request,
        input_memory: RequestFormatMemory,
        device_offset: Option<i64>,
    ) -> NtResult<()> {
        let mut memory_offset = WDFMEMORY_OFFSET::default();
        let (memory_ptr, memory_offset_ptr) =
            to_memory_ptrs(request, input_memory, &mut memory_offset, true)?;

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfIoTargetFormatRequestForWrite,
                self.as_ptr().cast(),
                request.as_ptr().cast(),
                memory_ptr.cast(),
                memory_offset_ptr,
                to_device_offset_ptr(device_offset)
            )
        }
        .ok()
    }
}

impl GetDevice for IoTarget {
    fn get_device_ptr(&self) -> WDFDEVICE {
        unsafe { call_unsafe_wdf_function_binding!(WdfIoTargetGetDevice, self.as_ptr().cast()) }
    }
}

pub(crate) fn to_memory_ptrs(
    request: &mut Request,
    memory: RequestFormatMemory,
    raw_memory_offset: &mut WDFMEMORY_OFFSET,
    is_input_memory: bool,
) -> NtResult<(WDFMEMORY, PWDFMEMORY_OFFSET)> {
    let (mem_ptr, buffer_len, offset) = match memory {
        RequestFormatMemory::None => (ptr::null_mut(), 0, None),
        RequestFormatMemory::RequestMemory(offset) => {
            let memory: &Memory = if is_input_memory {
                request.retrieve_input_memory()?
            } else {
                request.retrieve_output_memory()?
            };
            let (ptr, len) = get_memory_ptr_and_len(memory);

            (ptr, len, offset)
        }
        RequestFormatMemory::UserBuffer(memory, offset) => {
            let (ptr, len) = get_memory_ptr_and_len(&memory);

            // TODO: do we really have to save the buffer in the context?
            // Can't we just skip the drop of OwnedMemory and recover
            // it later when the buffer ptr is being given back to the driver?
            // This will eliminate the need to take &mut Request. We could just
            // take &Request instead.

            // IMPORTANT: Save the buffer in the request
            // so that it stays alive while the request
            // is being processed
            set_request_user_buffer(request, memory, is_input_memory)?;

            (ptr, len, offset)
        }
    };

    if !is_valid_offset(buffer_len, &offset) {
        return Err(status_codes::STATUS_INVALID_PARAMETER.into());
    }

    let raw_memory_offset_ptr = if let Some(ref offset) = offset {
        *raw_memory_offset = offset.into();
        raw_memory_offset as PWDFMEMORY_OFFSET
    } else {
        ptr::null_mut()
    };

    Ok((mem_ptr, raw_memory_offset_ptr))
}

fn to_device_offset_ptr(device_offset: Option<i64>) -> *mut i64 {
    device_offset
        .map(|mut offset| &raw mut offset)
        .unwrap_or(ptr::null_mut())
}

fn set_request_user_buffer(
    request: &mut Request,
    buffer: OwnedMemory,
    is_input_buffer: bool,
) -> NtResult<()> {
    if is_input_buffer {
        request.set_user_input_memory(buffer)?;
    } else {
        request.set_user_output_memory(buffer)?;
    }
    Ok(())
}

/// Gets the raw pointer corresponding to the given
/// `Memory` reference along with the length of its
/// underlying buffer
fn get_memory_ptr_and_len(memory: &Memory) -> (WDFMEMORY, usize) {
    let buffer = memory.get_buffer();
    (memory.as_ptr() as WDFMEMORY, buffer.len())
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

/// Specifies the memory used while formatting
/// a request
#[derive(Debug)]
pub enum RequestFormatMemory {
    /// Do not use any memory
    None,

    /// The memory associated with the request
    RequestMemory(Option<MemoryOffset>),

    /// An independent memory provided by user
    UserBuffer(OwnedMemory, Option<MemoryOffset>),
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
