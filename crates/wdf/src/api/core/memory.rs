use wdk_sys::{call_unsafe_wdf_function_binding, NT_SUCCESS, WDFMEMORY_OFFSET};

use super::{
    error::{NtError, NtResult},
    object::{impl_handle, Handle},
};

impl_handle!(Memory);

impl Memory {
    pub fn get_buffer(&self) -> &[u8] {
        let (ptr, size) = self.get_buffer_raw_impl();
        unsafe { core::slice::from_raw_parts(ptr, size) }
    }

    pub fn get_buffer_mut(&mut self) -> &mut [u8] {
        let (ptr, size) = self.get_buffer_raw_impl();
        unsafe { core::slice::from_raw_parts_mut(ptr, size) }
    }

    pub fn buffer_len(&self) -> usize {
        let (_, size) = self.get_buffer_raw_impl();
        size
    }

    pub fn copy_from_buffer(&mut self, offset: usize, buffer: &[u8]) -> NtResult<()> {
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCopyFromBuffer,
                self.as_ptr() as *mut _,
                offset,
                buffer.as_ptr() as *mut _,
                buffer.len()
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    pub fn copy_to_buffer(&self, offset: usize, buffer: &mut [u8]) -> NtResult<()> {
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCopyToBuffer,
                self.as_ptr() as *mut _,
                offset,
                buffer.as_mut_ptr() as *mut _,
                buffer.len()
            )
        };

        if NT_SUCCESS(status) {
            Ok(())
        } else {
            Err(status.into())
        }
    }

    fn get_buffer_raw_impl(&self) -> (*mut u8, usize) {
        let mut buf_size = 0;
        let buffer = unsafe {
            call_unsafe_wdf_function_binding!(WdfMemoryGetBuffer, self.as_ptr() as *mut _, &mut buf_size)
        };
        (buffer as *mut _, buf_size)
    }
}

#[derive(Debug)]
pub struct MemoryWithOffset {
    memory: Memory,
    offset: MemoryOffset,
}

impl MemoryWithOffset {
    pub fn try_new(memory: Memory, offset: MemoryOffset) -> NtResult<Self> {
        if !Self::is_valid_offset(&memory, &offset) {
            return Err(NtError::from(-1));
        }

        Ok(Self { memory, offset })
    }

    #[inline(always)]
    pub fn memory(&self) -> &Memory {
        &self.memory
    }

    #[inline(always)]
    pub fn offset(&self) -> &MemoryOffset {
        &self.offset
    }

    #[inline(always)]
    pub fn into_memory(self) -> Memory {
        self.memory
    }

    fn is_valid_offset(memory: &Memory, offset: &MemoryOffset) -> bool {
        let buf_len = memory.buffer_len();
        let end = offset.buffer_offset.checked_add(offset.buffer_length);
        match end {
            Some(end) if end <= buf_len => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct MemoryOffset {
    pub buffer_offset: usize,
    pub buffer_length: usize,
}

impl Into<WDFMEMORY_OFFSET> for &MemoryOffset {
    fn into(self) -> WDFMEMORY_OFFSET {
        WDFMEMORY_OFFSET {
            BufferOffset: self.buffer_offset,
            BufferLength: self.buffer_length,
        }
    }
}