use core::{ops::{Deref, DerefMut}, ptr};

use wdk_sys::{call_unsafe_wdf_function_binding, _POOL_TYPE, WDFMEMORY, WDFMEMORY_OFFSET};

use super::{
    object::{impl_handle, Handle},
    result::{NtResult, StatusCodeExt},
};

impl_handle! {
    /// A memory handle that only be accessed as a
    /// reference (`&Memory`) or a ref counted value
    /// (`Arc<Memory>`)
    ///
    /// `Memory` is the dual to `OwnedMemory` in the
    /// same way as `str` is to `String` and `Path`
    /// to `PathBuf`
    Memory
}

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
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCopyFromBuffer,
                self.as_ptr() as *mut _,
                offset,
                buffer.as_ptr() as *mut _,
                buffer.len()
            )
        }
        .ok()
    }

    pub fn copy_to_buffer(&self, offset: usize, buffer: &mut [u8]) -> NtResult<()> {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCopyToBuffer,
                self.as_ptr() as *mut _,
                offset,
                buffer.as_mut_ptr() as *mut _,
                buffer.len()
            )
        }
        .ok()
    }

    fn get_buffer_raw_impl(&self) -> (*mut u8, usize) {
        let mut buf_size = 0;
        let buffer = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryGetBuffer,
                self.as_ptr() as *mut _,
                &mut buf_size
            )
        };
        (buffer as *mut _, buf_size)
    }
}

/// A memory handle that is onwned
///
/// `OwnedMemory` is the dual to `Memory` in the
/// same way as `String` is to `str` and `Path`
/// to `PathBuf`
#[derive(Debug)]
#[repr(transparent)]
pub struct OwnedMemory(WDFMEMORY);

impl OwnedMemory {
    pub fn create(pool_type: PoolType, pool_tag: u32, buffer_size: usize) -> NtResult<Self> {
        let pool_type = match pool_type {
            PoolType::Paged => _POOL_TYPE::PagedPool,
            PoolType::NonPagedNx => _POOL_TYPE::NonPagedPoolNx,
        };

        let mut memory: WDFMEMORY = ptr::null_mut();
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCreate,
                ptr::null_mut(),
                pool_type,
                pool_tag,
                buffer_size,
                &mut memory,
                ptr::null_mut(),
            )
        }
        .ok()?;

        // TODO: wonder if we need to increment the WDF object ref count here
        // (and decrement it in drop) to keep the underlying object alive.
        // Investigate and confirm.

        Ok(OwnedMemory(memory))
    }
}

impl Drop for OwnedMemory {
    fn drop(&mut self) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfObjectDelete, self.0 as *mut _);
        }
    }
}

/// Although `OwnedMemory` carries a raw pointer it
/// is still `Sync` because sharing it across thread
/// will allow just readonly access to the pointer
/// as `&[u8]`
unsafe impl Sync for OwnedMemory {}

/// Although `OwnedMemory` carries a raw pointer it
/// is still `Send` because it uniquely owns the
/// pointer.
unsafe impl Send for OwnedMemory {}

impl Deref for OwnedMemory {
    type Target = Memory;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.0 as *const Memory) }
    }
}

impl DerefMut for OwnedMemory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.0 as *mut Memory) }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PoolType {
    Paged,
    NonPagedNx,
}