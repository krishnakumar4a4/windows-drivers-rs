use core::{
    ffi::c_void,
    ops::{Deref, DerefMut},
    ptr,
};

use wdk_sys::{
    call_unsafe_wdf_function_binding,
    MDL,
    WDFMEMORY,
    WDFMEMORY_OFFSET,
    WDF_MEMORY_DESCRIPTOR,
    _POOL_TYPE,
    _WDF_MEMORY_DESCRIPTOR_TYPE,
};

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
                self.as_ptr().cast(),
                offset,
                buffer.as_ptr().cast_mut().cast(),
                buffer.len()
            )
        }
        .ok()
    }

    pub fn copy_to_buffer(&self, offset: usize, buffer: &mut [u8]) -> NtResult<()> {
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfMemoryCopyToBuffer,
                self.as_ptr().cast(),
                offset,
                buffer.as_mut_ptr().cast(),
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
                self.as_ptr().cast(),
                &mut buf_size
            )
        };
        (buffer.cast(), buf_size)
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

        Ok(OwnedMemory(memory))
    }
}

impl Drop for OwnedMemory {
    fn drop(&mut self) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfObjectDelete, self.0.cast());
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
        unsafe { &*(self.0.cast::<Memory>()) }
    }
}

impl DerefMut for OwnedMemory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.0.cast::<Memory>()) }
    }
}

/// An type specifying different
/// kinds of memory buffers
pub enum MemoryDescriptor<'a> {
    Buffer(&'a [u8]),
    BufferMut(&'a mut [u8]),
    Mdl {
        mdl: &'a Mdl,
        buffer_length: u32, /* TODO: this can be arbitrary length. Ensure it is within buffer
                             * bounds */
    },
    MdlMut {
        mdl: &'a mut Mdl,
        buffer_length: u32, /* TODO: this can be arbitrary length. Ensure it is within buffer
                             * bounds */
    },
    Handle {
        memory: &'a Memory,
        offset: Option<&'a MemoryOffset>,
    },
    HandleMut {
        memory: &'a mut Memory,
        offset: Option<&'a MemoryOffset>,
    },
}

impl<'a> MemoryDescriptor<'a> {
    /// Returns true if the memory descriptor
    /// allows mutable access to its contents
    pub fn is_mut(&self) -> bool {
        matches!(
            self,
            MemoryDescriptor::BufferMut { .. }
                | MemoryDescriptor::MdlMut { .. }
                | MemoryDescriptor::HandleMut { .. }
        )
    }
}

impl<'a> Into<WDF_MEMORY_DESCRIPTOR> for &MemoryDescriptor<'a> {
    fn into(self) -> WDF_MEMORY_DESCRIPTOR {
        enum TempDescriptor<'a> {
            Buf(&'a [u8]),
            Mdl(&'a Mdl, u32),
            Handle(&'a Memory, Option<&'a MemoryOffset>),
        }

        // Convert to a temporary enum to avoid errors due to
        // type differences between mutable and immutable
        // references
        let temp_descriptor = match self {
            MemoryDescriptor::Buffer(buf) => TempDescriptor::Buf(buf),
            MemoryDescriptor::BufferMut(buf) => TempDescriptor::Buf(buf),
            MemoryDescriptor::Mdl { mdl, buffer_length } => {
                TempDescriptor::Mdl(mdl, *buffer_length)
            }
            MemoryDescriptor::MdlMut { mdl, buffer_length } => {
                TempDescriptor::Mdl(mdl, *buffer_length)
            }
            MemoryDescriptor::Handle { memory, offset } => TempDescriptor::Handle(memory, *offset),
            MemoryDescriptor::HandleMut { memory, offset } => {
                TempDescriptor::Handle(memory, *offset)
            }
        };

        let mut descriptor = WDF_MEMORY_DESCRIPTOR::default();

        match temp_descriptor {
            TempDescriptor::Buf(buf) => {
                descriptor.Type = _WDF_MEMORY_DESCRIPTOR_TYPE::WdfMemoryDescriptorTypeBuffer;
                descriptor.u.BufferType.Buffer = buf.as_ptr().cast::<c_void>().cast_mut();
                descriptor.u.BufferType.Length = buf.len() as u32;
            }
            TempDescriptor::Mdl(mdl, buffer_length) => {
                descriptor.Type = _WDF_MEMORY_DESCRIPTOR_TYPE::WdfMemoryDescriptorTypeMdl;
                descriptor.u.MdlType.Mdl = (mdl as *const Mdl).cast::<MDL>().cast_mut();
                descriptor.u.MdlType.BufferLength = buffer_length;
            }
            TempDescriptor::Handle(memory, offset) => {
                descriptor.Type = _WDF_MEMORY_DESCRIPTOR_TYPE::WdfMemoryDescriptorTypeHandle;
                descriptor.u.HandleType.Memory = memory.as_ptr().cast();
                descriptor.u.HandleType.Offsets = offset.map_or(ptr::null_mut(), |o| {
                    (o as *const MemoryOffset)
                        .cast::<WDFMEMORY_OFFSET>()
                        .cast_mut()
                });
            }
        };

        descriptor
    }
}

// TODO: define Mdl better
// Add the relevant fields like flags
// and maybe an iterator for Next
// plus buffer length to prevent
// buffer overflows
#[repr(C)]
pub struct Mdl {
    _private: [u8; 0], // Prevents instantiation of the struct from driver code
}

#[derive(Debug)]
#[repr(C)] // To allow casting to WDFMEMORY_OFFSET
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
