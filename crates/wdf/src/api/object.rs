use core::sync::atomic::AtomicUsize;
use wdk_sys::{WDFOBJECT, _WDF_EXECUTION_LEVEL, _WDF_SYNCHRONIZATION_SCOPE, WDF_OBJECT_ATTRIBUTES};

pub trait Handle {
    fn as_ptr(&self) -> WDFOBJECT;
}

pub trait RefCountedHandle: Handle {
    fn get_ref_count(&self) -> &AtomicUsize;
}

macro_rules! impl_ref_counted_handle {
    ($obj:ident, $raw_ptr:ty, $primary_context:ty) => {
        #[derive(Debug)]
        #[repr(C)]
        pub struct $obj {
            _private: [u8; 0], // Prevents instantiation of the struct from driver code
        }

        impl crate::api::object::Handle for $obj {
            fn as_ptr(&self) -> WDFOBJECT {
                self as *const _ as WDFOBJECT
            }
        }

        impl crate::api::object::RefCountedHandle for $obj {
            fn get_ref_count(&self) -> &core::sync::atomic::AtomicUsize {
                let primary_context = <$primary_context>::get(self).expect("Failed to get primary context");
                &primary_context.ref_count
            }
        }
    }
}

macro_rules! wdf_struct_size {
    ($StructName:ty) => {{
        paste::paste! {
            if unsafe { wdk_sys::WdfClientVersionHigherThanFramework } != 0 {
                let index = wdk_sys::_WDFSTRUCTENUM::[<INDEX_ $StructName>] as u32;
                if index < unsafe { wdk_sys::WdfStructureCount } {
                    unsafe {wdk_sys::WdfStructures.add(index as usize) as u32 }
                } else {
                    usize::MAX as u32
                }
            } else {
                core::mem::size_of::<$StructName>() as u32
            }
        }
    }};
}

pub(crate) use impl_ref_counted_handle;
pub(crate) use wdf_struct_size;

pub fn init_attributes() -> WDF_OBJECT_ATTRIBUTES {
    let mut attributes = WDF_OBJECT_ATTRIBUTES::default();

    attributes.Size = wdf_struct_size!(WDF_OBJECT_ATTRIBUTES);
    attributes.ExecutionLevel = _WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent;
    attributes.SynchronizationScope = _WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent;

    attributes
}
