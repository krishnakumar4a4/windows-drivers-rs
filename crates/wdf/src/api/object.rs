use core::sync::atomic::AtomicUsize;
use wdk_sys::{WDFOBJECT, _WDF_EXECUTION_LEVEL, _WDF_SYNCHRONIZATION_SCOPE, WDF_OBJECT_ATTRIBUTES};

pub trait Handle {
    unsafe fn from_raw(inner: WDFOBJECT) -> Self;
    fn as_raw(&self) -> WDFOBJECT;
    fn handle_type() -> HandleType;
}

pub(crate) trait RefCountedHandle: Handle + Clone {
    fn get_ref_count(&self) -> &AtomicUsize;
}

#[derive(PartialEq)]
pub enum HandleType {
    Device,
    IoQueue,
    Request,
    Timer,
}

macro_rules! impl_ref_counted_handle {
    ($obj:ident, $raw_ptr:ty, $primary_context:ty) => {
        #[derive(Debug)]
        #[repr(transparent)]
        pub struct $obj(pub $raw_ptr);

        impl crate::api::object::Handle for $obj {
            unsafe fn from_raw(inner: WDFOBJECT) -> Self {
                let obj = Self(inner as $raw_ptr);
                let ref_count = <Self as crate::api::object::RefCountedHandle>::get_ref_count(&obj);
                ref_count.fetch_add(1, core::sync::atomic::Ordering::Release);

                obj
            }

            fn as_raw(&self) -> WDFOBJECT {
                self.0 as WDFOBJECT
            }

            fn handle_type() -> crate::api::object::HandleType {
                crate::api::object::HandleType::$obj
            }
        }

        impl crate::api::object::RefCountedHandle for $obj {
            fn get_ref_count(&self) -> &AtomicUsize {
                let primary_context = <$primary_context>::get(self).expect("Failed to get primary context");
                &primary_context.ref_count
            }
        }

        impl Clone for $obj {
            fn clone(&self) -> Self {
                unsafe { Self::from_raw(self.0 as WDFOBJECT) }
            }
        }

        impl Drop for $obj {
            fn drop(&mut self) {
                let ref_count = <Self as crate::api::object::RefCountedHandle>::get_ref_count(self);
                ref_count.fetch_sub(1, core::sync::atomic::Ordering::Release);
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
