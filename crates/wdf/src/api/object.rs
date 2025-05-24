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

                // Relaxed ordering is fine here since we do not care if operations
                // on other variables such as T (i.e. the data we are carrying)
                // get reordered with respect to fetch_add. 
                // After all it is totally okay to access T after the ref count has 
                // been incremented because the object is guaranteed to be alive
                // thanks to this very increment.
                // We also prevent the ref count from overflowing here by bugchecking
                // if it gets too high because an overflow would lead to all kinds of unsafety.
                if ref_count.fetch_add(1, core::sync::atomic::Ordering::Relaxed) > usize::MAX / 2 {
                    let ref_count = ref_count.load(core::sync::atomic::Ordering::Relaxed);
                    crate::api::object_context::bug_check(0xDEADDEAD, obj.as_raw(), Some(ref_count));
                }

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
            fn get_ref_count(&self) -> &core::sync::atomic::AtomicUsize {
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
                // We need to ensure here that:
                // 1. Access to T, the data we are carrying, is not reordered
                // AFTER the fetch_sub operation because that might lead to
                // a use-after-free in case the data has already been freed.
                // 2. The call to WdfObjectDelete is not reordered BEFORE
                // fetch_sub because it is wrong to delete T before the ref count
                // has dropped to zero.

                // We could have achieved both of those by using the AcqRel
                // ordering.  However, the call to WdfObjectDelete is made
                // only when the ref count drops to zero. Therefore we do
                // not need Acquire every time fetch_sub is called. It is needed
                // only when the ref count has become zero. Hence, here we use
                // only Release in fetch_sub and have a separate fench with
                // Acquire inside the if block.
                if ref_count.fetch_sub(1, core::sync::atomic::Ordering::Release) == 0 {
                    core::sync::atomic::fence(core::sync::atomic::Ordering::Acquire);
                    // SAFETY: The object is guarateed to be valid here
                    // because it is deleted only here and no place else
                    unsafe {
                        call_unsafe_wdf_function_binding!(WdfObjectDelete, self.as_raw());
                    }
                }
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
