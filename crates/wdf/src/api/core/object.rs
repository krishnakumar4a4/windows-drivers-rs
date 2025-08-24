use alloc::string::String;
use core::sync::atomic::AtomicUsize;

use wdk_sys::{WDFOBJECT, WDF_OBJECT_ATTRIBUTES, _WDF_EXECUTION_LEVEL, _WDF_SYNCHRONIZATION_SCOPE};

use super::wdf_struct_size;

pub trait Handle {
    fn as_ptr(&self) -> WDFOBJECT;
    fn type_name() -> String;
}

pub trait RefCountedHandle: Handle {
    fn get_ref_count(&self) -> &AtomicUsize;
}

macro_rules! impl_handle {
    ($obj:ident) => {
        #[repr(C)]
        pub struct $obj {
            _private: [u8; 0], // Prevents instantiation of the struct from driver code
        }

        impl crate::api::object::Handle for $obj {
            fn as_ptr(&self) -> wdk_sys::WDFOBJECT {
                self as *const _ as wdk_sys::WDFOBJECT
            }

            fn type_name() -> alloc::string::String {
                let name = paste::paste! {
                    stringify!($obj)
                };

                alloc::string::String::from(name)
            }
        }

        impl core::fmt::Debug for $obj {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{}({:#x})", stringify!($obj), self as *const _ as usize)
            }
        }
    };
}

macro_rules! impl_ref_counted_handle {
    ($obj:ident, $inner_context:ty) => {
        crate::api::object::impl_handle!($obj);

        impl crate::api::object::RefCountedHandle for $obj {
            fn get_ref_count(&self) -> &core::sync::atomic::AtomicUsize {
                let inner_context =
                    <$inner_context>::get(self).expect("Failed to get inner context");
                &inner_context.ref_count
            }
        }
    };
}

pub(crate) use impl_handle;
pub(crate) use impl_ref_counted_handle;

pub fn init_attributes() -> WDF_OBJECT_ATTRIBUTES {
    let mut attributes = WDF_OBJECT_ATTRIBUTES::default();

    attributes.Size = wdf_struct_size!(WDF_OBJECT_ATTRIBUTES);
    attributes.ExecutionLevel = _WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent;
    attributes.SynchronizationScope =
        _WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent;

    attributes
}
