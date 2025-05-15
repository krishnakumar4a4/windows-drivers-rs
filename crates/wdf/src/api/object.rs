use wdk_sys::{WDFOBJECT, _WDF_EXECUTION_LEVEL, _WDF_SYNCHRONIZATION_SCOPE, WDF_OBJECT_ATTRIBUTES};

pub trait FrameworkHandle {
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self;
    fn as_ptr(&self) -> WDFOBJECT;
    fn object_type() -> FrameworkHandleType;
}

#[derive(PartialEq)]
pub enum FrameworkHandleType {
    Device,
    IoQueue,
    Request,
    Timer,
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

pub(crate) use wdf_struct_size;

pub fn init_attributes() -> WDF_OBJECT_ATTRIBUTES {
    let mut attributes = WDF_OBJECT_ATTRIBUTES::default();

    attributes.Size = wdf_struct_size!(WDF_OBJECT_ATTRIBUTES);
    attributes.ExecutionLevel = _WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent;
    attributes.SynchronizationScope = _WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent;

    attributes
}
