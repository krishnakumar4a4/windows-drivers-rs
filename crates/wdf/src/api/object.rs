use wdk_sys::{call_unsafe_wdf_function_binding, WDFOBJECT, _WDF_EXECUTION_LEVEL, _WDF_SYNCHRONIZATION_SCOPE, WDF_OBJECT_ATTRIBUTES};

macro_rules! call_ref_func {
    ($func:ident, $obj:expr) => {
        unsafe {
            let file = file!();
            let file = if file.is_ascii() { file } else { "UTF8_name" };
            let file = file.as_bytes() as *const _ as *const i8;
            call_unsafe_wdf_function_binding!(
                $func,
                $obj,
                core::ptr::null_mut(),
                line!() as i32,
                file
            );
        }
    };
}

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

pub struct Rc(WDFOBJECT);

impl Rc {
    pub unsafe fn new(wdf_obj: WDFOBJECT) -> Self {
        call_ref_func!(WdfObjectReferenceActual, wdf_obj);
        Self(wdf_obj)
    }

    pub fn inner(&self) -> WDFOBJECT {
        self.0
    }
}

impl Drop for Rc {
    fn drop(&mut self) {
        call_ref_func!(WdfObjectDereferenceActual, self.0);
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

pub(crate) use wdf_struct_size;

pub fn init_attributes() -> WDF_OBJECT_ATTRIBUTES {
    let mut attributes = WDF_OBJECT_ATTRIBUTES::default();

    attributes.Size = wdf_struct_size!(WDF_OBJECT_ATTRIBUTES);
    attributes.ExecutionLevel = _WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent;
    attributes.SynchronizationScope = _WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent;

    attributes
}
