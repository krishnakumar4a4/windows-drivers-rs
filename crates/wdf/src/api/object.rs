use wdk_sys::{call_unsafe_wdf_function_binding, WDFSPINLOCK, WDFOBJECT, WDF_OBJECT_ATTRIBUTES};

macro_rules! call_ref_func {
    ($func:ident, $obj:expr) => {
        unsafe {
            let file = file!();
            let file = if file.is_ascii() { file } else { "UTF8_name" };
            let file = file.as_bytes() as *const _ as *const i8;
            call_unsafe_wdf_function_binding!($func, $obj as *mut _, core::ptr::null_mut(), line!() as i32, file);
        }
    };
}

pub trait WdfObject {
    fn as_ptr(&self) -> WDFOBJECT;
}

pub struct WdfRc(WDFOBJECT);

impl WdfRc {
    pub unsafe fn new(wdf_obj: WDFOBJECT) -> Self {
        call_ref_func!(WdfObjectReferenceActual, wdf_obj);
        Self(wdf_obj)
    }

    pub fn inner(&self) -> WDFOBJECT {
        self.0
    }
}


impl Drop for WdfRc {
    fn drop(&mut self) {
        call_ref_func!(WdfObjectDereferenceActual, self.0);
    }
}