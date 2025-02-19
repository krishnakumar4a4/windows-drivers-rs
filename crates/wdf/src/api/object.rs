use core::{cell::UnsafeCell, ops::{Deref, DerefMut}};
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
    unsafe fn from_ptr(inner: WDFOBJECT) -> Self;
    fn as_ptr(&self) -> WDFOBJECT;
}

pub struct OwnedRef<T: WdfObject> {
    wdf_obj: T,
}

impl<T: WdfObject> OwnedRef<T> {
    pub fn new(wdf_obj: T) -> Self {
        call_ref_func!(WdfObjectReferenceActual, wdf_obj.as_ptr());
        Self { wdf_obj }
    }

    pub unsafe fn from_ptr(inner: WDFOBJECT) -> Self {
        Self::new(T::from_ptr(inner))
    }

    pub fn as_ptr(&self) -> WDFOBJECT {
        self.wdf_obj.as_ptr()
    }
}

impl<T: WdfObject> Drop for OwnedRef<T> {
    fn drop(&mut self) {
        call_ref_func!(WdfObjectDereferenceActual, self.wdf_obj.as_ptr());
    }
}

impl<T: WdfObject> Deref for OwnedRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.wdf_obj
    }
}

impl <T: WdfObject> DerefMut for OwnedRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.wdf_obj
    }
}


pub struct SharedRef<T: WdfObject> {
    wdf_obj: UnsafeCell<T>,
    spin_lock: WDFSPINLOCK, // TODO: will have to support other kinds of locks as well
}

impl<T: WdfObject> SharedRef<T> {
    pub fn new(wdf_obj: T) -> Self {
        // TODO: Don't create the lock every time this function is called
        // because that might lead to too many locks i.e. one new lock every
        // time WDF calls our shim. Instead create it once and cache it in
        // the object's context and retrieve it here if available.
        let mut spin_lock: WDFSPINLOCK  = core::ptr::null_mut();
        let mut attributes = WDF_OBJECT_ATTRIBUTES::default();

        // Set the WDF object as the parent to ensure that
        // the lock is deleted along with it
        attributes.ParentObject = wdf_obj.as_ptr() as *mut _;


        // SAFETY: The resulting ffi object is stored in a private member and not
        // accessible outside of this module, and this module guarantees that it is
        // always in a valid state.
        unsafe {
            // TODO: for perf maybe use RW spin locks intead:
            // https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/reader-writer-spin-locks
            let _ = call_unsafe_wdf_function_binding!( // TODO: handle failure to create the lock
                WdfSpinLockCreate,
                &mut attributes as *mut _,
                &mut spin_lock as *mut _,
            );
        }

        call_ref_func!(WdfObjectReferenceActual, wdf_obj.as_ptr());

        Self { wdf_obj: UnsafeCell::new(wdf_obj), spin_lock }
    }

    pub fn lock_spin(&self) -> SharedRefGuard<T> {
        SharedRefGuard::lock_and_create(self)
    }

    pub fn as_ptr(&self) -> WDFOBJECT {
        unsafe { (*self.wdf_obj.get()).as_ptr() }
    }
}

impl<T: WdfObject> Clone for SharedRef<T> {
    fn clone(&self) -> Self {
        call_ref_func!(WdfObjectReferenceActual, self.as_ptr());
        Self { wdf_obj: unsafe { T::from_ptr(self.as_ptr()) }.into(), spin_lock: self.spin_lock }
    }
}

impl<T: WdfObject> Drop for SharedRef<T> {
    fn drop(&mut self) {
        call_ref_func!(WdfObjectDereferenceActual, self.as_ptr());
    }
}

pub struct SharedRefGuard<'a, T: WdfObject + 'a> {
    shared: &'a SharedRef<T>,
}

impl<'a, T: WdfObject> SharedRefGuard<'a, T> {
    fn lock_and_create(shared: &'a SharedRef<T>) -> Self {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfSpinLockAcquire, shared.spin_lock);
        }
        Self { shared }
    }
}

impl<'a, T: WdfObject> Drop for SharedRefGuard<'a, T> {
    fn drop(&mut self) {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfSpinLockRelease, self.shared.spin_lock as *mut _);
        }
    }
}

impl<'a, T: WdfObject> Deref for SharedRefGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.shared.wdf_obj.get() }
    }
}

impl<'a, T: WdfObject> DerefMut for SharedRefGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut*self.shared.wdf_obj.get() }
    }
}

// TODO: DANGER - these have not yet been properly verified
unsafe impl<T: Send + Sync + WdfObject> Sync for SharedRef<T> {}
unsafe impl<T: Send + Sync + WdfObject> Send for SharedRef<T> {}
unsafe impl<T: Sync + WdfObject> Sync for SharedRefGuard<'_, T> {}
unsafe impl<T: Send + WdfObject> Send for SharedRefGuard<'_, T> {}
