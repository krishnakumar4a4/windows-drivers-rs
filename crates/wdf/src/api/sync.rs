extern crate alloc;

use alloc::string::String;

use core::{
    cell::UnsafeCell,
    marker::PhantomData,
    sync::atomic::{Ordering, fence},
    ops::{Deref, DerefMut},
};
use wdk_sys::{call_unsafe_wdf_function_binding, NT_SUCCESS, WDFOBJECT, WDFSPINLOCK};
use wdk::println;
use crate::api::{
    error::NtResult,
    object::{Handle, RefCountedHandle},
    object_context::bug_check
};

use super::init_attributes;

/// WDF Spin Lock
pub struct SpinLock<T> {
    wdf_spin_lock: WDFSPINLOCK,
    data: UnsafeCell<T>,
}

unsafe impl<T> Sync for SpinLock<T> where T: Send {}

impl<T> SpinLock<T> {
    /// Construct a WDF Spin Lock object with data
    pub fn create(data: T) -> NtResult<Self> {
        let mut spin_lock = Self {
            wdf_spin_lock: core::ptr::null_mut(),
            data: UnsafeCell::new(data),
        };

        let mut attributes = init_attributes();

        // SAFETY: The resulting ffi object is stored in a private member and not
        // accessible outside of this module, and this module guarantees that it is
        // always in a valid state.
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfSpinLockCreate,
                &mut attributes,
                &mut spin_lock.wdf_spin_lock,
            )
        };

        // TODO: should we increment the ref count of the spin lock
        // to prevent it from being deleted while we are using it?
        // This is super important for soundness so look into it

        if NT_SUCCESS(status) {
            Ok(spin_lock)
        } else {
            Err(status.into())
        }
    }

    /// Acquire the spinlock and return a guard that will release the spinlock when dropped
    pub fn lock(&self) -> SpinLockGuard<T> {
        // SAFETY: `wdf_spin_lock` is a private member of `SpinLock`, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfSpinLockAcquire, self.wdf_spin_lock);
        }
        SpinLockGuard {
            spin_lock: self,
            _not_send: PhantomData,
        }
    }
}

impl<T> Drop for SpinLock<T> {
    fn drop(&mut self) {
        // SAFETY: `wdf_spin_lock` is a private member of `SpinLock`, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfObjectDelete, self.wdf_spin_lock as *mut _);
        }
    }
}

/// RAII guard for `SpinLock`.
///
/// The lock is acquired when the guard is created and released when the guard is dropped.
pub struct SpinLockGuard<'a, T> {
    spin_lock: &'a SpinLock<T>,

    // This marker makes SpinLockGuard !Send.
    // !Send is needed to ensure that the same
    // thread that acquired the lock releases it
    _not_send: PhantomData<*const ()>,
}

impl<'a, T> Drop for SpinLockGuard<'a, T> {
    fn drop(&mut self) {
        // SAFETY: `wdf_spin_lock` is a private member of `SpinLock`, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfSpinLockRelease, self.spin_lock.wdf_spin_lock);
        }
    }
}

impl<'a, T> core::ops::Deref for SpinLockGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.spin_lock.data.get() }
    }
}

impl<'a, T> core::ops::DerefMut for SpinLockGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.spin_lock.data.get() }
    }
}

/// Arc for WDF object handles
pub struct Arc<T: RefCountedHandle> {
    ptr: WDFOBJECT,
    _marker: core::marker::PhantomData<T>,
}

impl<T: RefCountedHandle> Arc<T> {
    /// Creates a new `Arc` from a raw WDF object pointer
    /// # Safety
    /// `ptr` must be a valid WDF object pointer that implements `RefCountedHandle`.
    pub(crate) unsafe fn from_raw(ptr: WDFOBJECT) -> Self {
        let obj = &*ptr.cast::<T>();
        let ref_count = obj.get_ref_count();

        // Relaxed ordering is fine here since we do not care if operations
        // on other variables including T (i.e. the data we are carrying)
        // get reordered with respect to fetch_add. 
        // After all it is totally okay to access T after the ref count has 
        // been incremented because the object is guaranteed to be alive
        // thanks to this very increment.
        // We also prevent the ref count from overflowing here by bugchecking
        // if it gets too high because an overflow would lead to all kinds of unsafety.
        if ref_count.fetch_add(1, Ordering::Relaxed) > usize::MAX / 2 {
            let ref_count = ref_count.load(Ordering::Relaxed);
            bug_check(0xDEADDEAD, ptr, Some(ref_count));
        }

        Self { ptr, _marker: PhantomData }
    }

    /// Gets the underlying raw WDF object pointer.
    fn as_ptr(&self) -> WDFOBJECT {
        self.ptr
    }
}

impl<T: RefCountedHandle> Clone for Arc<T> {
    fn clone(&self) -> Self {
        unsafe { Self::from_raw(self.ptr) }
    }
}

impl<T: RefCountedHandle> Drop for Arc<T> {
    fn drop(&mut self) {
        let obj = unsafe { &*self.as_ptr().cast::<T>() };
        let ref_count = obj.get_ref_count();

        println!("Drop {}: Ref count {}", Self::type_name(), ref_count.load(Ordering::Relaxed));

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
        // only Release in fetch_sub and have a separate Acquire fence
        // inside the if block.
        if ref_count.fetch_sub(1, Ordering::Release) == 1 {
            fence(Ordering::Acquire);

            println!("Drop {}: Ref count 0. Deleting obj", Self::type_name());

            // SAFETY: The object is guarateed to be valid here
            // because it is deleted only here and no place else
            unsafe {
                call_unsafe_wdf_function_binding!(WdfObjectDelete, self.ptr);
            }
        }
    }
}

impl<T: RefCountedHandle> Handle for Arc<T> {
    fn as_ptr(&self) -> WDFOBJECT {
        self.as_ptr()
    }

    fn type_name() -> String {
        let type_name = T::type_name();
        alloc::format!("Arc<{}>", type_name)
    }
}

impl<T: RefCountedHandle> Deref for Arc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr.cast::<Self::Target>() }
    }
}

impl <T: RefCountedHandle> DerefMut for Arc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.ptr.cast::<Self::Target>() }
    }
}   

// Safety: `Arc<T>` being `Sync` requires `T` to be `Sync`
// because sharing it effectively shares `T` However it
// also requires `T` to be `Send` because any thread that
// has `Arc<T>` can call `clone` on it and then later
// potentially drop `T` if it happens to be the last reference
unsafe impl<T: RefCountedHandle + Sync + Send> Sync for Arc<T> {}


// Safety: `Arc<T>` being `Send` requires `T` to be both `Send`
// and `Sync` for the same reason as above.
unsafe impl<T: RefCountedHandle + Sync + Send> Send for Arc<T> {}