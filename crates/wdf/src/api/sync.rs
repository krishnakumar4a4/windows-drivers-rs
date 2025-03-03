use core::cell::UnsafeCell;
use wdk_sys::{call_unsafe_wdf_function_binding, NT_SUCCESS, WDFSPINLOCK, WDF_OBJECT_ATTRIBUTES};

use crate::api::error::NtResult;

/// WDF Spin Lock
pub struct SpinLock<T> {
    wdf_spin_lock: WDFSPINLOCK,
    data: UnsafeCell<T>,
}

unsafe impl<T> Sync for SpinLock<T> where T: Send {}

impl<T> SpinLock<T> {
    /// Try to construct a WDF Spin Lock object with data
    ///
    /// # Errors
    ///
    /// This function will return an error if WDF fails to construct a timer. The error variant will contain a [`NTSTATUS`] of the failure. Full error documentation is available in the [WDFSpinLock Documentation](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfsync/nf-wdfsync-wdfspinlockcreate#return-value)
    pub fn create(data: T) -> NtResult<Self> {
        let mut spin_lock = Self {
            wdf_spin_lock: core::ptr::null_mut(),
            data: UnsafeCell::new(data),
        };

        let mut attributes = WDF_OBJECT_ATTRIBUTES::default();

        // SAFETY: The resulting ffi object is stored in a private member and not
        // accessible outside of this module, and this module guarantees that it is
        // always in a valid state.
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfSpinLockCreate,
                &mut attributes as *mut _,
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
        SpinLockGuard { spin_lock: self, _not_send: core::marker::PhantomData }
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
    _not_send: core::marker::PhantomData<*const ()>,
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