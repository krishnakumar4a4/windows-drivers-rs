extern crate alloc;

use alloc::string::String;
use core::{
    cell::UnsafeCell,
    ffi::c_void,
    marker::PhantomData,
    ops::Deref,
    ptr::NonNull,
    sync::atomic::{fence, AtomicUsize, Ordering},
};

use wdk::println;
use wdk_sys::{call_unsafe_wdf_function_binding, NT_SUCCESS, WDFOBJECT, WDFSPINLOCK};

use super::{
    error::NtResult,
    object::{Handle, init_attributes, RefCountedHandle},
    object_context::bug_check,
};

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

    /// Acquire the spinlock and return a guard that will release the spinlock
    /// when dropped
    pub fn lock(&self) -> SpinLockGuard<T> {
        // SAFETY: `wdf_spin_lock` is a private member of `SpinLock`, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfSpinLockAcquire, self.wdf_spin_lock);
        }
        SpinLockGuard {
            spin_lock: self,
            _no_sync_send: PhantomData,
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
/// The lock is acquired when the guard is created and released when the guard
/// is dropped.
pub struct SpinLockGuard<'a, T> {
    spin_lock: &'a SpinLock<T>,

    // This marker makes SpinLockGuard !Sync and !Send.
    // !Send is needed to ensure that the same
    // thread that acquired the lock releases it.
    // !Sync is not needed but you can't avoid it
    // with PhantomData
    _no_sync_send: PhantomData<*const ()>,
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

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.spin_lock.data.get() }
    }
}

impl<'a, T> core::ops::DerefMut for SpinLockGuard<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.spin_lock.data.get() }
    }
}

/// Arc for WDF object handles
pub struct Arc<T: RefCountedHandle> {
    // NonNull enables certain compiler optimizations
    // such as making Option<Arc<T>> have the same size
    // as *mut c_void
    ptr: NonNull<c_void>,
    _marker: PhantomData<T>,
}

impl<T: RefCountedHandle> Arc<T> {
    /// Creates a new `Arc` from a raw WDF object pointer
    /// # Safety
    /// `ptr` must be a valid WDF object pointer that implements
    /// `RefCountedHandle`.
    pub(crate) unsafe fn from_raw(ptr: WDFOBJECT) -> Self {
        let obj = &*ptr.cast::<T>();
        let ref_count = obj.get_ref_count();

        // Relaxed ordering is fine here since we do not care if
        // operations on ptr (i.e. the WDF pointer we are carrying)
        // get reordered with respect to fetch_add.
        // It is totally okay to for an access to ptr to occur after
        // the fetch_add call because the object is guaranteed to be
        // alive thanks to this very ref count increment.
        // Here we also prevent the ref count from overflowing by bugchecking
        // early because an overflow would lead to all kinds of unsafety.
        if ref_count.fetch_add(1, Ordering::Relaxed) > usize::MAX / 2 {
            let ref_count = ref_count.load(Ordering::Relaxed);
            bug_check(0xDEADDEAD, ptr, Some(ref_count));
        }

        // SAFETY: the incoming `ptr` is required to be non-null
        // by the safety contract of `from_raw`
        let ptr = unsafe { NonNull::new_unchecked(ptr) };

        Self {
            ptr,
            _marker: PhantomData,
        }
    }
}

impl<T: RefCountedHandle> Clone for Arc<T> {
    fn clone(&self) -> Self {
        unsafe { Self::from_raw(self.as_ptr()) }
    }
}

impl<T: RefCountedHandle> Drop for Arc<T> {
    fn drop(&mut self) {
        let obj = unsafe { &*self.as_ptr().cast::<T>() };
        let ref_count = obj.get_ref_count();

        println!(
            "Drop {}: Ref count {}",
            Self::type_name(),
            ref_count.load(Ordering::Relaxed)
        );

        // We need to ensure here that if we are the thread doing
        // the final delete (i.e calling WdfObjectDelete) then
        // all other threads are done accessing ptr or we will get
        // a use-after-free. Hence we must form a happens-before
        // relationship with all the other threads calling drop.
        // We could have achieved that by using the AcqRel ordering
        // in fetch_sub. But WdfObjectDelete is called only when the
        // ref count reaches zero. Therefore as an optimization we
        // use only the Release ordering in fetch_sub and have a
        // separate Acquire fence inside the if block.
        if ref_count.fetch_sub(1, Ordering::Release) == 1 {
            fence(Ordering::Acquire);

            println!("Drop {}: Ref count 0. Deleting obj", Self::type_name());

            // SAFETY: The object is guarateed to be valid here
            // because it is deleted only here and no place else
            unsafe {
                call_unsafe_wdf_function_binding!(WdfObjectDelete, self.as_ptr());
            }
        }
    }
}

impl<T: RefCountedHandle> Handle for Arc<T> {
    #[inline(always)]
    fn as_ptr(&self) -> WDFOBJECT {
        self.ptr.as_ptr()
    }

    fn type_name() -> String {
        let type_name = T::type_name();
        alloc::format!("Arc<{}>", type_name)
    }
}

impl<T: RefCountedHandle> Deref for Arc<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.as_ptr().cast::<Self::Target>() }
    }
}

// Safety: `Arc<T>` being `Sync` requires `T` to be `Sync`
// because sharing it effectively shares `T`. However it
// also requires `T` to be `Send` because any thread that
// has `&Arc<T>` can call `clone` on it and get an `Arc<T>`.
// Later that `Arc<T>` could drop `T` if it is the last
// reference implying that `T` is effectively moved.
unsafe impl<T: RefCountedHandle + Sync + Send> Sync for Arc<T> {}

// Safety: `Arc<T>` being `Send` requires `T` to be both `Send`
// and `Sync` for the same reason as above.
unsafe impl<T: RefCountedHandle + Sync + Send> Send for Arc<T> {}

/// Thread-safe version of `OnceCell`
///
/// Like `OnceCell` it allows initialization only
/// once and getting access to `&T` after that.
/// All operations, including initialization, are
/// thread-safe.
///
/// Compared to `OnceLock` it uses no locks
/// and relies purely on atomic operations.
pub struct AtomicOnceCell<T> {
    init_state: AtomicUsize,
    inner: UnsafeCell<Option<T>>,
}

const UNINITIALIZED: usize = 0;
const INITIALIZING: usize = 1;
const INITIALIZED: usize = 2;

impl<T> AtomicOnceCell<T> {
    /// Creates a new `AtomicOnceCell` instance
    pub const fn new() -> Self {
        Self {
            init_state: AtomicUsize::new(UNINITIALIZED),
            inner: UnsafeCell::new(None),
        }
    }

    /// Initializes the cell with the given value.
    ///
    /// # Returns
    /// Returns `Ok(())` if the cell was successfully initialized,
    /// or `Err(value)` if it was already initialized.
    pub fn set(&self, value: T) -> Result<(), T> {
        if self
            .init_state
            .compare_exchange(
                UNINITIALIZED,
                INITIALIZING,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            unsafe { (*self.inner.get()) = Some(value) };
            self.init_state.store(INITIALIZED, Ordering::Release);
            Ok(())
        } else {
            Err(value)
        }
    }

    /// Returns a reference to the inner value if the cell
    /// is initialized.
    ///
    /// # Returns
    /// `Some(&T)` if the cell is initialized, `None` otherwise.
    pub fn get(&self) -> Option<&T> {
        if self.init_state.load(Ordering::Acquire) == INITIALIZED {
            unsafe { (*self.inner.get()).as_ref() }
        } else {
            None
        }
    }
}

// Safety: `AtomicOnceCell` contains two pieces of data:
// the initialization state and the inner value `T`.
// The initialization state being atomic is automatically
// `Sync`. Therefore `AtomicOnceCell` is `Sync` if and
// only if `T` is `Sync`.
unsafe impl<T> Sync for AtomicOnceCell<T> where T: Sync {}

// Safety: For the same reason as above, `AtomicOnceCell<T>` is
// `Send` if and only if `T` is `Send`.
unsafe impl<T> Send for AtomicOnceCell<T> where T: Send {}
