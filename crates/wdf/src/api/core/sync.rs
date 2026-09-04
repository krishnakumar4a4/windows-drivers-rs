use core::{
    cell::UnsafeCell,
    ffi::c_void,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::atomic::{AtomicIsize, Ordering, fence},
};

use wdk_sys::{WDFOBJECT, WDFSPINLOCK, WDFWAITLOCK, call_unsafe_wdf_function_binding};

use super::{
    Timeout,
    object::{Handle, RefCountedHandle, init_attributes, panic},
    result::{NtResult, StatusCodeExt},
};

/// Software interrupt request levels supported by WPP's `irql` type.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IRQL {
    /// Normal thread execution.
    Passive = wdk_sys::PASSIVE_LEVEL as u8,
    /// Asynchronous procedure call execution.
    Apc = wdk_sys::APC_LEVEL as u8,
    /// Dispatcher and DPC execution.
    Dispatch = wdk_sys::DISPATCH_LEVEL as u8,
}

impl IRQL {
    /// Returns the current processor's interrupt request level when it is a
    /// software IRQL representable by this type.
    pub fn current() -> Option<Self> {
        let current = unsafe { wdk_sys::ntddk::KeGetCurrentIrql() };
        Self::try_from(current).ok()
    }
}

impl From<IRQL> for wdk_sys::KIRQL {
    fn from(value: IRQL) -> Self {
        value as Self
    }
}

impl TryFrom<wdk_sys::KIRQL> for IRQL {
    type Error = wdk_sys::KIRQL;

    fn try_from(value: wdk_sys::KIRQL) -> Result<Self, Self::Error> {
        match value {
            value if value == Self::Passive as wdk_sys::KIRQL => Ok(Self::Passive),
            value if value == Self::Apc as wdk_sys::KIRQL => Ok(Self::Apc),
            value if value == Self::Dispatch as wdk_sys::KIRQL => Ok(Self::Dispatch),
            value => Err(value),
        }
    }
}

unsafe impl wpp::WppValue for IRQL {
    type Wire = u8;

    const TYPE_NAME: &'static str = "ItemListByte(Low,APC,DPC)";
    const FORMAT_SPEC: &'static str = "!s!";

    fn into_wire(self) -> Self::Wire {
        self.into()
    }
}

#[cfg(test)]
mod irql_tests {
    use wpp::{IntoWppField, WppField};

    use super::IRQL;

    #[test]
    fn irql_serializes_as_one_byte() {
        assert_eq!(IRQL::Passive.into_wpp_field().as_bytes(), &[wdk_sys::PASSIVE_LEVEL as u8]);
        assert_eq!(IRQL::Apc.into_wpp_field().as_bytes(), &[wdk_sys::APC_LEVEL as u8]);
        assert_eq!(IRQL::Dispatch.into_wpp_field().as_bytes(), &[wdk_sys::DISPATCH_LEVEL as u8]);
    }

    #[test]
    fn irql_converts_to_and_from_kirql() {
        for irql in [IRQL::Passive, IRQL::Apc, IRQL::Dispatch] {
            let raw = wdk_sys::KIRQL::from(irql);
            assert_eq!(IRQL::try_from(raw), Ok(irql));
        }
        assert_eq!(IRQL::try_from(3), Err(3));
    }

    #[test]
    fn irql_uses_wpp_list_format() {
        type Field = <IRQL as IntoWppField>::Output;
        assert_eq!(Field::TYPE_NAME, "ItemListByte(Low,APC,DPC)");
        assert_eq!(Field::FORMAT_SPEC, "!s!");
    }
}

/// WDF Spin Lock
pub struct SpinLock<T> {
    wdf_spin_lock: WDFSPINLOCK,
    data: UnsafeCell<T>,
}

/// `SpinLock` requires `T` to be `Send` because non-`Send`
/// types can lead to situations where a thread NOT holding
/// the lock can also access the data. An example of this
/// is `Rc` wherein the lock will protect only one clone of
/// `Rc` and another thread can still access the data through
/// another clone without taking the lock.
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
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfSpinLockCreate,
                &mut attributes,
                &mut spin_lock.wdf_spin_lock,
            )
        }
        .map(|| spin_lock)
    }

    /// Acquire the spinlock and return a guard that will release the spinlock
    /// when dropped
    pub fn lock(&self) -> SpinLockGuard<'_, T> {
        // SAFETY: `wdf_spin_lock` is a private member of `SpinLock`, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfSpinLockAcquire, self.wdf_spin_lock);
        }
        SpinLockGuard { spin_lock: self }
    }
}

impl<T> Drop for SpinLock<T> {
    fn drop(&mut self) {
        // SAFETY: `wdf_spin_lock` is a private member of `SpinLock`, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfObjectDelete, self.wdf_spin_lock.cast());
        }
    }
}

/// RAII guard for `SpinLock`.
///
/// The lock is acquired when the guard is created and released when the guard
/// is dropped.
pub struct SpinLockGuard<'a, T> {
    spin_lock: &'a SpinLock<T>,
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

impl<'a, T> Deref for SpinLockGuard<'a, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.spin_lock.data.get() }
    }
}

impl<'a, T> DerefMut for SpinLockGuard<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.spin_lock.data.get() }
    }
}

/// WDF Wait Lock
pub struct WaitLock<T> {
    wdf_wait_lock: WDFWAITLOCK,
    data: UnsafeCell<T>,
}

/// `WaitLock` requires `T` to be `Send` because non-`Send`
/// types can lead to situations where a thread NOT holding
/// the lock can also access the data. An example of this
/// is `Rc` wherein the lock will protect only one clone of
/// `Rc` and another thread can still access the data through
/// another clone without taking the lock.
unsafe impl<T> Sync for WaitLock<T> where T: Send {}

impl<T> WaitLock<T> {
    /// Construct a WDF Wait Lock object with data.
    pub fn create(data: T) -> NtResult<Self> {
        let mut wait_lock = Self {
            wdf_wait_lock: core::ptr::null_mut(),
            data: UnsafeCell::new(data),
        };

        let mut attributes = init_attributes();

        // SAFETY: The resulting ffi object is stored in a private member and not
        // accessible outside of this module, and this module guarantees that it is
        // always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfWaitLockCreate,
                &mut attributes,
                &mut wait_lock.wdf_wait_lock,
            )
        }
        .map(|| wait_lock)
    }

    /// Acquire the wait lock, blocking indefinitely until it
    /// becomes available.
    ///
    /// Returns a guard that releases the lock when dropped.
    pub fn lock(&self) -> WaitLockGuard<'_, T> {
        // SAFETY: `wdf_wait_lock` is a private member, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        // Passing null for the timeout means wait indefinitely, which always
        // succeeds.
        let _ = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfWaitLockAcquire,
                self.wdf_wait_lock,
                core::ptr::null_mut(),
            )
        };
        WaitLockGuard { wait_lock: self }
    }

    /// Try to acquire the wait lock with a timeout.
    ///
    /// Returns `Some(guard)` if the lock was acquired, or `None`
    /// if the timeout elapsed.
    pub fn try_lock(&self, timeout: Timeout) -> Option<WaitLockGuard<'_, T>> {
        let mut timeout_value = timeout.as_wdf_timeout();

        // SAFETY: `wdf_wait_lock` is a private member, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfWaitLockAcquire,
                self.wdf_wait_lock,
                &mut timeout_value,
            )
        };

        if status.is_success() {
            Some(WaitLockGuard { wait_lock: self })
        } else {
            None
        }
    }
}

impl<T> Drop for WaitLock<T> {
    fn drop(&mut self) {
        // SAFETY: `wdf_wait_lock` is a private member, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfObjectDelete, self.wdf_wait_lock.cast());
        }
    }
}

/// RAII guard for [`WaitLock`].
///
/// The lock is acquired when the guard is created and released when the guard
/// is dropped.
pub struct WaitLockGuard<'a, T> {
    wait_lock: &'a WaitLock<T>,
}

impl<'a, T> Drop for WaitLockGuard<'a, T> {
    fn drop(&mut self) {
        // SAFETY: `wdf_wait_lock` is a private member, originally created
        // by WDF, and this module guarantees that it is always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(WdfWaitLockRelease, self.wait_lock.wdf_wait_lock);
        }
    }
}

impl<'a, T> Deref for WaitLockGuard<'a, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.wait_lock.data.get() }
    }
}

impl<'a, T> DerefMut for WaitLockGuard<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.wait_lock.data.get() }
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
    /// Creates a new `Arc` from a raw WDF object pointer and
    /// increments the ref count by 1.
    ///
    /// # Safety
    ///
    /// The following requirements must be met:
    /// - `ptr` must be non-null
    /// - `ptr` must be a valid WDF object pointer that implements
    /// `RefCountedHandle`.
    /// - The ref count of the object pointed to by `ptr` must be 0
    /// or greater (`from_raw` will increment it by 1)
    pub(crate) unsafe fn from_raw(ptr: WDFOBJECT) -> Self {
        let obj = unsafe { &*ptr.cast::<T>() };
        let ref_count = obj.get_ref_count();

        // Relaxed ordering is fine here since we do not care if
        // operations on ptr (i.e. the WDF pointer we are carrying)
        // get reordered with respect to fetch_add.
        // It is totally okay for an access to ptr to occur after
        // the fetch_add call because the object is guaranteed to be
        // alive thanks to this very ref count increment.
        // Here we also prevent the ref count from overflowing by bugchecking
        // early because an overflow would lead to all kinds of unsafety.
        if ref_count.fetch_add(1, Ordering::Relaxed) > usize::MAX / 2 {
            let ref_count = ref_count.load(Ordering::Relaxed);
            panic(0xDEADDEAD, ptr, Some(ref_count));
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
            obj.delete_raw_handle();
        }
    }
}

impl<T: RefCountedHandle> Handle for Arc<T> {
    #[inline(always)]
    fn as_ptr(&self) -> WDFOBJECT {
        self.ptr.as_ptr()
    }
}

impl<T: RefCountedHandle> Deref for Arc<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.as_ptr().cast::<Self::Target>() }
    }
}

/// `Arc<T>` being `Sync` requires `T` to be `Sync`
/// because sharing `Arc<T>` effectively shares `T`. It
/// also requires `T` to be `Send` because any thread that
/// has `&Arc<T>` can call `clone` on it and get an `Arc<T>`.
/// Later that `Arc<T>` could drop `T` if it is the last
/// reference, implying that `T` is effectively being moved.
unsafe impl<T: RefCountedHandle + Sync + Send> Sync for Arc<T> {}

/// `Arc<T>` being `Send` requires `T` to be both `Send`
/// and `Sync` for the same reason as above.
unsafe impl<T: RefCountedHandle + Sync + Send> Send for Arc<T> {}

/// Thread-safe version of `RefCell`
///
/// Behaves like a reader-writer lock except it never
/// blocks any thread. If a borrow cannot be obtained,
/// `None` is returned instead.
///
/// Uses an `AtomicIsize` borrow counter:
/// - `0` means unborrowed
/// - `> 0` means shared (number of active readers)
/// - `-1` means exclusively borrowed (writer active)
///
/// All operations are lock-free and work at any IRQL.
pub struct AtomicRefCell<T> {
    borrow_state: AtomicIsize,
    data: UnsafeCell<T>,
}

/// `AtomicRefCell` requires `T` to be `Send` because
/// non-`Send` types can lead to situations where a thread
/// NOT holding the borrow can also access the data. An
/// example of this is `Rc` wherein the borrow will protect
/// only one clone of `Rc` and another thread can still
/// access the data through another clone without borrowing.
///
/// `AtomicRefCell` also requires `T` to be `Sync` because
/// `borrow()` hands out shared references `&T` to multiple
/// threads concurrently, which is only valid if `T` supports
/// shared references across threads.
unsafe impl<T> Sync for AtomicRefCell<T> where T: Send + Sync {}

/// `AtomicRefCell<T>` is `Send` if `T` is `Send` because
/// moving the cell to another thread moves the owned `T`.
unsafe impl<T> Send for AtomicRefCell<T> where T: Send {}

impl<T> AtomicRefCell<T> {
    /// Creates a new `AtomicRefCell` with the given value
    pub const fn new(data: T) -> Self {
        Self {
            borrow_state: AtomicIsize::new(0),
            data: UnsafeCell::new(data),
        }
    }

    /// Attempts to immutably borrow the value.
    ///
    /// Returns `Some(AtomicRef)` if no exclusive borrow
    /// is active, or `None` if the value is currently
    /// exclusively borrowed.
    ///
    /// Multiple shared borrows can be held simultaneously.
    pub fn borrow(&self) -> Option<AtomicRef<'_, T>> {
        // `Relaxed` is sufficient for the initial load because this
        // is just a hint for the CAS loop. If the value is stale,
        // compare_exchange_weak will fail and give us the fresh
        // value — no synchronization is needed here.
        let mut cur = self.borrow_state.load(Ordering::Relaxed);

        loop {
            // Exclusively borrowed — cannot obtain shared borrow
            if cur < 0 {
                return None;
            }

            // Shared borrow count saturated — cannot obtain another borrow.
            if cur == isize::MAX {
                return None;
            }

            // `Acquire` on success pairs with `Release` in both
            // AtomicRef::drop and AtomicRefMut::drop. This
            // ensures we see all writes a previous writer
            // performed before releasing its exclusive borrow.
            //
            // `Relaxed` on failure because we do not access the
            // data on the failure path. The returned error
            // value is used as the next expected value.
            //
            // compare_exchange_weak (rather than strong) is used
            // because we are already in a retry loop. Weak is
            // allowed to spuriously fail, which lets the compiler
            // emit more efficient LL/SC instructions on
            // architectures that do not have true hardware CAS.
            match self.borrow_state.compare_exchange_weak(
                cur,
                cur + 1,
                Ordering::Acquire,
                Ordering::Relaxed,
            ) {
                Ok(_) => return Some(AtomicRef { cell: self }),
                Err(next) => cur = next,
            }
        }
    }

    /// Attempts to mutably borrow the value.
    ///
    /// Returns `Some(AtomicRefMut)` if there are no active
    /// borrows (shared or exclusive), or `None` otherwise.
    ///
    /// Only one exclusive borrow can be held at a time,
    /// and it cannot coexist with shared borrows.
    pub fn borrow_mut(&self) -> Option<AtomicRefMut<'_, T>> {
        // `Acquire` on success pairs with `Release` in both
        // AtomicRef::drop (readers releasing) and
        // AtomicRefMut::drop (previous writer releasing).
        // This ensures we see all data modifications made
        // by any prior borrow holder before we access the
        // UnsafeCell.
        //
        // `Relaxed` on failure because we return None
        // immediately — no data is accessed.
        //
        // Strong compare_exchange (not weak) is used because
        // there is no retry loop. A spurious failure would
        // incorrectly return None to the caller when the
        // borrow was actually available.
        if self
            .borrow_state
            .compare_exchange(0, -1, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            Some(AtomicRefMut { cell: self })
        } else {
            None
        }
    }

    /// Bugchecks with the address of this cell and the
    /// invalid borrow state value for debugging.
    fn bug_check_invalid_state(&self, state: isize) -> ! {
        panic(
            0xDEADDEAD,
            (self as *const Self).cast_mut().cast::<_>(),
            Some(state as usize),
        )
    }
}

impl<T> AtomicRefCell<Option<T>> {
    /// Attempts to immutably borrow the inner `T`.
    ///
    /// Returns `Some(InnerAtomicRef)` if the `Option` contains
    /// a value **and** no exclusive borrow is active. The
    /// returned guard dereferences directly to `&T`.
    ///
    /// Returns `None` if:
    /// - The `Option` is `None`, or
    /// - An exclusive borrow is currently held.
    pub fn borrow_inner(&self) -> Option<InnerAtomicRef<'_, T>> {
        let guard = self.borrow()?;

        if guard.is_none() {
            return None;
        }

        Some(InnerAtomicRef { inner: guard })
    }

    /// Attempts to exclusively borrow the inner `T`.
    ///
    /// Returns `Some(InnerAtomicRefMut)` if the `Option` contains
    /// a value **and** no other borrow (shared or exclusive) is
    /// active. The returned guard dereferences directly to
    /// `&mut T`.
    ///
    /// Returns `None` if:
    /// - The `Option` is `None`, or
    /// - Any borrow is currently held.
    pub fn borrow_inner_mut(&self) -> Option<InnerAtomicRefMut<'_, T>> {
        let guard = self.borrow_mut()?;

        if guard.is_none() {
            return None;
        }

        Some(InnerAtomicRefMut { inner: guard })
    }
}

/// RAII guard for a shared borrow from [`AtomicRefCell`].
///
/// The shared borrow is held for the lifetime of this guard
/// and released when it is dropped.
pub struct AtomicRef<'a, T> {
    cell: &'a AtomicRefCell<T>,
}

impl<T> Drop for AtomicRef<'_, T> {
    fn drop(&mut self) {
        // `Release` pairs with `Acquire` in borrow() and
        // borrow_mut(). This ensures that all reads
        // performed through this guard are completed
        // before the borrow count decreases.
        //
        // Every reader needs `Release` (not just the last
        // one going from 1 → 0) because unlike Arc::drop
        // where 1 → 0 means the object is being destroyed
        // and no further access will occur, here a writer
        // could be accessing T next and we don't want any
        // of the readers' accesses to be reordered such
        // that they occur concurrently with the writer's
        // writes.
        let prev = self.cell.borrow_state.fetch_sub(1, Ordering::Release);

        // prev must be > 0 (at least one active reader).
        // A value <= 0 means the borrow state is corrupted.
        if prev <= 0 {
            self.cell.bug_check_invalid_state(prev);
        }
    }
}

impl<T> Deref for AtomicRef<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: The borrow state guarantees that no exclusive
        // borrow is active while this guard exists, so reading
        // from the UnsafeCell is safe.
        unsafe { &*self.cell.data.get() }
    }
}

/// RAII guard for an exclusive borrow from [`AtomicRefCell`].
///
/// The exclusive borrow is held for the lifetime of this guard
/// and released when it is dropped.
pub struct AtomicRefMut<'a, T> {
    cell: &'a AtomicRefCell<T>,
}

impl<T> Drop for AtomicRefMut<'_, T> {
    fn drop(&mut self) {
        // `Release` pairs with `Acquire` in borrow() and
        // borrow_mut(). This ensures that all writes
        // performed through this guard via DerefMut are
        // visible to the next thread that successfully
        // acquires a borrow.
        //
        // A swap (not a CAS loop) is used because we hold
        // the exclusive borrow — the state is guaranteed to
        // be -1 and no other thread can change it
        let prev = self.cell.borrow_state.swap(0, Ordering::Release);

        // prev must be -1 (exclusively borrowed).
        // Any other value means the borrow state is corrupted.
        if prev != -1 {
            self.cell.bug_check_invalid_state(prev);
        }
    }
}

impl<T> Deref for AtomicRefMut<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: The borrow state guarantees exclusive access
        // while this guard exists.
        unsafe { &*self.cell.data.get() }
    }
}

impl<T> DerefMut for AtomicRefMut<'_, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: The borrow state guarantees exclusive access
        // while this guard exists.
        unsafe { &mut *self.cell.data.get() }
    }
}

/// RAII guard for a shared borrow of the inner value
/// from an `AtomicRefCell<Option<T>>`.
///
/// Dereferences to `&T`
pub struct InnerAtomicRef<'a, T> {
    inner: AtomicRef<'a, Option<T>>,
}

impl<T> Deref for InnerAtomicRef<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: `InnerAtomicRef` is only created inside
        // `borrow_inner` after confirming that the `Option`
        // is `Some`. The `Option` cannot be changed back to
        // `None` while `inner` borrow is alive. Therefore it is
        // safe to use `unwrap_unchecked`
        unsafe { self.inner.as_ref().unwrap_unchecked() }
    }
}

/// RAII guard for an exclusive borrow of the inner value
/// from an `AtomicRefCell<Option<T>>`.
///
/// Dereferences to `&T` and `&mut T`
pub struct InnerAtomicRefMut<'a, T> {
    inner: AtomicRefMut<'a, Option<T>>,
}

impl<T> Deref for InnerAtomicRefMut<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: `unwrap_unchecked` is safe to use due to
        // the same reasoning as `InnerAtomicRef::deref`.
        unsafe { self.inner.as_ref().unwrap_unchecked() }
    }
}

impl<T> DerefMut for InnerAtomicRefMut<'_, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: `unwrap_unchecked` is safe to use due to
        // the same reasoning as `InnerAtomicRef::deref`.
        unsafe { self.inner.as_mut().unwrap_unchecked() }
    }
}
