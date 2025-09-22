use alloc::string::String;
use core::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr,
    sync::atomic::{AtomicUsize, Ordering},
};

use wdk_sys::{
    call_unsafe_wdf_function_binding,
    ntddk::KeBugCheckEx,
    NT_SUCCESS,
    PCWDF_OBJECT_CONTEXT_TYPE_INFO,
    STATUS_INVALID_PARAMETER,
    WDFOBJECT,
    WDF_OBJECT_ATTRIBUTES,
    WDF_OBJECT_CONTEXT_TYPE_INFO,
    _WDF_EXECUTION_LEVEL,
    _WDF_SYNCHRONIZATION_SCOPE,
};

use super::{device::Device, init_wdf_struct, result::NtResult};

pub trait Handle {
    fn as_ptr(&self) -> WDFOBJECT;
    fn type_name() -> String;
}

pub trait RefCountedHandle: Handle {
    fn get_ref_count(&self) -> &AtomicUsize;
}

macro_rules! impl_handle {
    ($(#[$meta:meta])* $obj:ident) => {
        #[repr(C)]
        $(#[$meta])*
        pub struct $obj {
            _private: [u8; 0], // Prevents instantiation of the struct from driver code
        }

        impl crate::api::object::Handle for $obj {
            fn as_ptr(&self) -> wdk_sys::WDFOBJECT {
                (self as *const Self).cast_mut().cast()
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
                let inner_context = <$inner_context>::get(self);
                &inner_context.ref_count
            }
        }
    };
}

pub(crate) use impl_handle;
pub(crate) use impl_ref_counted_handle;

/// A trait for framework handles that can return the device they belong to.
pub trait GetDevice: Handle {
    // TODO: this trait is not the final design. We need something more coherent
    fn get_device(&self) -> &Device;
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Owned<T: Handle> {
    inner: WDFOBJECT,
    _marker: PhantomData<T>,
}

unsafe impl<T: Handle + Sync> Sync for Owned<T> {}
unsafe impl<T: Handle + Send> Send for Owned<T> {}

impl<T: Handle> Owned<T> {
    /// Creates a new `Owned<T>` instance.
    ///
    /// # Safety
    ///
    /// The provided `WDFOBJECT` pointer must be valid and of
    /// the raw WDF type corresponding to `T` and properly aligned.
    pub(crate) unsafe fn new(inner: WDFOBJECT) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
}

impl<T: Handle> Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.inner.cast::<Self::Target>()) }
    }
}

impl<T: Handle> DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.inner.cast::<Self::Target>()) }
    }
}

/// A Rust wrapper over [WDF context type info](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/ns-wdfobject-_wdf_object_context_type_info)
/// which is used while setting up an object context.
#[doc(hidden)]
#[repr(transparent)]
pub struct WdfObjectContextTypeInfo(WDF_OBJECT_CONTEXT_TYPE_INFO);

/// SAFETY: This type is NOT safe to send or share
/// across threads but still implements `Sync` because
/// it is used to declare a static during the setup
/// of WDF object contexts. It is not meant to be used
/// by the end user so this is okay.
unsafe impl Sync for WdfObjectContextTypeInfo {}

impl WdfObjectContextTypeInfo {
    pub const fn new(inner: WDF_OBJECT_CONTEXT_TYPE_INFO) -> Self {
        Self(inner)
    }

    pub const fn get_unique_type(&self) -> PCWDF_OBJECT_CONTEXT_TYPE_INFO {
        let inner = (self as *const Self).cast::<WDF_OBJECT_CONTEXT_TYPE_INFO>();
        // SAFETY: This dereference is sound since the underlying
        // WDF_OBJECT_CONTEXT_TYPE_INFO is guaranteed to have the same memory
        // layout as WDFObjectContextTypeInfo since WDFObjectContextTypeInfo is
        // declared as repr(transparent)
        unsafe { *inner }.UniqueType
    }
}
/// A trait indicating that this type is an object
/// context.
///
/// # Safety
///
/// This trait is unsafe because implementing it involves
/// inherently unsafe and error-prone operations like
/// declaring a static variable, correctly storing its
/// address in an `WDF_OBJECT_CONTEXT_TYPE_INFO` object
/// in a specific way and so on.
///
/// Typically you should not have to implement it by hand.
/// It is implemented automatically by the attributes
/// `object_context` and `object_context_with_ref_count_check`.
#[doc(hidden)]
pub unsafe trait ObjectContext: Sync {
    fn get_type_info() -> &'static WdfObjectContextTypeInfo;
}

// Smallest possible alignment of allocations made by
// WDF on 64 bit systems is 16 which comes form ExAllocatePool2
// or HeapAlloc which are the two functions used by WDF.
const MIN_FRAMEWORK_ALIGNMENT_ON_64_BIT: usize = 16;

pub unsafe fn attach_context<H: Handle, C: ObjectContext>(
    handle: &H,
    context: C,
    cleanup_callback: unsafe extern "C" fn(WDFOBJECT),
    destroy_callback: Option<unsafe extern "C" fn(WDFOBJECT)>,
) -> NtResult<()> {
    set_up_context::<H, C>(
        context,
        |mut attributes| {
            let mut wdf_context: *mut C = core::ptr::null_mut();
            let status = unsafe {
                call_unsafe_wdf_function_binding!(
                    WdfObjectAllocateContext,
                    handle.as_ptr(),
                    &mut attributes,
                    (&mut wdf_context as *mut *mut C).cast(),
                )
            };

            if !NT_SUCCESS(status) {
                return Err(status.into());
            }

            Ok(wdf_context)
        },
        cleanup_callback,
        destroy_callback,
    )
}

pub unsafe fn create_with_context<T: Handle, U: ObjectContext>(
    create: impl Fn(WDF_OBJECT_ATTRIBUTES) -> NtResult<T>,
    context: U,
    cleanup_callback: unsafe extern "C" fn(WDFOBJECT),
    destroy_callback: Option<unsafe extern "C" fn(WDFOBJECT)>,
) -> NtResult<T> {
    let mut created_obj: Option<T> = None;
    set_up_context::<T, U>(
        context,
        |attributes| {
            let obj = create(attributes)?;
            let raw_context = get_context_raw::<U>(obj.as_ptr());
            if raw_context.is_null() {
                return Err(STATUS_INVALID_PARAMETER.into());
            }

            created_obj = Some(obj);

            Ok(raw_context)
        },
        cleanup_callback,
        destroy_callback,
    )?;

    Ok(created_obj.unwrap()) // The object is guaranteed to be valid here
}

fn set_up_context<H: Handle, C: ObjectContext>(
    context: C,
    mut create_wdf_context: impl FnMut(WDF_OBJECT_ATTRIBUTES) -> NtResult<*mut C>,
    cleanup_callback: unsafe extern "C" fn(WDFOBJECT),
    destroy_callback: Option<unsafe extern "C" fn(WDFOBJECT)>,
) -> NtResult<()> {
    // Make sure the alignment requirement of the object struct
    // does not exceed the minimum possible alignment of allocations
    // made by the framework.
    if core::mem::align_of::<C>() > MIN_FRAMEWORK_ALIGNMENT_ON_64_BIT {
        return Err(STATUS_INVALID_PARAMETER.into());
    }

    let mut attributes = init_attributes();
    attributes.ContextTypeInfo = C::get_type_info().get_unique_type();
    attributes.EvtCleanupCallback = Some(cleanup_callback);
    if destroy_callback.is_some() {
        attributes.EvtDestroyCallback = destroy_callback;
    }

    let wdf_context: *mut C = create_wdf_context(attributes)?;

    // SAFETY: The required alignment of the Rust context type
    // is guaranteed to not conflict with the alignment of the
    // memory allocated by the framework thanks to the alignment
    // check above.
    unsafe {
        core::ptr::write(wdf_context, context);
    }

    Ok(())
}

/// Gets context of type `C` for the given framework handle if it exists
pub fn try_get_context<H: Handle, C: ObjectContext>(handle: &H) -> Option<&C> {
    // SAFETY: The pointer to framework handle is obtained via as_ptr()
    // which is guaranteed to be valid
    let context = unsafe { get_context_raw::<C>(handle.as_ptr()) };

    if !context.is_null() {
        Some(unsafe { &*context })
    } else {
        None
    }
}

/// Gets mutable context of type `C` for the given framework handle if it
/// exists
pub fn try_get_context_mut<H: Handle, C: ObjectContext>(handle: &mut H) -> Option<&mut C> {
    // SAFETY: The pointer to framework handle is obtained via as_ptr()
    // which is guaranteed to be valid
    let context = unsafe { get_context_raw::<C>(handle.as_ptr()) };

    if !context.is_null() {
        Some(unsafe { &mut *context })
    } else {
        None
    }
}

/// Gets raw pointer to the context object for a given raw framework handle.
// SAFETY: The pointer to WDF object must point to a valid WDF object
unsafe fn get_context_raw<C: ObjectContext>(handle: WDFOBJECT) -> *mut C {
    let context_metadata = C::get_type_info();

    unsafe {
        call_unsafe_wdf_function_binding!(
            WdfObjectGetTypedContextWorker,
            handle,
            &context_metadata.0
        )
        .cast::<C>()
    }
}

pub unsafe fn drop_context<C: ObjectContext>(handle: WDFOBJECT) {
    let context_metadata = C::get_type_info();

    let context = unsafe {
        call_unsafe_wdf_function_binding!(
            WdfObjectGetTypedContextWorker,
            handle,
            &context_metadata.0
        )
        .cast::<C>()
    };

    if !context.is_null() {
        unsafe {
            ptr::drop_in_place(context);
        }
    }
}

pub(crate) fn bug_check_if_ref_count_not_zero<H: RefCountedHandle, C: ObjectContext>(
    obj: WDFOBJECT,
) {
    let handle = unsafe { &*obj.cast::<H>() };
    let ref_count = handle.get_ref_count().load(Ordering::Acquire);
    if ref_count > 0 {
        bug_check(0xDEADDEAD, obj, Some(ref_count));
    }
}

pub(crate) fn bug_check(code: u32, obj: WDFOBJECT, ref_count: Option<usize>) {
    let ref_count = ref_count.unwrap_or(0);
    unsafe {
        KeBugCheckEx(code, obj as u64, ref_count as u64, 0, 0);
    }
}

pub(crate) fn init_attributes() -> WDF_OBJECT_ATTRIBUTES {
    let mut attributes = init_wdf_struct!(WDF_OBJECT_ATTRIBUTES);
    attributes.ExecutionLevel = _WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent;
    attributes.SynchronizationScope =
        _WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent;

    attributes
}
