// Copyright (c) Microsoft Corporation.
// License: MIT OR Apache-2.0

use core::sync::atomic::Ordering;
use crate::api::{init_attributes, Handle, RefCountedHandle, NtResult};
use wdk_sys::{
    call_unsafe_wdf_function_binding, NT_SUCCESS, PCWDF_OBJECT_CONTEXT_TYPE_INFO, WDFOBJECT, WDF_OBJECT_CONTEXT_TYPE_INFO,
    WDF_OBJECT_ATTRIBUTES, STATUS_INVALID_PARAMETER, ntddk::KeBugCheckEx,
};

#[doc(hidden)]
#[repr(transparent)]
pub struct WdfObjectContextTypeInfo(WDF_OBJECT_CONTEXT_TYPE_INFO);

/// SAFETY: This type is NOT safe to send across threads
/// but it will never be used by the end user.
/// It is used only to declare a static during the setup
/// of WDF object contexts and is only internally used by
/// WDF wherein proper locks are taken to ensure thread safety.
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

/// Marker trait that must be implemented by
/// any types that are to be used as context objects
pub unsafe trait ObjectContext: Sync {
    fn get_type_info() -> &'static WdfObjectContextTypeInfo;
}

// Smallest possible alignment of allocations made by
// WDF on 64 bit systems is 16 which comes form ExAllocatePool2
// or HeapAlloc which are the two functions used by WDF.
const MIN_FRAMEWORK_ALIGNMENT_ON_64_BIT: usize = 16;

pub unsafe fn attach_context<T: Handle, U: ObjectContext>(
    fw_obj: &mut T,
    context: U,
    cleanup_callback: unsafe extern "C" fn(WDFOBJECT),
    destroy_callback: Option<unsafe extern "C" fn(WDFOBJECT)>
) -> NtResult<()> {
    set_up_context::<T, U>(
        context,
        |mut attributes| {
            let mut wdf_context: *mut U = core::ptr::null_mut();
            let status = unsafe {
                call_unsafe_wdf_function_binding!(
                    WdfObjectAllocateContext,
                    fw_obj.as_raw(),
                    &mut attributes,
                    core::mem::transmute(&mut wdf_context),
                )
            };

            if !NT_SUCCESS(status) {
                return Err(status.into());
            }

            Ok(wdf_context)
        },
        cleanup_callback,
        destroy_callback
    )
}

pub unsafe fn create_with_context<T: Handle, U: ObjectContext>(
    create: impl Fn(WDF_OBJECT_ATTRIBUTES) -> NtResult<T>,
    context: U,
    cleanup_callback: unsafe extern "C" fn(WDFOBJECT),
    destroy_callback: Option<unsafe extern "C" fn(WDFOBJECT)>
) -> NtResult<T> {
    let mut created_obj: Option<T> = None;
    set_up_context::<T, U>(
        context,
        |attributes| {
            let obj = create(attributes)?;
            let raw_context = get_context_raw::<U>(obj.as_raw());
            if raw_context.is_null() {
                return Err(STATUS_INVALID_PARAMETER.into());
            }

            created_obj = Some(obj);

            Ok(raw_context)
        },
        cleanup_callback,
        destroy_callback
    )?;

    Ok(created_obj.unwrap()) // The object is guaranteed to be valid here
}

fn set_up_context<T: Handle, U: ObjectContext>(
    context: U,
    mut create_wdf_context: impl FnMut(WDF_OBJECT_ATTRIBUTES) -> NtResult<*mut U>,
    cleanup_callback: unsafe extern "C" fn(WDFOBJECT),
    destroy_callback: Option<unsafe extern "C" fn(WDFOBJECT)>
) -> NtResult<()> {
    // Make sure the aligntment requirement of the object struct
    // does not exceed the minimum possible alignment of allocations
    // made by the framework.
    if core::mem::align_of::<U>() > MIN_FRAMEWORK_ALIGNMENT_ON_64_BIT {
        return Err(STATUS_INVALID_PARAMETER.into());
    }

    let mut attributes = init_attributes();
    attributes.ContextTypeInfo = U::get_type_info().get_unique_type();
    attributes.EvtCleanupCallback = Some(cleanup_callback);
    if destroy_callback.is_some() {
        attributes.EvtDestroyCallback = destroy_callback;
    }

    let wdf_context: *mut U = create_wdf_context(attributes)?;

    // SAFETY: The required alignment of the Rust context type
    // is guaranteed to not conflict with the alignment of the
    // memory allocated by the framework thanks to the alignment
    // check above.
    unsafe {
        core::ptr::write(wdf_context, context);
    }

    Ok(())
}

/// Gets context of type `U` for the given frameework object if it exists
pub fn get_context<T: Handle, U: ObjectContext>(fw_obj: &T) -> Option<&U> {
    // SAFETY: The pointer to framewok object is obtained via as_raw()
    // which is guaranteed to be valid
    let context = unsafe { get_context_raw::<U>(fw_obj.as_raw()) };

    if !context.is_null() {
        Some(unsafe { &*context })
    } else {
        None
    }
}

/// Gets raw pointer to the context object for a given raw framework handle.
// SAFETY: The pointer to WDF object must point to a valid WDF object
unsafe fn get_context_raw<U: ObjectContext>(fw_obj: WDFOBJECT) -> *mut U {
    let context_metadata = U::get_type_info();

    unsafe {
        call_unsafe_wdf_function_binding!(
            WdfObjectGetTypedContextWorker,
            fw_obj,
            &context_metadata.0
        ) as *mut U
    }
}

pub unsafe fn drop_context<U: ObjectContext>(fw_obj: WDFOBJECT) {
    let context_metadata = U::get_type_info();

    let context = unsafe {
        call_unsafe_wdf_function_binding!(
            WdfObjectGetTypedContextWorker,
            fw_obj,
            &context_metadata.0
        ) as *mut core::mem::ManuallyDrop<U>
    };

    if !context.is_null() {
        unsafe {
            core::mem::ManuallyDrop::drop(&mut *context);
        }
    }
}

#[doc(hidden)]
pub(crate) fn bug_check_if_ref_count_not_zero<T: RefCountedHandle, U: ObjectContext>(obj: WDFOBJECT) {
    let handle = unsafe { T::from_raw(obj) };
    let ref_count = handle.get_ref_count().load(Ordering::Acquire);
    if ref_count > 0 {
        bug_check(0xDEADDEAD, obj, Some(ref_count));
    }
}

pub(crate) fn bug_check(
    code: u32,
    obj: WDFOBJECT,
    ref_count: Option<usize>,
) {
    let ref_count = ref_count.unwrap_or(0);
    unsafe {
        KeBugCheckEx(
            code,
            obj as u64,
            ref_count as u64,
            0,
            0,
        );
    }
}