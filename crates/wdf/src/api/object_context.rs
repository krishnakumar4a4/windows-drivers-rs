// Copyright (c) Microsoft Corporation.
// License: MIT OR Apache-2.0

use core::sync::atomic::{AtomicUsize, Ordering};
use crate::api::{init_attributes, FrameworkHandle, NtResult};
use wdk_sys::{
    call_unsafe_wdf_function_binding, NT_SUCCESS, PCWDF_OBJECT_CONTEXT_TYPE_INFO, WDFOBJECT, WDF_OBJECT_CONTEXT_TYPE_INFO,
    STATUS_INVALID_PARAMETER,
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


#[doc(hidden)]
pub trait RefCount {
    fn get(&self) -> &AtomicUsize;
    fn get_mut(&mut self) -> &mut AtomicUsize;
}

/// Trait that must be implemented by primary context types
trait PrimaryObjectContext {
    fn get_ref_count(&self) -> usize;
    fn increment_ref_count(&mut self);
    fn decrement_ref_count(&mut self);
}

impl<T: RefCount> PrimaryObjectContext for T {
    fn get_ref_count(&self) -> usize {
        // TODO: check if we need Ordering::Acquire here 
        self.get().load(Ordering::Relaxed) as usize
    }

    fn increment_ref_count(&mut self) {
        self.get_mut().fetch_add(1, Ordering::Relaxed);
    }

    fn decrement_ref_count(&mut self) {
        // TODO: check if we need Ordering::Release here 
        self.get_mut().fetch_sub(1, Ordering::Relaxed);
    }
}

// Smallest possible alignment of allocations made by
// WDF on 64 bit systems is 16 which comes form ExAllocatePool2
// or HeapAlloc which are the two functions used by WDF.
const MIN_FRAMEWORK_ALIGNMENT_ON_64_BIT: usize = 16;

pub unsafe fn attach_context<T: FrameworkHandle, U: Sync>(
    fw_obj: &mut T,
    context: U,
    context_type_info: &'static WdfObjectContextTypeInfo,
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
    attributes.ContextTypeInfo = context_type_info.get_unique_type();
    attributes.EvtCleanupCallback = Some(cleanup_callback);
    if destroy_callback.is_some() {
        attributes.EvtDestroyCallback = destroy_callback;
    }

    let mut wdf_context: *mut U = core::ptr::null_mut();
    let status = unsafe {
        call_unsafe_wdf_function_binding!(
            WdfObjectAllocateContext,
            fw_obj.as_ptr(),
            &mut attributes,
            core::mem::transmute(&mut wdf_context),
        )
    };

    if !NT_SUCCESS(status) {
        return Err(status.into());
    }

    unsafe {
        // TODO: Check if we're violating any alignment expectations
        // when we write this struct to C allocated pointer.
        // Most likely we are!!
        core::ptr::write(wdf_context, context);
    }

    Ok(())
}

pub fn get_context<'a, T: FrameworkHandle, U: Sync>(
    fw_obj: &'a T,
    context_metadata: &'static WdfObjectContextTypeInfo,
) -> Option<&'a U> {
    let state = unsafe {
        call_unsafe_wdf_function_binding!(
            WdfObjectGetTypedContextWorker,
            fw_obj.as_ptr(),
            &context_metadata.0
        ) as *mut U
    };

    if !state.is_null() {
        Some(unsafe { &*state })
    } else {
        None
    }
}

pub unsafe fn drop_context<U: Sync>(
    fw_obj: WDFOBJECT,
    context_metadata: &'static WdfObjectContextTypeInfo,
) {
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
pub fn _bugcheck_if_ref_count_not_zero<T: FrameworkHandle, U: PrimaryObjectContext + Sync>(
    fw_obj: WDFOBJECT,
    context_metadata: &'static WdfObjectContextTypeInfo,
) {
    let fw_handle = unsafe { T::from_ptr(fw_obj) };
    if let Some(context) = get_context::<T, U>(&fw_handle, context_metadata) {
        if context.get_ref_count() != 0 {
            // Todo: Bug check here
        }
    }
}