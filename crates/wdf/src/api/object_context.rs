// Copyright (c) Microsoft Corporation.
// License: MIT OR Apache-2.0

use wdk_sys::{NT_SUCCESS, PCWDF_OBJECT_CONTEXT_TYPE_INFO, WDF_OBJECT_ATTRIBUTES, WDF_OBJECT_CONTEXT_TYPE_INFO, WDFOBJECT, call_unsafe_wdf_function_binding};
use crate::api::{NtResult, FrameworkObject};

#[doc(hidden)]
#[repr(transparent)]
pub struct WdfObjectContextTypeInfo(WDF_OBJECT_CONTEXT_TYPE_INFO);

/// SAFETY: This type is NOT safe to send across threads
/// but it never is not supposed to be used by the end user.
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

pub struct ObjectContext;

impl ObjectContext {
    pub unsafe fn attach<T: FrameworkObject, U: Sync>(wdf_obj: &mut T, context: U, context_metadata: &'static WdfObjectContextTypeInfo, cleanup_callback: unsafe extern "C" fn(WDFOBJECT)) -> NtResult<()> {
        let mut attributes = WDF_OBJECT_ATTRIBUTES::default();
        attributes.ContextTypeInfo = context_metadata.get_unique_type();
        attributes.EvtCleanupCallback = Some(cleanup_callback);

        let mut wdf_context: *mut U = core::ptr::null_mut();
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfObjectAllocateContext,
                wdf_obj as *const _ as WDFOBJECT,
                &mut attributes as *mut _,
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

    pub fn get<'a, T: FrameworkObject, U: Sync>(wdf_obj: &'a T, context_metadata: &'static WdfObjectContextTypeInfo) -> Option<&'a U> {
         let state = unsafe {
             call_unsafe_wdf_function_binding!(WdfObjectGetTypedContextWorker, wdf_obj.as_ptr(), &context_metadata.0 as *const WDF_OBJECT_CONTEXT_TYPE_INFO) as *mut U
        };

        if !state.is_null() {
            Some(unsafe { &*state })
        } else {
            None
        }
    }

    pub unsafe fn drop<U: Sync>(wdf_obj: WDFOBJECT, context_metadata: &'static WdfObjectContextTypeInfo) {
        let context =
        unsafe {
            call_unsafe_wdf_function_binding!(WdfObjectGetTypedContextWorker, wdf_obj, &context_metadata.0 as *const WDF_OBJECT_CONTEXT_TYPE_INFO) as *mut core::mem::ManuallyDrop<U>
        };

        if !context.is_null() {
            unsafe {
                core::mem::ManuallyDrop::drop(&mut *context);
            }
        }
    }
}