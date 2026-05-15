// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Thin wrappers around ETW APIs.
//!
//! When the driver model is KMDF or WDM (kernel mode), binds to the
//! kernel-mode `Etw*` functions. For UMDF (user mode), binds to the
//! `Event*` functions from `onecore_apiset`.

use crate::GUID;

/// ETW event descriptor.
#[repr(C)]
pub struct EVENT_DESCRIPTOR {
    pub Id: u16,
    pub Version: u8,
    pub Channel: u8,
    pub Level: u8,
    pub Opcode: u8,
    pub Task: u16,
    pub Keyword: u64,
}

/// ETW event data descriptor — points to one event field's data.
#[repr(C)]
pub struct EVENT_DATA_DESCRIPTOR {
    pub Ptr: u64,
    pub Size: u32,
    pub Reserved: u32,
}

/// Enable callback function pointer type.
pub type EnableCallback = Option<
    unsafe extern "system" fn(
        source_id: *const GUID,
        control_code: u32,
        level: u8,
        match_any_keyword: u64,
        match_all_keyword: u64,
        filter_data: *const core::ffi::c_void,
        callback_context: *mut core::ffi::c_void,
    ),
>;

// ── FFI declarations ────────────────────────────────────────────────────────

#[cfg(any(driver_model__driver_type = "WDM", driver_model__driver_type = "KMDF"))]
unsafe extern "system" {
    fn EtwRegister(
        ProviderId: *const GUID,
        EnableCallback: EnableCallback,
        CallbackContext: *mut core::ffi::c_void,
        RegHandle: *mut u64,
    ) -> u32;

    fn EtwWriteTransfer(
        RegHandle: u64,
        EventDescriptor: *const EVENT_DESCRIPTOR,
        ActivityId: *const core::ffi::c_void,
        RelatedActivityId: *const core::ffi::c_void,
        UserDataCount: u32,
        UserData: *const EVENT_DATA_DESCRIPTOR,
    ) -> u32;

    fn EtwUnregister(RegHandle: u64) -> u32;

    fn EtwSetInformation(
        RegHandle: u64,
        InformationClass: u32,
        EventInformation: *const core::ffi::c_void,
        InformationLength: u32,
    ) -> u32;
}

#[cfg(driver_model__driver_type = "UMDF")]
#[link(name = "onecore_apiset")]
unsafe extern "system" {
    fn EventRegister(
        ProviderId: *const GUID,
        EnableCallback: EnableCallback,
        CallbackContext: *mut core::ffi::c_void,
        RegHandle: *mut u64,
    ) -> u32;

    fn EventWriteTransfer(
        RegHandle: u64,
        EventDescriptor: *const EVENT_DESCRIPTOR,
        ActivityId: *const core::ffi::c_void,
        RelatedActivityId: *const core::ffi::c_void,
        UserDataCount: u32,
        UserData: *const EVENT_DATA_DESCRIPTOR,
    ) -> u32;

    fn EventUnregister(RegHandle: u64) -> u32;

    fn EventSetInformation(
        RegHandle: u64,
        InformationClass: u32,
        EventInformation: *const core::ffi::c_void,
        InformationLength: u32,
    ) -> u32;
}

// ── Unified aliases ─────────────────────────────────────────────────────────

#[cfg(any(driver_model__driver_type = "WDM", driver_model__driver_type = "KMDF"))]
use self::{
    EtwRegister as event_register,
    EtwSetInformation as event_set_information,
    EtwUnregister as event_unregister,
    EtwWriteTransfer as event_write,
};

#[cfg(driver_model__driver_type = "UMDF")]
use self::{
    EventRegister as event_register,
    EventSetInformation as event_set_information,
    EventUnregister as event_unregister,
    EventWriteTransfer as event_write,
};

// ── Public wrappers ─────────────────────────────────────────────────────────

/// Register an ETW provider.
///
/// # Safety
///
/// `provider_id` must point to a valid GUID. If `callback` is `Some`,
/// `callback_context` must remain valid for the lifetime of the registration.
pub unsafe fn register(
    provider_id: &GUID,
    callback: EnableCallback,
    callback_context: *mut core::ffi::c_void,
) -> (u32, u64) {
    let mut handle: u64 = 0;
    let status = unsafe { event_register(provider_id, callback, callback_context, &mut handle) };
    (status, handle)
}

/// Write an ETW event.
///
/// # Safety
///
/// `handle` must be a valid registration handle. `event_descriptor` and
/// `data` (if non-null) must point to valid memory.
pub unsafe fn write(
    handle: u64,
    event_descriptor: *const EVENT_DESCRIPTOR,
    count: u32,
    data: *const EVENT_DATA_DESCRIPTOR,
) -> u32 {
    unsafe {
        event_write(
            handle,
            event_descriptor,
            core::ptr::null(),
            core::ptr::null(),
            count,
            data,
        )
    }
}

/// Unregister an ETW provider.
///
/// # Safety
///
/// `handle` must be a valid registration handle that has not already
/// been unregistered.
pub unsafe fn unregister(handle: u64) -> u32 {
    unsafe { event_unregister(handle) }
}

/// Set the decode GUID trait on a registered provider.
///
/// # Safety
///
/// `handle` must be a valid registration handle. `decode_guid` must
/// point to a valid GUID.
pub unsafe fn set_decode_guid(handle: u64, decode_guid: &GUID) -> u32 {
    let mut traits = [0u8; 22];
    let blob_size: u16 = 22;
    traits[0..2].copy_from_slice(&blob_size.to_le_bytes());
    traits[2] = 0; // empty provider name
    let trait_size: u16 = 19;
    traits[3..5].copy_from_slice(&trait_size.to_le_bytes());
    traits[5] = 2; // EtwProviderTraitDecodeGuid
    traits[6..10].copy_from_slice(&decode_guid.data1.to_le_bytes());
    traits[10..12].copy_from_slice(&decode_guid.data2.to_le_bytes());
    traits[12..14].copy_from_slice(&decode_guid.data3.to_le_bytes());
    traits[14..22].copy_from_slice(&decode_guid.data4);

    const EVENT_INFO_CLASS_SET_TRAITS: u32 = 2;
    unsafe {
        event_set_information(
            handle,
            EVENT_INFO_CLASS_SET_TRAITS,
            traits.as_ptr() as *const core::ffi::c_void,
            traits.len() as u32,
        )
    }
}
