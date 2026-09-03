// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Thin wrappers around ETW APIs.
//!
//! When the `kernel_mode` feature is enabled, binds to the kernel-mode
//! `Etw*` functions. Otherwise, binds to the user-mode `Event*` functions
//! from `onecore_apiset`.

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

/// `EVENT_DATA_DESCRIPTOR.Type` value marking a descriptor as the per-event
/// decode GUID (RS name: `EVENT_DATA_DESCRIPTOR_TYPE_DECODE_GUID`; servicing
/// name: `EVENT_DATA_DESCRIPTOR_TYPE_RESERVED1`).
///
/// The `Type` field is the low byte of the `Reserved` union, so setting the
/// whole `Reserved` word to this value tags the descriptor's `Ptr`/`Size` as a
/// `GUID` used for TMF decode lookup rather than as event payload. Requires the
/// provider to have opted in via [`enable_modern_wpp`] (which sets
/// `EventProviderUseDescriptorType`).
pub const EVENT_DATA_DESCRIPTOR_TYPE_RESERVED1: u32 = 4;

/// Enable callback function pointer type.
type EnableCallback = Option<
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

#[cfg(feature = "kernel_mode")]
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
        ActivityId: Option<&[u8; 16]>,
        RelatedActivityId: Option<&[u8; 16]>,
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

#[cfg(not(feature = "kernel_mode"))]
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
        ActivityId: Option<&[u8; 16]>,
        RelatedActivityId: Option<&[u8; 16]>,
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

#[cfg(feature = "kernel_mode")]
use self::{
    EtwRegister as event_register,
    EtwSetInformation as event_set_information,
    EtwUnregister as event_unregister,
    EtwWriteTransfer as event_write,
};
#[cfg(not(feature = "kernel_mode"))]
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
    unsafe { event_write(handle, event_descriptor, None, None, count, data) }
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

/// Marks a registered provider as a ModernWpp (WPPv3) trace-message provider.
///
/// This corresponds to `EtwSetInformation(RegHandle, EventProviderSetReserved2,
/// NULL, 0)`. It requests that the kernel:
///
/// * stamp `EVENT_HEADER_FLAG_RESERVED1` on every event written by this
///   provider, so the trace decoder routes the (Crimson) events to the WPP/TMF
///   decode path, and
/// * track the binary's **DebugId** (PDB signature) so the decoder can locate
///   the PDB that carries the WPPv1 `TMF:`/`TMC:` annotations.
///
/// It additionally sets `EventProviderUseDescriptorType`, so that the kernel
/// honours `EVENT_DATA_DESCRIPTOR.Type`. This is required for the per-module
/// decode-GUID model (Option B): each event prepends a descriptor of type
/// [`EVENT_DATA_DESCRIPTOR_TYPE_RESERVED1`] carrying the module's decode GUID,
/// which the kernel copies into `EventHeader.ProviderId` (with
/// `EVENT_HEADER_FLAG_DECODE_GUID`). The decoder then looks up that module's
/// TMF under the decode GUID — decoupled from the provider control GUID used
/// for registration and enablement.
///
/// This is best-effort: on kernels without `Feature_ModernWpp`, the call
/// returns a failure status (e.g. `STATUS_INVALID_DEVICE_REQUEST`) and the
/// caller should ignore it — the driver still loads, only PDB/TMF decoding of
/// these events is unavailable. The returned status is that of the primary
/// `EventProviderSetReserved2` call.
///
/// # Safety
///
/// `handle` must be a valid registration handle that has not been unregistered.
pub unsafe fn enable_modern_wpp(handle: u64) -> u32 {
    // EVENT_INFO_CLASS::EventProviderSetReserved2 (a.k.a.
    // EventProviderTraceMessage). Not present in the public SDK enum (which
    // ends at EventProviderUseDescriptorType = 3 / MaxEventInfo = 4); defined
    // by the ModernWpp-capable OS.
    const EVENT_INFO_CLASS_SET_RESERVED2: u32 = 4;
    // EVENT_INFO_CLASS::EventProviderUseDescriptorType — honour
    // `EVENT_DATA_DESCRIPTOR.Type` so the leading RESERVED1 descriptor is
    // treated as the decode GUID (metadata) instead of event payload.
    const EVENT_INFO_CLASS_USE_DESCRIPTOR_TYPE: u32 = 3;

    let status = unsafe {
        event_set_information(handle, EVENT_INFO_CLASS_SET_RESERVED2, core::ptr::null(), 0)
    };

    let use_descriptor_type: u8 = 1; // BOOLEAN TRUE
    let _ = unsafe {
        event_set_information(
            handle,
            EVENT_INFO_CLASS_USE_DESCRIPTOR_TYPE,
            &use_descriptor_type as *const u8 as *const core::ffi::c_void,
            core::mem::size_of::<u8>() as u32,
        )
    };

    status
}
