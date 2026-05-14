// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Runtime state for ETW providers.

use core::sync::atomic::{AtomicPtr, AtomicU64, AtomicU8, Ordering};

pub const UNINITIALIZED: u8 = 0;
pub const INITIALIZING: u8 = 1;
pub const INITIALIZED: u8 = 2;

/// Runtime state for a single ETW provider.
///
/// Holds the registration handle and the currently-enabled level/keywords,
/// updated atomically by the ETW enable callback.
///
/// The `init_state` field tracks the lifecycle:
/// UNINITIALIZED → INITIALIZING → INITIALIZED.
/// This prevents double-registration and ensures safe concurrent access
/// between `init()`, `clean_up()`, and `trace!` calls.
pub struct ProviderState {
    pub reg_handle: AtomicU64,
    pub enabled_level: AtomicU8,
    pub enabled_keywords: AtomicU64,
    pub init_state: AtomicU8,
    /// IFR auto-log context pointer, set by `writer::init_tracing`.
    pub auto_log_context: AtomicPtr<core::ffi::c_void>,
}

// SAFETY: All fields are atomic — concurrent access is safe.
unsafe impl Sync for ProviderState {}

impl ProviderState {
    pub const fn new() -> Self {
        Self {
            reg_handle: AtomicU64::new(0),
            enabled_level: AtomicU8::new(0),
            enabled_keywords: AtomicU64::new(0),
            init_state: AtomicU8::new(UNINITIALIZED),
            auto_log_context: AtomicPtr::new(core::ptr::null_mut()),
        }
    }

    /// Fast-path check: returns `true` only if the provider is currently
    /// enabled at the given level and keyword by a trace controller.
    #[inline]
    pub fn is_enabled(&self, level: u8, keyword: u64) -> bool {
        let cur_level = self.enabled_level.load(Ordering::Relaxed);
        if cur_level == 0 || level > cur_level {
            return false;
        }
        let cur_kw = self.enabled_keywords.load(Ordering::Relaxed);
        keyword == 0 || (keyword & cur_kw) != 0
    }

    #[inline]
    pub fn reg_handle(&self) -> u64 {
        self.reg_handle.load(Ordering::Relaxed)
    }

    /// Returns the IFR auto-log context pointer.
    #[doc(hidden)]
    #[inline]
    pub fn auto_log_context(&self) -> *mut core::ffi::c_void {
        self.auto_log_context.load(Ordering::Relaxed)
    }
}

/// ETW enable callback invoked by the kernel when a trace controller
/// enables or disables this provider.
///
/// # Safety
///
/// Must only be passed to `EtwRegister` as the enable callback.
/// `callback_context` must point to a valid `ProviderState`.
pub unsafe extern "system" fn enable_callback(
    _source_id: *const crate::GUID,
    control_code: u32,
    level: u8,
    match_any_keyword: u64,
    _match_all_keyword: u64,
    _filter_data: *const core::ffi::c_void,
    callback_context: *mut core::ffi::c_void,
) {
    if callback_context.is_null() {
        return;
    }
    let state = unsafe { &*(callback_context as *const ProviderState) };

    const DISABLE: u32 = 0;
    const ENABLE: u32 = 1;

    match control_code {
        DISABLE => {
            state.enabled_level.store(0, Ordering::Relaxed);
            state.enabled_keywords.store(0, Ordering::Relaxed);
        }
        ENABLE => {
            state.enabled_level.store(level, Ordering::Relaxed);
            state
                .enabled_keywords
                .store(match_any_keyword, Ordering::Relaxed);
        }
        _ => {}
    }
}
