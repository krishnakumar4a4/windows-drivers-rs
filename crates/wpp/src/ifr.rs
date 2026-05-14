// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0
//! IFR (In-Flight Recorder) support for WPP tracing - types and FFI.

use core::mem;
use core::ptr;

// -- WPP control block types -------------------------------------------------

#[repr(C)]
pub union WPP_PROJECT_CONTROL_BLOCK {
    pub Control: mem::ManuallyDrop<WPP_TRACE_CONTROL_BLOCK>,
    ReserveSpace: [u8; mem::size_of::<WPP_TRACE_CONTROL_BLOCK>()],
}

#[repr(C)]
pub struct WPP_TRACE_CONTROL_BLOCK {
    Callback: Option<unsafe extern "C" fn(u8, *mut core::ffi::c_void, u32, *mut core::ffi::c_void, *mut core::ffi::c_void, *mut u32) -> u64>,
    pub ControlGuid: *const crate::GUID,
    pub Next: *const WPP_TRACE_CONTROL_BLOCK,
    pub Logger: u64,
    RegistryPath: *mut core::ffi::c_void,
    pub FlagsLen: u8,
    pub Level: u8,
    Reserved: u16,
    pub Flags: [u32; 1],
    ReservedFlags: u32,
    pub RegHandle: u64,
    pub AutoLogContext: *mut core::ffi::c_void,
    pub AutoLogVerboseEnabled: u16,
    pub AutoLogAttachToMiniDump: u16,
}

impl WPP_TRACE_CONTROL_BLOCK {
    #[doc(hidden)]
    pub const fn new(flags_len: u8) -> Self {
        Self {
            Callback: None,
            ControlGuid: ptr::null(),
            Next: ptr::null(),
            Logger: 0,
            RegistryPath: ptr::null_mut(),
            FlagsLen: flags_len,
            Level: 0,
            Reserved: 0,
            Flags: [0; 1],
            ReservedFlags: 0,
            RegHandle: 0,
            AutoLogContext: ptr::null_mut(),
            AutoLogVerboseEnabled: 0,
            AutoLogAttachToMiniDump: 0,
        }
    }
}

// -- IFR globals (expected by WinDbg / rcrdrkd) ------------------------------

// NOTE: These are declared in the generated code in the sample driver
// (via the macro) to avoid LTO bitcode issues with #[no_mangle] statics
// in the wpp library crate.

/// Driver object pointer stored for cleanup.
static mut WPP_DRIVER_OBJ: *mut core::ffi::c_void = ptr::null_mut();

// -- FFI ---------------------------------------------------------------------

unsafe extern "C" {
    pub fn WppAutoLogStart(
        WppCb: *mut WPP_PROJECT_CONTROL_BLOCK,
        DrvObj: *mut core::ffi::c_void,
        RegPath: *const core::ffi::c_void,
    );
    pub fn WppAutoLogStop(
        WppCb: *mut WPP_PROJECT_CONTROL_BLOCK,
        DrvObj: *mut core::ffi::c_void,
    );
    #[doc(hidden)]
    pub fn WppAutoLogTrace(
        AutoLogContext: *mut core::ffi::c_void,
        MessageLevel: u8,
        MessageFlags: u32,
        MessageGuid: *mut core::ffi::c_void,
        MessageNumber: u16,
        ...
    ) -> i32;
    pub fn imp_WppRecorderReplay(
        WppCb: *mut core::ffi::c_void,
        WppTraceHandle: u64,
        EnableFlags: u32,
        EnableLevel: u8,
    );
    pub fn MmGetSystemRoutineAddress(
        SystemRoutineName: *mut core::ffi::c_void,
    ) -> *mut core::ffi::c_void;
}

// -- Lifecycle: start / stop --------------------------------------------------

/// Starts IFR auto-log recording and propagates auto-log contexts to provider
/// states.
///
/// This is "phase 2" of IFR init — called *after* the control block array has
/// been created and each provider has set its `ControlGuid` on its control
/// block (inside `Provider::init()`).
///
/// # Safety
///
/// * `control_blocks` must point to a valid linked list of
///   `WPP_PROJECT_CONTROL_BLOCK`.
/// * `driver_obj` and `reg_path` must be valid pointers from DriverEntry.
/// * `provider_states` must match the control blocks in order.
/// * `global_control` and `recorder_initialized` must point to the
///   `#[no_mangle]` statics `WPP_GLOBAL_Control` / `WPP_RECORDER_INITIALIZED`.
#[doc(hidden)]
pub unsafe fn start_ifr(
    control_blocks: *mut WPP_PROJECT_CONTROL_BLOCK,
    driver_obj: *mut core::ffi::c_void,
    reg_path: *const core::ffi::c_void,
    provider_states: &[&crate::ProviderState],
    global_control: *mut *mut WPP_PROJECT_CONTROL_BLOCK,
    recorder_initialized: *mut *mut WPP_PROJECT_CONTROL_BLOCK,
) {
    unsafe {
        WPP_DRIVER_OBJ = driver_obj;
        *global_control = control_blocks;
        WppAutoLogStart(control_blocks, driver_obj, reg_path);
        *recorder_initialized = *global_control;
    }

    // Propagate auto-log contexts to ProviderState
    let primary_ctx = unsafe { (*(*control_blocks).Control).AutoLogContext };
    let mut cb_ptr = control_blocks.cast::<WPP_TRACE_CONTROL_BLOCK>();
    for state in provider_states {
        if cb_ptr.is_null() {
            break;
        }
        unsafe {
            let ctx = if (*cb_ptr).AutoLogContext.is_null() {
                primary_ctx
            } else {
                (*cb_ptr).AutoLogContext
            };
            state
                .auto_log_context
                .store(ctx, core::sync::atomic::Ordering::Release);
            cb_ptr = (*cb_ptr).Next.cast_mut();
        }
    }
}

/// Stops IFR tracing. Called during driver unload.
///
/// # Safety
///
/// `global_control` and `recorder_initialized` must point to the same
/// `#[no_mangle]` statics passed to [`start_ifr`].
#[doc(hidden)]
pub unsafe fn stop_ifr(
    global_control: *mut *mut WPP_PROJECT_CONTROL_BLOCK,
    recorder_initialized: *mut *mut WPP_PROJECT_CONTROL_BLOCK,
) {
    unsafe {
        let cb = *global_control;
        if !cb.is_null() {
            WppAutoLogStop(cb, WPP_DRIVER_OBJ);
            *recorder_initialized = ptr::null_mut();
            *global_control = ptr::null_mut();
            WPP_DRIVER_OBJ = ptr::null_mut();
        }
    }
}
