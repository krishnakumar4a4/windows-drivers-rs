// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0
//! IFR (In-Flight Recorder) support for WPP tracing - types, FFI and state.

use core::mem;
use core::ptr;
use core::sync::atomic::{AtomicPtr, AtomicU8, Ordering};

// -- IFR lifecycle states ----------------------------------------------------

pub const UNINITIALIZED: u8 = 0;
pub const INITIALIZING: u8 = 1;
pub const INITIALIZED: u8 = 2;

// -- IFR state ---------------------------------------------------------------

/// Runtime state for IFR (In-Flight Recorder).
///
/// Holds the auto-log context pointer set by `start_ifr` and used by
/// `WppAutoLogTrace` to record trace events into the IFR circular buffer.
pub struct IFRState {
    pub auto_log_context: AtomicPtr<core::ffi::c_void>,
    pub init_state: AtomicU8,
}

// SAFETY: All fields are atomic — concurrent access is safe.
unsafe impl Sync for IFRState {}

impl IFRState {
    pub const fn new() -> Self {
        Self {
            auto_log_context: AtomicPtr::new(ptr::null_mut()),
            init_state: AtomicU8::new(UNINITIALIZED),
        }
    }

    /// Returns the IFR auto-log context pointer.
    #[doc(hidden)]
    #[inline]
    pub fn auto_log_context(&self) -> *mut core::ffi::c_void {
        self.auto_log_context.load(Ordering::Relaxed)
    }
}

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

/// Starts IFR auto-log recording and stores the auto-log context in `IFRState`.
///
/// Control blocks must already have their `ControlGuid` and `Next` pointers
/// set before calling this function.
///
/// # Safety
///
/// * `control_blocks` must point to a valid linked list of
///   `WPP_PROJECT_CONTROL_BLOCK` with `ControlGuid` set on each block.
/// * `driver_obj` and `reg_path` must be valid pointers from DriverEntry.
/// * `ifr_state` must point to the IFR module's static `IFRState`.
/// * `global_control` and `recorder_initialized` must point to the
///   `#[no_mangle]` statics `WPP_GLOBAL_Control` / `WPP_RECORDER_INITIALIZED`.
#[doc(hidden)]
pub unsafe fn start_ifr(
    control_blocks: *mut WPP_PROJECT_CONTROL_BLOCK,
    driver_obj: *mut core::ffi::c_void,
    reg_path: *const core::ffi::c_void,
    ifr_state: &IFRState,
    global_control: *mut *mut WPP_PROJECT_CONTROL_BLOCK,
    recorder_initialized: *mut *mut WPP_PROJECT_CONTROL_BLOCK,
) {
    unsafe {
        WPP_DRIVER_OBJ = driver_obj;
        *global_control = control_blocks;
        WppAutoLogStart(control_blocks, driver_obj, reg_path);
        *recorder_initialized = *global_control;
    }

    // Store auto-log context from the primary control block into IFRState
    let ctx = unsafe { (*(*control_blocks).Control).AutoLogContext };
    ifr_state
        .auto_log_context
        .store(ctx, Ordering::Release);
}

/// Stops IFR tracing. Called during driver unload.
///
/// # Safety
///
/// `global_control` and `recorder_initialized` must point to the same
/// `#[no_mangle]` statics passed to [`start_ifr`].
#[doc(hidden)]
pub unsafe fn stop_ifr(
    ifr_state: &IFRState,
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
    ifr_state
        .auto_log_context
        .store(ptr::null_mut(), Ordering::Release);
}
