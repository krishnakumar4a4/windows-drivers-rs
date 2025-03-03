use crate::api::{error::NtResult, object::{wdf_struct_size, WdfObject}};
use wdf_macros::object_context;
use wdk_sys::{call_unsafe_wdf_function_binding, NT_SUCCESS, WDFTIMER, WDF_OBJECT_ATTRIBUTES, WDF_TIMER_CONFIG};
use core::{mem::MaybeUninit, ptr::null_mut};

// TODO: Make timer more ergonomic and safer. It's
// not fully safe yet. For example it lets you pass
// a negative value for due time to start when
// use_high_resolution_timer is set to true which would
// crash the system.

/// A WDF timer
pub struct Timer(WDFTIMER);

impl Timer {
    pub(crate) unsafe fn new(inner: WDFTIMER) -> Self {
        Self(inner)
    }

    pub fn create<'a, P: WdfObject>(config: &TimerConfig<'a, P>) -> NtResult<Self> {
        let context = TimerContext {
            evt_timer_func: config.evt_timer_func,
        };

        let mut timer : WDFTIMER = null_mut();

        let mut attributes = WDF_OBJECT_ATTRIBUTES::default();
        attributes.ParentObject = config.parent.as_ptr();

        let mut config:  WDF_TIMER_CONFIG = config.into();

        // SAFETY: The resulting ffi object is stored in a private member and not
        // accessible outside of this module, and this module guarantees that it is
        // always in a valid state.
        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfTimerCreate,
                &mut config as *mut _,
                &mut attributes as *mut _,
                &mut timer as *mut _,
            )
        };

        if NT_SUCCESS(status) {
            let mut timer = unsafe { Timer::new(timer) };

            TimerContext::attach(&mut timer, context)?;

            Ok(timer)
        } else {
            Err(status.into())
        }
    }

    pub fn start(&self, due_time: i64) { // TODO: use something like duration instead of i64 for due_time
        unsafe {
            call_unsafe_wdf_function_binding!(WdfTimerStart, self.0, due_time);
        }
    }

    pub fn stop(&self, wait: bool) -> bool {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfTimerStop, self.0, wait as u8) != 0
        }
    }
}

impl WdfObject for Timer {
    fn as_ptr(&self) -> *mut core::ffi::c_void {
        self.0 as *mut _
    }
}

/// SAFETY: This is safe because all the WDF functions
/// that operate on WDFTIMER do so in a thread-safe manner.
/// As a result, all the Rust methods on this struct are
/// also thread-safe.
unsafe impl Send for Timer {}
unsafe impl Sync for Timer {}

pub struct TimerConfig<'a, P: WdfObject> {
    pub evt_timer_func: fn(&mut Timer),
    pub period: u32,
    pub tolerable_delay: u32,
    pub use_high_resolution_timer: bool,
    pub parent: &'a P
}

impl<'a, P: WdfObject> TimerConfig<'a, P> {
    pub fn new_non_periodic(parent: &'a P, evt_timer_func: fn(&mut Timer)) -> Self {
        Self {
            evt_timer_func,
            period: 0,
            tolerable_delay: 0,
            use_high_resolution_timer: false,
            parent: &parent
        }
    }

    pub fn new_periodic(parent: &'a P, evt_timer_func: fn(&mut Timer), period: u32, tolerable_delay: u32, use_high_resolution_timer: bool) -> Self {
        Self {
            evt_timer_func,
            period,
            tolerable_delay,
            use_high_resolution_timer,
            parent: &parent
        }
    }
}

impl<'a, P: WdfObject> From<&TimerConfig<'a, P>> for WDF_TIMER_CONFIG {
    fn from(config: &TimerConfig<'a, P>) -> Self {
        let mut wdf_config: WDF_TIMER_CONFIG = unsafe {
            MaybeUninit::zeroed().assume_init()
        };

        wdf_config.Size = wdf_struct_size!(WDF_TIMER_CONFIG);
        wdf_config.Period = config.period;
        wdf_config.AutomaticSerialization = 0;
        wdf_config.TolerableDelay = config.tolerable_delay;
        wdf_config.UseHighResolutionTimer = config.use_high_resolution_timer as u8;
        wdf_config.EvtTimerFunc = Some(__evt_timer_func);

        wdf_config
    }
}

#[object_context(Timer)]
struct TimerContext {
    evt_timer_func: fn(&mut Timer)
}

pub extern "C" fn __evt_timer_func(timer: WDFTIMER) {
    let mut timer = unsafe { Timer::new(timer) };
    if let Some(timer_state) = TimerContext::get(&timer) {
        (timer_state.evt_timer_func)(&mut timer);
    }
}