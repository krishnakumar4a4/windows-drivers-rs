use core::{ptr::null_mut, sync::atomic::AtomicUsize, time::Duration};

use wdf_macros::object_context_with_ref_count_check;
use wdk_sys::{call_unsafe_wdf_function_binding, WDFDEVICE, WDFTIMER, WDF_TIMER_CONFIG};

use super::{
    device::Device,
    init_wdf_struct,
    object::{impl_ref_counted_handle, init_attributes, GetDevice, Handle},
    result::{NtResult, StatusCodeExt},
    sync::Arc,
};

// TODO: Make timer more ergonomic and safer. It's
// not fully safe yet. For example it lets you pass
// a negative value for due time to start when
// use_high_resolution_timer is set to true which would
// crash the system.

impl_ref_counted_handle!(Timer, TimerContext);

impl Timer {
    pub fn create<'a, P: Handle>(config: &TimerConfig<'a, P>) -> NtResult<Arc<Self>> {
        let context = TimerContext {
            ref_count: AtomicUsize::new(0),
            evt_timer_func: config.evt_timer_func,
        };

        let mut timer: WDFTIMER = null_mut();

        let mut attributes = init_attributes();
        attributes.ParentObject = config.parent.as_ptr();

        let mut config: WDF_TIMER_CONFIG = config.into();

        // SAFETY: The resulting ffi object is stored in a private member and not
        // accessible outside of this module, and this module guarantees that it is
        // always in a valid state.
        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfTimerCreate,
                &mut config,
                &mut attributes,
                &mut timer,
            )
        }
        .and_then(|| {
            TimerContext::attach(unsafe { &*timer.cast() }, context)?;
            let timer = unsafe { Arc::from_raw(timer.cast()) };

            Ok(timer)
        })
    }

    // TODO: takes &self instead of &mut self because right now
    // we don't have a good design for representation thread safey
    // of WDF objects to the driver code. So we're using &self for
    // the moment as it lets us put the object in the object context.
    // When we have a good design for thread safe reprensetation we
    // will change it back to &mut self
    // TODO: also support absolute time in addition to duration
    pub fn start(&self, duration: &Duration) -> bool {
        let due_time = -1 * duration.as_nanos() as i64 / 100; // To ticks. -1 is for relative time

        // TODO: use something like duration instead of i64 for due_time
        unsafe {
            call_unsafe_wdf_function_binding!(WdfTimerStart, self.as_ptr().cast(), due_time) != 0
        }
    }

    // TODO: Change to &mut self. See comment on start() method
    pub fn stop(&self, wait: bool) -> bool {
        unsafe {
            call_unsafe_wdf_function_binding!(WdfTimerStop, self.as_ptr().cast(), wait as u8) != 0
        }
    }

    pub fn get_device(&self) -> &Device {
        self.get_device_safely()
    }
}

impl GetDevice for Timer {
    fn get_device_ptr(&self) -> WDFDEVICE {
        let device_ptr = unsafe {
            call_unsafe_wdf_function_binding!(WdfTimerGetParentObject, self.as_ptr().cast())
                as WDFDEVICE
        };

        if device_ptr.is_null() {
            panic!("Timer has no parent device");
        }

        device_ptr
    }
}

pub struct TimerConfig<'a, P: Handle> {
    pub evt_timer_func: fn(&Timer),
    pub period: u32,
    pub tolerable_delay: u32,
    pub use_high_resolution_timer: bool,
    pub parent: &'a P,
}

impl<'a, P: Handle> TimerConfig<'a, P> {
    pub fn new_non_periodic(parent: &'a P, evt_timer_func: fn(&Timer)) -> Self {
        Self {
            evt_timer_func,
            period: 0,
            tolerable_delay: 0,
            use_high_resolution_timer: false,
            parent: &parent,
        }
    }

    pub fn new_periodic(
        parent: &'a P,
        evt_timer_func: fn(&Timer),
        period: u32,
        tolerable_delay: u32,
        use_high_resolution_timer: bool,
    ) -> Self {
        Self {
            evt_timer_func,
            period,
            tolerable_delay,
            use_high_resolution_timer,
            parent: &parent,
        }
    }
}

impl<'a, P: Handle> From<&TimerConfig<'a, P>> for WDF_TIMER_CONFIG {
    fn from(config: &TimerConfig<'a, P>) -> Self {
        let mut wdf_config = init_wdf_struct!(WDF_TIMER_CONFIG);
        wdf_config.Period = config.period;
        wdf_config.AutomaticSerialization = 0;
        wdf_config.TolerableDelay = config.tolerable_delay;
        wdf_config.UseHighResolutionTimer = config.use_high_resolution_timer as u8;
        wdf_config.EvtTimerFunc = Some(__evt_timer_func);

        wdf_config
    }
}

#[object_context_with_ref_count_check(Timer)]
struct TimerContext {
    ref_count: AtomicUsize,
    evt_timer_func: fn(&Timer),
}

pub extern "C" fn __evt_timer_func(timer: WDFTIMER) {
    let timer = unsafe { &*timer.cast::<Timer>() };
    let timer_state = TimerContext::get(&timer);
    (timer_state.evt_timer_func)(timer);
}
