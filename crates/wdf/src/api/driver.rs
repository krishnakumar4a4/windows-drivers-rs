extern crate alloc;

use alloc::string::String;
use core::{cell::UnsafeCell, mem, ptr};

use wdk_sys::{
    call_unsafe_wdf_function_binding,
    PDRIVER_OBJECT,
    ULONG,
    UNICODE_STRING,
    WDFDEVICE_INIT,
    WDFDRIVER,
    WDF_DRIVER_CONFIG,
    WDF_DRIVER_VERSION_AVAILABLE_PARAMS,
    WDF_NO_OBJECT_ATTRIBUTES,
};
#[doc(hidden)]
pub use wdk_sys::{
    DRIVER_OBJECT,
    NTSTATUS,
    NT_SUCCESS,
    PCUNICODE_STRING,
    WDFOBJECT,
    WDF_OBJECT_ATTRIBUTES,
    WDF_OBJECT_CONTEXT_TYPE_INFO,
};

use crate::api::{
    error::NtResult,
    guid::Guid,
    object::wdf_struct_size,
    string::{to_rust_str, WString},
    tracing::TraceWriter,
    *,
};

static TRACE_WRITER: UnsafeOnceCell<TraceWriter> = UnsafeOnceCell::new();
static EVT_DEVICE_ADD: UnsafeOnceCell<fn(&mut DeviceInit) -> Result<(), NtError>> =
    UnsafeOnceCell::new();

/// Represents a driver
pub struct Driver {
    wdm_driver: PDRIVER_OBJECT,
    wdf_driver: WDFDRIVER,
    reg_path: PCUNICODE_STRING,
}

impl Driver {
    pub fn retrieve_version_string(&self) -> NtResult<String> {
        let string = WString::create()?;

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDriverRetrieveVersionString,
                self.wdf_driver,
                string.as_ptr() as *mut _,
            )
        };

        if !NT_SUCCESS(status) {
            return Err(status.into());
        }

        Ok(string.to_rust_string())
    }

    pub fn is_version_available(&self, major_vesion: u32, minor_version: u32) -> bool {
        let mut params = WDF_DRIVER_VERSION_AVAILABLE_PARAMS {
            Size: wdf_struct_size!(WDF_DRIVER_VERSION_AVAILABLE_PARAMS),
            MajorVersion: major_vesion,
            MinorVersion: minor_version,
        };

        let res = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDriverIsVersionAvailable,
                self.wdf_driver,
                &raw mut params,
            )
        };

        res != 0
    }

    /// Registers a callback for the `EvtDeviceAdd` event
    pub fn on_evt_device_add(
        &mut self,
        callback: fn(&mut DeviceInit) -> Result<(), NtError>,
    ) -> NtResult<()> {
        // SAFETY: `on_evt_device_add` is called only from the user's
        // driver entry function which is guaranteed to run before
        // any other driver code. Therefore the `set()`
        // call below cannot run concurrently with another `set()`
        // or `get()` call which is the invariant we must uphold.
        // In fact no other function even calls `EVT_DEVICE_ADD.set()`
        // except this one which strengthens the safety guarantee further.
        // 
        // TODO: There is still one soundness hole however: what if
        // the user's driver entry function spawns one or more threads
        // that call `on_evt_device_add`? We must plug that hole
        unsafe {
            EVT_DEVICE_ADD.set(callback)?;
        }

        Ok(())
    }

    /// Enables tracing for the driver
    pub fn enable_tracing(&mut self, control_guid: Guid) -> NtResult<()> {
        // SAFETY: `enable_tracing` is called only from the user's
        // driver entry function which is guaranteed to run before
        // any other driver code. Therefore the `get()`
        // call below cannot run concurrently with a`set()`
        // call which is the invariant we must uphold.
        // 
        // TODO: There is still one soundness hole however: what if
        // the user's driver entry function spawns one or more threads
        // that call `enable-tracing`? We must plug that hole
        unsafe {
            // We want to make sure we init WPP ONLY if it hasn't 
            // already been initialized. That's why we check first
            // befor calling `set`
            if TRACE_WRITER.get().is_some() {
                return Err(NtError::from(1)); // TODO: Change this to a proper error
                                            // code
            }
        }
        let trace_writer =
            unsafe { TraceWriter::init(control_guid, self.wdm_driver, self.reg_path) };

        trace_writer.start();

        // SAFETY: `enable_tracing` is called only from the user's
        // driver entry function which is guaranteed to run before
        // any other driver code. Therefore the `set()` call below
        // cannot run concurrently with another `set()` or `get()`
        // call which is the invariant we must uphold.
        // 
        // TODO: this too suffers from the same soundness hole as
        // above.
        unsafe {
            TRACE_WRITER.set(trace_writer).expect("trace writer should not be already set");
        }

        Ok(())
    }
}

/// A container like [`core::cell::OnceCell`] that is 
/// set once and read multiple times.
/// 
/// # Safety
/// The reason this type has the prefix `Unsafe` in its name
/// is because it is not thread safe. To use it safely
/// the user must uphold the following invariants:
/// - The [`set`] method must not be called concurrently
///   with itself or the [`get`] method.
/// - The `get` method must not be called concurrently
///   with the `set` method.
/// 
/// The typical pattern is to call `set` first from
/// a single thread and initialize the value, and then
/// call `get` from multiple threads as needed.
/// 
/// `UnsafeOnceCell` implements `Sync` but only to allow it to
/// be used in certain static variables in this module. In
/// reality it is not `Sync` under all conditions. It is `Sync`
/// only when the above-mentioned invariants are maintained.
/// Therefore **do not use it in contexts which assume/require
/// that it is always `Sync`**.
/// 
/// In general `UnsafeOnceCell` is NOT a general-purpose type.
/// It is meant to be used only in the way it is used right
/// now wherein instances of it are placed in static variables,
/// the driver entry function -- and only the driver entry
/// function -- calls `set` and other methods in this module
/// call `get` only after driver entry is done. Therefore
/// **please do not use it for any other purpose and be careful
/// when changing its code or any of the code that uses it**.
///
/// We could have made this type thread-safe and avoid all of
/// the above constraitns but that would have meant that every
/// access to it requires an atomic operation which is bad for
/// performance because values stored in it are meant to be
/// accessed very frequently such as from tracing calls.
struct UnsafeOnceCell<T> {
    val: UnsafeCell<Option<T>>,
}

impl<T> UnsafeOnceCell<T> {
    /// Creates a new `UnsafeOnceCell` instance
    pub const fn new() -> Self {
        Self {
            val: UnsafeCell::new(None),
        }
    }

    /// Returns a reference to the value.
    /// 
    /// # Safety
    /// This method will causes data races if
    /// called concurrently with the [`set`]
    /// method. It is safe to be called
    /// concurrently with itself however.
    pub unsafe fn get(&self) -> Option<&T> {
        // SAFETY: Safe because we assume that the call to this method
        // is not concurrent with the `set` method. This is true
        let val_ref = unsafe { &*self.val.get() };
        val_ref.as_ref()
    }

    /// Sets the value if it has not been already set
    /// 
    /// # Returns
    /// Returns `Ok(())` if the value was set successfully,
    /// or an `Err(NtError)` if it was already set.
    /// 
    /// # Safety
    /// This method will cause data races if called
    /// concurrently with itself or the [`get`] method. 
    pub unsafe fn set(&self, val: T) -> NtResult<()> {
        // SAFETY: Safe because we assume that the call to this method
        // is not concurrent with itself or the `get` method.
        unsafe {
            let val_ptr = self.val.get();
            if (*val_ptr).is_some() {
                return Err(NtError::from(1)); // TODO: Change this to a proper error
            }
            *val_ptr = Some(val);
        };
        Ok(())
    }
}

/// This type is `Sync` if `T` is `Sync` AND if the 
/// safety invariants stated on the `UnsafeOnceCell` type
/// are upheld. Ideally we should not have implemented `Sync`
/// for it, but we had to to make it usable in static variables
unsafe impl<T> Sync for UnsafeOnceCell<T> where T: Sync {}



fn clean_up_tracing() {
    if let Some(trace_writer) = 
    // SAFETY: This is safe because this call to `get`
    // is not concurrent with any call to `set`. `set` is
    // called only once in the beginning in the driver entry
    // function
    unsafe {TRACE_WRITER.get() } {
        trace_writer.stop();
    }
}

/// Calls the safe driver entry function
///
/// It is meant to be called by the driver entry function generated
/// in the user's driver code by the `driver_entry` procedural
/// macro attribute
#[doc(hidden)]
pub fn call_safe_driver_entry(
    wdm_driver: &mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
    safe_entry: fn(&mut Driver, &str) -> Result<(), NtError>,
) -> NTSTATUS {
    wdm_driver.DriverUnload = Some(driver_unload);

    let mut driver_config = WDF_DRIVER_CONFIG {
        Size: mem::size_of::<WDF_DRIVER_CONFIG>() as ULONG,
        EvtDriverDeviceAdd: Some(evt_driver_device_add),
        ..WDF_DRIVER_CONFIG::default()
    };

    let mut wdf_driver: WDFDRIVER = ptr::null_mut();

    let nt_status = unsafe {
        call_unsafe_wdf_function_binding!(
            WdfDriverCreate,
            wdm_driver,
            reg_path,
            WDF_NO_OBJECT_ATTRIBUTES,
            &mut driver_config,
            &raw mut wdf_driver,
        )
    };

    if !NT_SUCCESS(nt_status) {
        return nt_status;
    }

    let mut safe_driver = Driver {
        wdm_driver,
        wdf_driver,
        reg_path,
    };

    // Translate UTF16 string to rust string
    let reg_path: UNICODE_STRING =
        // SAFETY: This dereference is safe since `registry_path` is:
        //         * provided by `DriverEntry` and is never null
        //         * a valid pointer to a `UNICODE_STRING`
        unsafe { *reg_path };

    let reg_path = to_rust_str(reg_path);
    match safe_entry(&mut safe_driver, &reg_path) {
        Ok(_) => NtStatus::Success.into(),
        Err(e) => {
            clean_up_tracing();
            e.nt_status()
        }
    }
}

#[link_section = "PAGE"]
extern "C" fn evt_driver_device_add(
    _driver: WDFDRIVER,
    device_init: *mut WDFDEVICE_INIT,
) -> NTSTATUS {
    if let Some(cb) = 
    // SAFETY: This is safe because this call to `get`
    // is not concurrent with any call to `set`. `set` is
    // called only once in the beginning in the user's
    // driver entry function
    unsafe { EVT_DEVICE_ADD.get() } {
        let mut device_init = unsafe { DeviceInit::from(device_init) };
        match cb(&mut device_init) {
            Ok(_) => 0,
            Err(e) => e.nt_status(),
        }
    } else {
        0
    }
}

extern "C" fn driver_unload(_driver: *mut DRIVER_OBJECT) {
    println!("Driver unload");

    clean_up_tracing();

    println!("Driver unload done");
}

pub fn trace(message: &str) {
    // SAFETY: This is safe because this call to `get`
    // is not concurrent with any call to `set`. `set` is
    // called only once in the beginning in the user's
    // driver entry function
    unsafe {
        if let Some(trace_writer) = TRACE_WRITER.get() {
            trace_writer.write(message);
        }
    }
}