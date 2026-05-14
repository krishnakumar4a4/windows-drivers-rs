use alloc::string::String;
use core::ptr;
use core::sync::atomic::{AtomicPtr, Ordering};

use wdf_macros::object_context;
#[doc(hidden)]
pub use wdk_sys::{
    NT_SUCCESS,
    NTSTATUS,
    PCUNICODE_STRING,
    WDF_OBJECT_ATTRIBUTES,
    WDF_OBJECT_CONTEXT_TYPE_INFO,
    WDFOBJECT,
};
use wdk_sys::{
    WDF_DRIVER_CONFIG,
    WDF_DRIVER_VERSION_AVAILABLE_PARAMS,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDFDEVICE_INIT,
    WDFDRIVER,
    call_unsafe_wdf_function_binding,
};

use super::{
    device::DeviceInit,
    init_wdf_struct,
    object::{Handle, impl_handle},
    result::{NtResult, StatusCodeExt},
    string::{UnicodeString, WString},
};
use crate::println;

// HACK: DRIVER_OBJECT is not generated in the auto-generated bindings
// for UMDF (although it is for KMDF) because relevant headers are not
// included under UMDF currently. Needs a fix in wdk_sys
pub type DRIVER_OBJECT = wdk_sys::_DRIVER_OBJECT;

/// Global pointer to the WPP provider cleanup function.
/// Set during driver entry, called during driver unload.
static WPP_CLEANUP_FN: AtomicPtr<()> = AtomicPtr::new(ptr::null_mut());

/// A safe wrapper around `DRIVER_OBJECT`
#[repr(transparent)]
pub struct DriverObject(DRIVER_OBJECT);

/// Configuration for creating a WDF driver
pub struct DriverConfig {
    /// The pool tag to be used for all allocations made by the framework
    /// on behalf of this driver. A value of `0` means the framework will
    /// use the driver's image name as the pool tag.
    pub pool_tag: u32,

    /// The callback invoked by the framework when a new device is added
    pub evt_device_add: fn(&mut DeviceInit) -> NtResult<()>,
}

impl DriverConfig {
    pub fn new(evt_device_add: fn(&mut DeviceInit) -> NtResult<()>) -> Self {
        Self {
            pool_tag: 0,
            evt_device_add,
        }
    }
}

impl_handle!(
    /// Represents a WDF driver object
    Driver
);

#[object_context(Driver)]
struct DriverContext {
    evt_device_add: fn(&mut DeviceInit) -> NtResult<()>,
}

impl Driver {
    /// Creates a new driver object
    pub fn create<'a>(
        driver_object: &'a mut DriverObject,
        registry_path: &UnicodeString,
        config: DriverConfig,
    ) -> NtResult<&'a Driver> {
        let mut driver_config = init_wdf_struct!(WDF_DRIVER_CONFIG);
        driver_config.EvtDriverDeviceAdd = Some(evt_driver_device_add);
        driver_config.DriverPoolTag = config.pool_tag;
        driver_config.EvtDriverUnload = Some(driver_unload);

        let mut wdf_driver: WDFDRIVER = ptr::null_mut();

        let reg_path_ptr: PCUNICODE_STRING = (registry_path as *const UnicodeString).cast();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDriverCreate,
                &mut driver_object.0,
                reg_path_ptr,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut driver_config,
                &raw mut wdf_driver,
            )
        }
        .ok()?;

        // SAFETY: `Driver` is a ZST handle type (via `impl_handle!`), so
        // casting the raw `WDFDRIVER` pointer to `&Driver` is sound.
        let driver: &Driver = unsafe { &*(wdf_driver.cast()) };

        DriverContext::attach(
            driver,
            DriverContext {
                evt_device_add: config.evt_device_add,
            },
        )?;

        Ok(driver)
    }

    pub fn retrieve_version_string(&self) -> NtResult<String> {
        let string = WString::create()?;

        let status = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDriverRetrieveVersionString,
                self.as_ptr().cast(),
                string.as_ptr().cast(),
            )
        };

        if !NT_SUCCESS(status) {
            return Err(status.into());
        }

        Ok(string.to_rust_string_lossy())
    }

    pub fn is_version_available(&self, major_version: u32, minor_version: u32) -> bool {
        let mut params = init_wdf_struct!(WDF_DRIVER_VERSION_AVAILABLE_PARAMS);
        params.MajorVersion = major_version;
        params.MinorVersion = minor_version;

        let res = unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDriverIsVersionAvailable,
                self.as_ptr().cast(),
                &raw mut params,
            )
        };

        res != 0
    }
}

fn clean_up_wpp() {
    let ptr = WPP_CLEANUP_FN.swap(ptr::null_mut(), Ordering::Relaxed);
    if !ptr.is_null() {
        // SAFETY: The pointer was stored from a valid `fn()` during driver entry.
        let cleanup: fn() = unsafe { core::mem::transmute(ptr) };
        cleanup();
    }
}

/// Calls the safe driver entry function
///
/// It is meant to be called by the driver entry function generated
/// in the user's driver code by the `driver_entry` procedural
/// macro attribute
#[doc(hidden)]
pub fn call_safe_driver_entry(
    driver_object: &mut DRIVER_OBJECT,
    reg_path: PCUNICODE_STRING,
    safe_entry: fn(&mut DriverObject, &UnicodeString) -> NtResult<()>,
    wpp_cleanup: Option<fn()>,
) -> NTSTATUS {
    // SAFETY: `DriverObject` is `#[repr(transparent)]` over `DRIVER_OBJECT`,
    // so this cast is sound.
    let driver_object: &mut DriverObject =
        unsafe { &mut *(driver_object as *mut DRIVER_OBJECT as *mut DriverObject) };

    // SAFETY: `UnicodeString` is `#[repr(transparent)]` over `UNICODE_STRING`,
    // so casting `PCUNICODE_STRING` to `&UnicodeString` preserves pointer identity.
    let registry_path: &UnicodeString = unsafe { &*(reg_path.cast::<UnicodeString>()) };

    // Store WPP cleanup function for driver_unload to call
    if let Some(cleanup) = wpp_cleanup {
        WPP_CLEANUP_FN.store(cleanup as *mut (), Ordering::Relaxed);
    }

    match safe_entry(driver_object, registry_path) {
        Ok(()) => 0,
        Err(e) => {
            clean_up_wpp();
            e.code()
        }
    }
}

#[unsafe(link_section = "PAGE")]
extern "C" fn evt_driver_device_add(
    driver: WDFDRIVER,
    device_init: *mut WDFDEVICE_INIT,
) -> NTSTATUS {
    // SAFETY: `Driver` is a ZST handle type, so casting `WDFDRIVER` to
    // `&Driver` is sound.
    let driver: &Driver = unsafe { &*(driver.cast()) };
    let ctxt = DriverContext::get(driver);

    let mut device_init = unsafe { DeviceInit::from(device_init) };
    match (ctxt.evt_device_add)(&mut device_init) {
        Ok(_) => 0,
        Err(e) => e.code(),
    }
}

unsafe extern "C" fn driver_unload(_driver: WDFDRIVER) {
    println!("Driver unload");

    clean_up_wpp();

    println!("Driver unload done");
}
