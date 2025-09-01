//! USB FX2 sample driver

#![no_std]

use wdf::{
    Arc,
    CmResList,
    Device,
    DeviceInit,
    DeviceIoType,
    Driver,
    IoTypeConfig,
    NtResult,
    PnpPowerEventCallbacks,
    PowerDeviceState,
    SpinLock,
    driver_entry,
    object_context,
    trace,
    usb::{UsbDevice, UsbDeviceCreateConfig},
};

#[object_context(Device)]
struct DeviceContext {
    usb_device: SpinLock<Option<Arc<UsbDevice>>>,
}

/// The entry point for the driver. It initializes the driver and is the first
/// routine called by the system after the driver is loaded. `driver_entry`
/// specifies the other entry points in the function driver such as
/// `evt_device_add`.
///
/// The #[driver_entry] attribute is used to mark the entry point.
/// It is a proc macro that generates the shim code which enables WDF
/// to call this driver
///
/// # Arguments
///
/// * `driver` - Represents the instance of the function driver that is loaded
/// into memory. `driver` object is allocated by the system before the
/// driver is loaded, and it is released by the system after the system unloads
/// the function driver from memory.
///
/// * `registry_path` - Represents the driver specific path in the Registry.
/// The function driver can use the path to store driver related data between
/// reboots. The path does not store hardware instance specific data.
#[driver_entry(tracing_control_guid = "cb94defb-592a-4509-8f2e-54f204929669")]
fn driver_entry(driver: &mut Driver, _registry_path: &str) -> NtResult<()> {
    trace("OSRUSBFX2 Driver Sample - Driver Framework Edition.\n");

    // Set up the device add callback
    driver.on_evt_device_add(evt_device_add);

    Ok(())
}

fn evt_device_add(device_init: &mut DeviceInit) -> NtResult<()> {
    trace("Device add callback called");

    let pnp_power_callbacks = PnpPowerEventCallbacks {
        evt_device_prepare_hardware: Some(evt_device_prepare_hardware),
        evt_device_d0_entry: Some(evt_device_d0_entry),
        evt_device_d0_exit: Some(evt_device_d0_exit),
        evt_device_self_managed_io_flush: Some(evt_device_self_managed_io_flush),
        ..PnpPowerEventCallbacks::default()
    };


    let io_type = IoTypeConfig {
        read_write_io_type: DeviceIoType::Buffered,
        ..Default::default()
    };

    device_init.set_io_type(&io_type);

    let device = Device::create(device_init, Some(pnp_power_callbacks))?;

    let context = DeviceContext {
        usb_device: SpinLock::create(None)?,
    };

    DeviceContext::attach(&device, context)?;

    Ok(())
}

fn evt_device_prepare_hardware(
    device: &mut Device,
    _resources_raw: &CmResList,
    _resources_translated: &CmResList,
) -> NtResult<()> {
    trace("Device prepare hardware callback called");

    if let Some(device_context) = DeviceContext::get(device) {
        let usb_device = UsbDevice::create(
            device,
            &UsbDeviceCreateConfig {
                usbd_client_contract_version: 0x0100_0000,
            },
            |_usb_device| {},
        )?;

        {
            let mut usb_device_in_ctxt = device_context.usb_device.lock();
            if usb_device_in_ctxt.is_none() {
                *usb_device_in_ctxt = Some(usb_device);
            }
        }
    }

    Ok(())
}

fn evt_device_d0_entry(_device: &Device, _previous_state: PowerDeviceState) -> NtResult<()> {
    trace("Device D0 entry callback called");

    Ok(())
}

fn evt_device_d0_exit(_device: &Device, _next_state: PowerDeviceState) -> NtResult<()> {
    trace("Device D0 exit callback called");

    Ok(())
}

fn evt_device_self_managed_io_flush(_device: &Device) {
    trace("Device self-managed I/O flush callback called");
}
