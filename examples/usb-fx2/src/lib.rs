//! USB FX2 sample driver

#![no_std]

use wdf::{
    Arc,
    CmResList,
    Device,
    DeviceInit,
    DeviceIoType,
    DevicePnpCapabilities,
    Driver,
    IoTypeConfig,
    IoQueue,
    IoQueueConfig,
    IoQueueDispatchType,
    NtResult,
    NtStatusError,
    PnpPowerEventCallbacks,
    PowerDeviceState,
    println,
    Request,
    RequestId,
    RequestType,
    RequestStopActionFlags,
    driver_entry,
    object_context,
    Slot,
    SpinLock,
    status_codes,
    trace,
    TriState,
    usb::{UsbDevice, UsbDeviceCreateConfig, UsbDeviceTraits, UsbPipeType},
};

mod interrupt;

use interrupt::cont_reader_for_interrupt_endpoint;

#[object_context(Device)]
struct DeviceContext {
    usb_device: Slot<Arc<UsbDevice>>,
    usb_device_traits: SpinLock<UsbDeviceTraits>,
}

#[object_context(UsbDevice)]
struct UsbDeviceContext {
    interrupt_pipe_index: u8,
    bulk_read_pipe_index: u8,
    bulk_write_pipe_index: u8,
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

    let pnp_caps = DevicePnpCapabilities {
        surprise_removal_ok: TriState::True,
        ..Default::default()
    };

    let device = Device::create(device_init, Some(pnp_power_callbacks), Some(&pnp_caps))?;

    let context = DeviceContext {
        usb_device: Slot::try_new(None)?,
        usb_device_traits: SpinLock::create(UsbDeviceTraits::empty())?,
    };

    DeviceContext::attach(&device, context)?;

    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Parallel {
            presented_requests_limit: None,
        },
        evt_io_device_control: Some(evt_io_device_control),
        ..Default::default()
    };

    let _ = IoQueue::create(
        &device,
        &queue_config,
    )?;

    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Sequential,
        evt_io_read: Some(evt_io_read),
        evt_io_stop: Some(evt_io_stop),
        ..Default::default()
    };

    let read_queue = IoQueue::create(
        &device,
        &queue_config,
    )?;

    device.configure_request_dispatching(&read_queue, RequestType::Read)?;

    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Sequential,
        evt_io_write: Some(evt_io_write),
        evt_io_stop: Some(evt_io_stop),
        ..Default::default()
    };

    let write_queue = IoQueue::create(
        &device,
        &queue_config,
    )?;

    device.configure_request_dispatching(&write_queue, RequestType::Write)?;

    Ok(())
}

fn evt_device_prepare_hardware(
    device: &Device,
    _resources_raw: &CmResList,
    _resources_translated: &CmResList,
) -> NtResult<()> {
    trace("Device prepare hardware callback called");

    let device_ctxt = DeviceContext::get(device).expect("device context should exist");

    // Create a UsbDevice only if it does not already exist
    if device_ctxt.usb_device.is_some() {
        return Ok(());
    }

    // No UsbDevice created so create a new one and store it context
    let usb_device = UsbDevice::create(
        device,
        &UsbDeviceCreateConfig { usbd_client_contract_version: 0x0100_0000 },
        select_interface
    )?;

    let info = usb_device.retrieve_information()?;
    println!("IsDeviceHighSpeed: {}", info.traits.contains(UsbDeviceTraits::AT_HIGH_SPEED));
    println!("IsDeviceSelfPowered: {}", info.traits.contains(UsbDeviceTraits::SELF_POWERED));
    println!("IsDeviceRemoteWakeable: {}", info.traits.contains(UsbDeviceTraits::REMOTE_WAKE_CAPABLE));

    device_ctxt.usb_device.set(Some(usb_device));
    *device_ctxt.usb_device_traits.lock() = info.traits;

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

fn evt_io_device_control(_queue: &IoQueue, _request: Request, _output_buffer_length: usize, _input_buffer_length: usize, _control_code: u32) {
    trace("I/O device control callback called");
}

fn evt_io_read(_queue: &IoQueue, _request: Request, _length: usize) {
    trace("I/O read callback called");
}

fn evt_io_write(_queue: &IoQueue, _request: Request, _length: usize) {
    trace("I/O write callback called");
}

fn evt_io_stop(_queue: &IoQueue, _request_id: RequestId, _action_flags: RequestStopActionFlags) {
    trace("I/O stop callback called");
}

fn select_interface(usb_device: &mut UsbDevice) -> NtResult<()> {
    let interface_info = usb_device.select_config_single_interface()?;

    let mut interrupt_pipe_index = None;
    let mut bulk_read_pipe_index = None;
    let mut bulk_write_pipe_index = None;

    for i in 0..interface_info.number_of_configured_pipes {
        let (pipe, pipe_info) = interface_info.configured_usb_interface.get_configured_pipe_with_information(i).ok_or_else(|| {
            println!("Failed to get pipe information for pipe index {}", i);
            NtStatusError::from(status_codes::STATUS_INTERNAL_ERROR)
        })?;

        match pipe_info.pipe_type {
            UsbPipeType::Interrupt => {
                println!("Interrupt Pipe is 0x{:x}", i);
                interrupt_pipe_index = Some(i);
            },
            UsbPipeType::Bulk => {
                if pipe.is_in_endpoint()  {
                    println!("BulkInput Pipe is 0x{:x}", i);
                    bulk_read_pipe_index = Some(i);
                } else if pipe.is_out_endpoint() {
                    println!("BulkOutput Pipe is 0x{:x}", i);
                    bulk_write_pipe_index = Some(i);
                }
            },
            _ => {}
        }
    }

    if interrupt_pipe_index.is_none() || bulk_read_pipe_index.is_none() || bulk_write_pipe_index.is_none() {
        println!("Device is not configured properly");
        return Err(NtStatusError::from(status_codes::STATUS_INVALID_DEVICE_STATE));
    }

    let context = UsbDeviceContext {
        interrupt_pipe_index: interrupt_pipe_index.expect("interrupt_pipe_index should be Some"),
        bulk_read_pipe_index: bulk_read_pipe_index.expect("bulk_read_pipe_index should be Some"),
        bulk_write_pipe_index: bulk_write_pipe_index.expect("bulk_write_pipe_index should be Some"),
    };

    UsbDeviceContext::attach(usb_device, context)?;

    Ok(())
}

