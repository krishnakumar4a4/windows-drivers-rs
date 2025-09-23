//! USB FX2 sample driver

#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use wdf::{
    Arc,
    CmResList,
    Device,
    DeviceInit,
    DeviceIoType,
    DevicePnpCapabilities,
    DevicePowerPolicyIdleSettings,
    DevicePowerPolicyWakeSettings,
    Driver,
    Guid,
    IoQueue,
    IoQueueConfig,
    IoQueueDispatchType,
    IoTarget,
    IoTargetSentIoAction,
    IoTypeConfig,
    NtResult,
    NtStatusError,
    PnpPowerEventCallbacks,
    PowerDeviceState,
    PowerPolicyS0IdleCapabilities,
    RequestId,
    RequestType,
    SentRequest,
    SpinLock,
    TriState,
    driver_entry,
    object_context,
    println,
    status_codes,
    usb::{UsbDevice, UsbDeviceCreateConfig, UsbDeviceTraits, UsbPipe, UsbPipeType},
};

mod bulkrwr;
mod dev_prop;
mod interrupt;
mod ioctl;

use bulkrwr::{evt_io_read, evt_io_stop, evt_io_write};
use dev_prop::{
    set_device_interface_property_restricted,
    set_device_interface_property_unrestricted_device_capabilities,
};
use interrupt::cont_reader_for_interrupt_endpoint;
use ioctl::{SwitchState, evt_io_device_control, usb_ioctl_get_interrupt_message};

const GUID_DEVINTERFACE_OSRUSBFX2: &str = "573e8c73-0cb4-4471-a1bf-fab26c31d384";
const USBD_CLIENT_CONTRACT_VERSION_602: u32 = 0x602;

#[object_context(Device)]
struct DeviceContext {
    usb_device: Option<Arc<UsbDevice>>,
    usb_device_traits: SpinLock<UsbDeviceTraits>,
    current_switch_state: SpinLock<SwitchState>,
    interrupt_msg_queue: Arc<IoQueue>,
    sent_requests: SpinLock<Vec<SentRequest>>, // TODO: change to HashMap when available
    // Below three queue are never used.
    // They're  stored here only to keep them alive
    _default_queue: Arc<IoQueue>,
    _read_queue: Arc<IoQueue>,
    _write_queue: Arc<IoQueue>,
}

impl DeviceContext {
    fn get_interrupt_pipe(&self) -> &UsbPipe {
        self.get_usb_pipe(|usb_dev_ctx| usb_dev_ctx.interrupt_pipe_index)
    }

    fn get_bulk_read_pipe(&self) -> &UsbPipe {
        self.get_usb_pipe(|usb_dev_ctx| usb_dev_ctx.bulk_read_pipe_index)
    }

    fn get_bulk_write_pipe(&self) -> &UsbPipe {
        self.get_usb_pipe(|usb_dev_ctx| usb_dev_ctx.bulk_write_pipe_index)
    }

    fn add_sent_request(&self, sent_request: SentRequest) {
        let mut sent_requests = self.sent_requests.lock();
        sent_requests.push(sent_request);
    }

    fn get_sent_request(&self, request_id: RequestId) -> Option<SentRequest> {
        let mut sent_requests = self.sent_requests.lock();
        if let Some(pos) = sent_requests.iter().position(|r| r.id() == request_id) {
            Some(sent_requests.remove(pos))
        } else {
            None
        }
    }

    fn get_usb_pipe<F: Fn(&UsbDeviceContext) -> u8>(&self, pipe_index: F) -> &UsbPipe {
        let usb_device = self.usb_device.as_ref().expect("USB device should be set");
        let usb_device_context = UsbDeviceContext::get(&usb_device);
        let usb_interface = usb_device
            .get_interface(0)
            .expect("USB interface 0 should be present");
        usb_interface
            .get_configured_pipe(pipe_index(usb_device_context))
            .expect("USB pipe should be present")
    }
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
    println!("OSRUSBFX2 Rust Driver Sample - Driver Framework Edition.\n");

    // Set up the device add callback
    driver.on_evt_device_add(evt_device_add);

    Ok(())
}

/// `evt_device_add` is called by the framework in response to AddDevice
/// call from the PnP manager. We create and initialize a device object to
/// represent a new instance of the device. All the software resources
/// should be allocated in this callback.
fn evt_device_add(device_init: &mut DeviceInit) -> NtResult<()> {
    println!("Device add callback called");

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

    let device = Device::create(device_init, Some(pnp_power_callbacks))
        .inspect_err(|e| println!("Failed to create device: {:?}", e))?;

    let pnp_caps = DevicePnpCapabilities {
        surprise_removal_ok: TriState::True,
        ..Default::default()
    };

    device.set_pnp_capabilities(&pnp_caps);

    // Create a parallel default queue and register an event callback to
    // receive ioctl requests. We will create separate queues for
    // handling read and write requests. All other requests will be
    // completed with error status automatically by the framework.

    // If the driver has not explicitly set PowerManaged to WdfFalse, the
    // framework creates power-managed queues when the device is not a filter
    // driver. Normally the EvtIoStop is required for power-managed queues,
    // but for this driver it is not needed b/c the driver doesn't hold on
    // to the requests for long time or forward them to other drivers.
    // If the EvtIoStop callback is not implemented, the framework waits for
    // all driver-owned requests to be done before moving in the Dx/sleep
    // states or before removing the device, which is the correct behavior
    // for this type of driver. If the requests were taking an indeterminate
    // amount of time to complete, or if the driver forwarded the requests
    // to a lower driver/another stack, the queue should have an
    // EvtIoStop/EvtIoResume.
    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Parallel {
            presented_requests_limit: None,
        },
        evt_io_device_control: Some(evt_io_device_control),
        ..Default::default()
    };

    let default_queue = IoQueue::create(device, &queue_config)
        .inspect_err(|e| println!("Failed to create default queue: {:?}", e))?;

    // We will create a separate sequential queue and configure it
    // to receive read requests.  We also need to register a EvtIoStop
    // handler so that we can acknowledge requests that are pending
    // at the target driver.
    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Sequential,
        evt_io_read: Some(evt_io_read),
        evt_io_stop: Some(evt_io_stop),
        ..Default::default()
    };

    let read_queue = IoQueue::create(device, &queue_config)
        .inspect_err(|e| println!("Failed to create read queue: {:?}", e))?;

    device
        .configure_request_dispatching(&read_queue, RequestType::Read)
        .inspect_err(|e| {
            println!(
                "Failed to configure request dispatching for the read queue: {:?}",
                e
            )
        })?;

    // We will create another sequential queue and configure it
    // to receive write requests.
    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Sequential,
        evt_io_write: Some(evt_io_write),
        evt_io_stop: Some(evt_io_stop),
        ..Default::default()
    };

    let write_queue = IoQueue::create(device, &queue_config)
        .inspect_err(|e| println!("Failed to create write queue: {:?}", e))?;

    device
        .configure_request_dispatching(&write_queue, RequestType::Write)
        .inspect_err(|e| {
            println!(
                "Failed to configure request dispatching for the write queue: {:?}",
                e
            )
        })?;

    // Register a manual I/O queue for handling Interrupt Message Read Requests.
    // This queue will be used for storing Requests that need to wait for an
    // interrupt to occur before they can be completed.
    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Manual,
        // This queue is used for requests that don't directly access the device. The
        // requests in this queue are serviced only when the device is in a fully
        // powered state and sends an interrupt. So we can use a non-power managed
        // queue to park the requests since we don't care whether the device is idle
        // or fully powered up.
        power_managed: TriState::False,
        ..Default::default()
    };

    let interrupt_msg_queue = IoQueue::create(device, &queue_config)
        .inspect_err(|e| println!("Failed to create interrupt message queue: {:?}", e))?;

    // Attach device context
    let context = DeviceContext {
        usb_device: None,
        usb_device_traits: SpinLock::create(UsbDeviceTraits::empty())?,
        current_switch_state: SpinLock::create(SwitchState::empty())?,
        _default_queue: default_queue,
        _read_queue: read_queue,
        _write_queue: write_queue,
        interrupt_msg_queue,
        sent_requests: SpinLock::create(Vec::new())?,
    };

    DeviceContext::attach(device, context)?;

    // Register a device interface so that the app can find our device and talk to
    // it.
    let interface_guid = Guid::parse(GUID_DEVINTERFACE_OSRUSBFX2)
        .expect("GUID_DEVINTERFACE_OSRUSBFX2 should be valid");
    device
        .create_device_interface(&interface_guid, None)
        .inspect_err(|e| println!("Failed to create device interface: {:?}", e))?;

    // Get the string for the device interface and set the restricted
    // property on it to allow applications bound with device metadata
    // to access the interface.
    let symbolic_link_name = device
        .retrieve_device_interface_string(&interface_guid, None)
        .inspect_err(|e| println!("Failed to get device interface symbolic link name: {:?}", e))?;

    set_device_interface_property_restricted(&symbolic_link_name).inspect_err(|e| {
        println!(
            "Failed to set restricted property on device interface: {:?}",
            e
        )
    })?;

    // Adding Custom Capability:
    //
    // Adds a custom capability to device interface instance that allows a Windows
    // Store device app to access this interface using Windows.Devices.Custom
    // namespace. This capability can be defined either in INF or here as shown
    // below. In order to define it from the INF, uncomment the section "OsrUsb
    // Interface installation" from the INF and remove the block of code below.
    set_device_interface_property_unrestricted_device_capabilities(
        &symbolic_link_name,
        "microsoft.hsaTestCustomCapability_q536wpkpf5cy2",
    )
    .inspect_err(|e| {
        println!(
            "Failed to set custom capabilities property on device interface: {:?}",
            e
        )
    })?;

    Ok(())
}

fn evt_device_prepare_hardware(
    device: &mut Device,
    _resources_raw: &CmResList,
    _resources_translated: &CmResList,
) -> NtResult<()> {
    println!("Device prepare hardware callback called");

    // In this function we have to get DeviceContext multiple times
    // in order to prevent borrow checker issues.
    // TODO: try to simplify this

    // Create a UsbDevice if it does not already exist
    let usb_device_exists = DeviceContext::get(device).usb_device.is_some();

    if !usb_device_exists {
        let usb_device = UsbDevice::create(
            device,
            &UsbDeviceCreateConfig {
                usbd_client_contract_version: USBD_CLIENT_CONTRACT_VERSION_602,
            },
        )?;

        let device_ctxt = DeviceContext::get_mut(device);
        device_ctxt.usb_device = Some(usb_device);
    }

    let device_ctxt = DeviceContext::get(device);
    let usb_device = device_ctxt
        .usb_device
        .as_ref()
        .expect("USB device should be set");

    let info = usb_device.retrieve_information()?;
    println!(
        "IsDeviceHighSpeed: {}",
        info.traits.contains(UsbDeviceTraits::AT_HIGH_SPEED)
    );
    println!(
        "IsDeviceSelfPowered: {}",
        info.traits.contains(UsbDeviceTraits::SELF_POWERED)
    );
    println!(
        "IsDeviceRemoteWakeable: {}",
        info.traits.contains(UsbDeviceTraits::REMOTE_WAKE_CAPABLE)
    );

    if info.traits.contains(UsbDeviceTraits::REMOTE_WAKE_CAPABLE) {
        set_power_policy(device)?;
    }

    let device_ctxt = DeviceContext::get_mut(device);
    let usb_device = device_ctxt
        .usb_device
        .as_mut()
        .expect("USB device should be set");
    select_interface(usb_device.get_mut())?;

    *device_ctxt.usb_device_traits.lock() = info.traits;

    Ok(())
}

fn evt_device_d0_entry(device: &Device, _previous_state: PowerDeviceState) -> NtResult<()> {
    println!("Device D0 entry callback called");
    let io_target = get_interrupt_io_target(device);

    if let Err(e) = io_target.start() {
        println!("Failed to start IO target: {:?}", e);
        io_target.stop(IoTargetSentIoAction::CancelSentIo);
        return Err(e);
    }

    Ok(())
}

fn evt_device_d0_exit(device: &Device, _next_state: PowerDeviceState) -> NtResult<()> {
    println!("Device D0 exit callback called");

    let io_target = get_interrupt_io_target(device);
    io_target.stop(IoTargetSentIoAction::CancelSentIo);

    Ok(())
}

fn get_interrupt_io_target(device: &Device) -> &IoTarget {
    let device_context = DeviceContext::get(device);
    let interrupt_pipe = device_context.get_interrupt_pipe();
    interrupt_pipe.get_io_target()
}

fn evt_device_self_managed_io_flush(device: &Device) {
    println!("Device self-managed I/O flush callback called");
    usb_ioctl_get_interrupt_message(device, status_codes::STATUS_DEVICE_REMOVED.into());
}

fn set_power_policy(device: &Device) -> NtResult<()> {
    let mut idle_settings = DevicePowerPolicyIdleSettings::from_caps(
        PowerPolicyS0IdleCapabilities::UsbSelectiveSuspend,
    );
    idle_settings.idle_timeout = 10_000; // 10 seconds
    device.assign_s0_idle_settings(&idle_settings)?;

    let wake_settings = DevicePowerPolicyWakeSettings::default();
    device.assign_sx_wake_settings(&wake_settings)?;

    Ok(())
}

fn select_interface(usb_device: &mut UsbDevice) -> NtResult<()> {
    let interface_info = usb_device.select_config_single_interface()?;

    let mut interrupt_pipe_index = None;
    let mut bulk_read_pipe_index = None;
    let mut bulk_write_pipe_index = None;

    for i in 0..interface_info.number_of_configured_pipes {
        let Some((pipe, pipe_info)) = interface_info
            .configured_usb_interface
            .get_configured_pipe_with_information(i)
        else {
            println!("Failed to get pipe information for pipe index {}", i);
            return Err(NtStatusError::from(status_codes::STATUS_INTERNAL_ERROR));
        };

        match pipe_info.pipe_type {
            UsbPipeType::Interrupt => {
                println!("Interrupt Pipe is 0x{:x}", i);
                interrupt_pipe_index = Some(i);
            }
            UsbPipeType::Bulk => {
                if pipe.is_in_endpoint() {
                    println!("BulkInput Pipe is 0x{:x}", i);
                    bulk_read_pipe_index = Some(i);
                } else if pipe.is_out_endpoint() {
                    println!("BulkOutput Pipe is 0x{:x}", i);
                    bulk_write_pipe_index = Some(i);
                }
            }
            _ => {}
        }
    }

    if interrupt_pipe_index.is_none()
        || bulk_read_pipe_index.is_none()
        || bulk_write_pipe_index.is_none()
    {
        println!("Device is not configured properly");
        return Err(NtStatusError::from(
            status_codes::STATUS_INVALID_DEVICE_STATE,
        ));
    }

    let context = UsbDeviceContext {
        interrupt_pipe_index: interrupt_pipe_index.expect("interrupt_pipe_index should be Some"),
        bulk_read_pipe_index: bulk_read_pipe_index.expect("bulk_read_pipe_index should be Some"),
        bulk_write_pipe_index: bulk_write_pipe_index.expect("bulk_write_pipe_index should be Some"),
    };

    UsbDeviceContext::attach(usb_device, context)?;

    let Some(interface) = usb_device.get_interface_mut(0) else {
        println!("Failed to get interface 0");
        return Err(NtStatusError::from(status_codes::STATUS_INTERNAL_ERROR));
    };

    let Some(interrupt_pipe) = interface.get_configured_pipe_mut(interrupt_pipe_index.unwrap())
    else {
        println!("Failed to get interrupt pipe");
        return Err(NtStatusError::from(status_codes::STATUS_INTERNAL_ERROR));
    };

    cont_reader_for_interrupt_endpoint(interrupt_pipe)?;

    Ok(())
}
