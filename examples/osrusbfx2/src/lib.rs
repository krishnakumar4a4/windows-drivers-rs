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
    Slot,
    SpinLock,
    TriState,
    driver_entry,
    object_context,
    println,
    status_codes,
    usb::{UsbDevice, UsbDeviceCreateConfig, UsbDeviceTraits, UsbPipe, UsbPipeType},
};

mod bulkrwr;
mod interrupt;
mod ioctl;

use bulkrwr::{evt_io_read, evt_io_stop, evt_io_write};
use interrupt::cont_reader_for_interrupt_endpoint;
use ioctl::{SwitchState, evt_io_device_control, usb_ioctl_get_interrupt_message};

const USBD_CLIENT_CONTRACT_VERSION_602: u32 = 0x602;

#[object_context(Device)]
struct DeviceContext {
    usb_device: Slot<Arc<UsbDevice>>,
    usb_device_traits: SpinLock<UsbDeviceTraits>,
    current_switch_state: SpinLock<SwitchState>,
    interrupt_msg_queue: Slot<Arc<IoQueue>>,
    sent_requests: SpinLock<Vec<SentRequest>>, // TODO: change to HashMap when available
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
        let usb_device = self.usb_device.get().expect("USB device should be set");
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

    let pnp_caps = DevicePnpCapabilities {
        surprise_removal_ok: TriState::True,
        ..Default::default()
    };

    let device = Device::create(device_init, Some(pnp_power_callbacks), Some(&pnp_caps))?;

    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Parallel {
            presented_requests_limit: None,
        },
        evt_io_device_control: Some(evt_io_device_control),
        ..Default::default()
    };

    let _ = IoQueue::create(device, &queue_config)?;

    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Sequential,
        evt_io_read: Some(evt_io_read),
        evt_io_stop: Some(evt_io_stop),
        ..Default::default()
    };

    let read_queue = IoQueue::create(device, &queue_config)?;

    device.configure_request_dispatching(&read_queue, RequestType::Read)?;

    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Sequential,
        evt_io_write: Some(evt_io_write),
        evt_io_stop: Some(evt_io_stop),
        ..Default::default()
    };

    let write_queue = IoQueue::create(device, &queue_config)?;

    device.configure_request_dispatching(&write_queue, RequestType::Write)?;

    let queue_config = IoQueueConfig {
        dispatch_type: IoQueueDispatchType::Manual,
        power_managed: TriState::False,
        ..Default::default()
    };

    let interrupt_msg_queue = IoQueue::create(device, &queue_config)?;

    let context = DeviceContext {
        usb_device: Slot::try_new(None)?,
        usb_device_traits: SpinLock::create(UsbDeviceTraits::empty())?,
        current_switch_state: SpinLock::create(SwitchState::empty())?,
        interrupt_msg_queue: Slot::try_new(Some(interrupt_msg_queue))?,
        sent_requests: SpinLock::create(Vec::new())?,
    };

    DeviceContext::attach(device, context)?;

    Ok(())
}

fn evt_device_prepare_hardware(
    device: &mut Device,
    _resources_raw: &CmResList,
    _resources_translated: &CmResList,
) -> NtResult<()> {
    println!("Device prepare hardware callback called");

    let device_ctxt = DeviceContext::get(device);

    // Create a UsbDevice only if it does not already exist
    if device_ctxt.usb_device.is_some() {
        return Ok(());
    }

    // No UsbDevice created so create a new one and store it context
    let mut usb_device = UsbDevice::create(
        device,
        &UsbDeviceCreateConfig {
            usbd_client_contract_version: USBD_CLIENT_CONTRACT_VERSION_602,
        },
    )?;

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

    select_interface(usb_device.get_mut())?;

    device_ctxt.usb_device.set(Some(usb_device));
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
    println!("Set power policy callback called");

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
