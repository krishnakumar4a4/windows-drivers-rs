use wdf::{
    Device,
    IoQueue,
    NtResult,
    NtStatus,
    println,
    Request,
    status_codes,
    usb::{UsbDevice, UsbDeviceCreateConfig, UsbDeviceTraits, UsbPipeType},
};

use crate::DeviceContext;

pub fn usb_ioctl_get_interrupt_message(device: &Device, status: NtStatus) {
    println!("usb_ioctl_get_interrupt_message called with status: {:?}", status);
}

