use alloc::vec::Vec;

use wdf::{NtResult, NtStatusError, UnicodeString, WString, println, status_codes};
use wdk_sys::{
    DEVPROP_BOOLEAN,
    DEVPROP_TYPE_BOOLEAN,
    DEVPROP_TYPE_STRING_LIST,
    DEVPROPGUID,
    DEVPROPKEY,
    DEVPROPTYPE,
    NTSTATUS,
    PDEVPROPKEY,
    PUNICODE_STRING,
    PVOID,
    ULONG,
    UNICODE_STRING,
};

const IO_SET_DEVICE_INTERFACE_PROPERTY_DATA: &str = "IoSetDeviceInterfacePropertyData";
const DEVPROP_TRUE: DEVPROP_BOOLEAN = -1;

type IoSetDeviceInterfacePropertyData = extern "system" fn(
    symbolic_link_name: *mut UNICODE_STRING,
    property_key: *const DEVPROPKEY,
    lcid: u32,
    flags: u32,
    type_: u32,
    size: u32,
    data: *mut core::ffi::c_void,
) -> NTSTATUS;

unsafe extern "C" {
    fn MmGetSystemRoutineAddress(SystemRoutineName: PUNICODE_STRING) -> PVOID;
}

pub fn set_device_interface_property_restricted(symbolic_link_name: &WString) -> NtResult<()> {
    set_device_interface_property_data(
        symbolic_link_name,
        &restricted_prop_key(),
        DEVPROP_TYPE_BOOLEAN,
        size_of::<DEVPROP_BOOLEAN>() as ULONG,
        &DEVPROP_TRUE as *const DEVPROP_BOOLEAN as PVOID,
    )
}

pub fn set_device_interface_property_unrestricted_device_capabilities(
    symbolic_link_name: &WString,
    capabilities: &str,
) -> NtResult<()> {
    // Convert the capabilities string to a wide string multi‑sz
    let mut capabilities: Vec<u16> = capabilities.encode_utf16().collect();

    // Two NUL values to make a multi‑sz (double‑NUL terminated)
    capabilities.push(0);
    capabilities.push(0);

    let capabilities_size = capabilities.len() * size_of::<u16>();

    set_device_interface_property_data(
        symbolic_link_name,
        &unrestricted_device_capabilities_prop_key(),
        DEVPROP_TYPE_STRING_LIST,
        capabilities_size as ULONG,
        capabilities.as_ptr() as PVOID,
    )
}

fn set_device_interface_property_data(
    symbolic_link_name: &WString,
    property_key: &DEVPROPKEY,
    data_type: DEVPROPTYPE,
    size: ULONG,
    data: PVOID,
) -> NtResult<()> {
    let Some(set_device_interface_property_data) = get_interface_prop_data_routine() else {
        println!("Failed to get address of routine {IO_SET_DEVICE_INTERFACE_PROPERTY_DATA}");
        return Err(NtStatusError::from(status_codes::STATUS_NOT_SUPPORTED));
    };

    let mut symbolic_link_name = symbolic_link_name.get_unicode_string();

    let status = set_device_interface_property_data(
        &mut symbolic_link_name as *mut UNICODE_STRING,
        property_key as *const DEVPROPKEY as PDEVPROPKEY,
        0, // lcid
        0, // flags
        data_type,
        size,
        data,
    );

    if status >= 0 {
        Ok(())
    } else {
        println!(
            "Failed to set device interface property data, status: {:#X}",
            status
        );
        Err(NtStatusError::from(status))
    }
}

pub const fn restricted_prop_key() -> DEVPROPKEY {
    DEVPROPKEY {
        fmtid: DEVPROPGUID {
            Data1: 0x026E516E,
            Data2: 0xB814,
            Data3: 0x414B,
            Data4: [0x83, 0xCD, 0x85, 0x6D, 0x6F, 0xEF, 0x48, 0x22],
        },
        pid: 6,
    }
}

pub const fn unrestricted_device_capabilities_prop_key() -> DEVPROPKEY {
    DEVPROPKEY {
        fmtid: DEVPROPGUID {
            Data1: 0x026E516E,
            Data2: 0xB814,
            Data3: 0x414B,
            Data4: [0x83, 0xCD, 0x85, 0x6D, 0x6F, 0xEF, 0x48, 0x22],
        },
        pid: 8,
    }
}

fn get_interface_prop_data_routine() -> Option<IoSetDeviceInterfacePropertyData> {
    let routine_name = UnicodeString::from_rust_str(IO_SET_DEVICE_INTERFACE_PROPERTY_DATA);
    let routine_name_raw = routine_name.as_raw();

    let addr = unsafe {
        MmGetSystemRoutineAddress((routine_name_raw as *const UNICODE_STRING).cast_mut())
    };
    unsafe { core::mem::transmute::<PVOID, Option<IoSetDeviceInterfacePropertyData>>(addr) }
}
