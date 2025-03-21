extern crate windows;
use std::ffi::OsString;
use std::os::windows::ffi::OsStrExt;
use std::ptr::null_mut;
use windows::{
    core::{GUID, PCWSTR},
    Win32::{
        Devices::DeviceAndDriverInstallation::{
            CM_Get_Device_Interface_ListW, CM_Get_Device_Interface_List_SizeW,
            CM_GET_DEVICE_INTERFACE_LIST_PRESENT, CONFIGRET,
        },
        Foundation::{CloseHandle, ERROR_SUCCESS, GetLastError, HANDLE, BOOL},
        Storage::FileSystem::{
            CreateFileW, WriteFile, FILE_FLAGS_AND_ATTRIBUTES, FILE_GENERIC_WRITE, FILE_SHARE_MODE,
            OPEN_EXISTING,
        },
        System::IO::{CancelIoEx, OVERLAPPED },
        System::Console::{CTRL_C_EVENT, SetConsoleCtrlHandler},
    },
};
use std::env;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;


static CANCEL_FLAG: AtomicBool = AtomicBool::new(false);


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <interface_guid>", args[0]);
        std::process::exit(1);
    }

    let Some(interface_guid) = parse_guid(&args[1]) else {
        eprintln!("Failed to parse GUID");
        return;
    };

    let device_path = match get_device_path(&interface_guid) {
        Ok(path) => path,
        Err(e) => {
            eprintln!("Error: {}", e);
            return;
        }
    };

    println!("Device Path: {}", device_path);

    unsafe {
        SetConsoleCtrlHandler(Some(ctrlc_handler), true);
    }

    // Send a write request to the device
    match send_write_request(&device_path, "Hello, Device!") {
        Ok(()) => println!("Write request completed"),
        Err(e) => {
            eprintln!("Error sending write request: {}", e);
            return;
        }
    }
}

unsafe extern "system" fn ctrlc_handler(ctrl_type: u32) -> BOOL {
    if ctrl_type == CTRL_C_EVENT {
        CANCEL_FLAG.store(true, Ordering::SeqCst);
        println!("Ctrl+C pressed. Attempting to cancel the write request...");
        return true.into();
    }
    false.into()
}

fn get_device_path(interface_guid: &GUID) -> Result<String, String> {
    let mut device_interface_list_length: u32 = 0;

    // Get the size of the device interface list
    let cr = unsafe {
        CM_Get_Device_Interface_List_SizeW(
            &mut device_interface_list_length,
            interface_guid,
            core::ptr::null(),
            CM_GET_DEVICE_INTERFACE_LIST_PRESENT,
        )
    };

    if cr != CONFIGRET(ERROR_SUCCESS.0) {
        return Err(format!(
            "Error retrieving device interface list size: 0x{:x}",
            cr.0
        ));
    }

    if device_interface_list_length <= 1 {
        return Err("No active device interfaces found. Is the driver loaded?".to_string());
    }

    // Allocate memory for the device interface list
    let mut device_interface_list = vec![0u16; device_interface_list_length as usize];

    // Get the device interface list
    let cr = unsafe {
        CM_Get_Device_Interface_ListW(
            interface_guid,
            core::ptr::null(),
            &mut device_interface_list,
            CM_GET_DEVICE_INTERFACE_LIST_PRESENT,
        )
    };

    if cr != CONFIGRET(ERROR_SUCCESS.0) {
        return Err(format!(
            "Error retrieving device interface list: 0x{:x}",
            cr.0
        ));
    }

    // Copy the first device interface path to the output buffer
    let first_interface = device_interface_list
        .split(|&c| c == 0)
        .next()
        .unwrap_or(&[]);
    if first_interface.is_empty() {
        return Err("No valid device interfaces found.".to_string());
    }

    let device_path = String::from_utf16_lossy(first_interface);

    Ok(device_path)
}

fn send_write_request(device_path: &str, data: &str) -> Result<(), String> {
    // Convert the device path to a wide string
    let device_path_wide: Vec<u16> = OsString::from(device_path)
        .encode_wide()
        .chain(Some(0))
        .collect();

    // Open the device
    let handle = match unsafe {
        CreateFileW(
            PCWSTR(device_path_wide.as_ptr()),
            FILE_GENERIC_WRITE,
            FILE_SHARE_MODE(0),
            null_mut(),
            OPEN_EXISTING,
            FILE_FLAGS_AND_ATTRIBUTES(0),
            None,
        )
    } {
        Ok(handle) => handle,
        Err(e) => {
            return Err(format!("Failed to open device: {e}"));
        }
    };

    if handle == HANDLE::default() {
        return Err("Failed to open device".to_string());
    }

    // Data to write to the device
    let data = data.as_bytes();
    let mut bytes_written = 0;

    // Create a thread to monitor the cancel flag
    let cancel_thread = thread::spawn(move || {
        while !CANCEL_FLAG.load(Ordering::SeqCst) {
            thread::sleep(Duration::from_millis(100));
        }
        unsafe {
            CancelIoEx(handle, null_mut());
        }
    });

    // Send the write request
    let result = unsafe {
        WriteFile(
            handle,
            data.as_ptr() as *const _,
            data.len() as u32,
            &mut bytes_written,
            null_mut() as *mut OVERLAPPED,
        )
    };

    // Wait for the cancel thread to finish
    cancel_thread.join().unwrap();

    // Close the device handle
    unsafe {
        CloseHandle(handle);
    }

    if result.as_bool() {
        Ok(())
    } else {
        let error_code = unsafe { GetLastError() };
        return Err(format!(
            "Failed to write to the device. Error code: {}",
            error_code.0
        ));
    }
}

fn parse_guid(guid_str: &str) -> Option<GUID> {
    let parsed_guid = uuid::Uuid::parse_str(guid_str).ok()?;
    let fields = parsed_guid.as_fields();
    Some(GUID::from_values(
        fields.0 as u32,
        fields.1 as u16,
        fields.2 as u16,
        [
            fields.3[0],
            fields.3[1],
            fields.3[2],
            fields.3[3],
            fields.3[4],
            fields.3[5],
            fields.3[6],
            fields.3[7],
        ],
    ))
}
