pub use wdk_sys::{
    FILE_ANY_ACCESS,
    FILE_READ_ACCESS,
    FILE_SPECIAL_ACCESS,
    FILE_WRITE_ACCESS,
    METHOD_BUFFERED,
    METHOD_DIRECT_FROM_HARDWARE,
    METHOD_DIRECT_TO_HARDWARE,
    METHOD_IN_DIRECT,
    METHOD_NEITHER,
    METHOD_OUT_DIRECT,
};

pub const fn ctl_code(device_type: u32, function: u32, method: u32, access: u32) -> u32 {
    ((device_type) << 16) | ((access) << 14) | ((function) << 2) | (method)
}
