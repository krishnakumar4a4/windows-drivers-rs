pub mod device;
pub mod driver;
pub mod guid;
pub mod io_queue;
pub mod io_target;
pub mod ioctl;
pub mod memory;
pub mod object;
pub mod request;
pub mod resource;
pub mod result;
pub mod string;
pub mod sync;
pub mod timer;
pub mod tracing;

use core::time::Duration;

pub use device::*;
pub use driver::*;
pub use guid::*;
pub use io_queue::*;
pub use io_target::*;
pub use ioctl::*;
pub use memory::*;
pub use object::*;
pub use request::*;
pub use resource::*;
pub use result::*;
pub use sync::*;
pub use timer::*;
pub use wdf_macros::*;
pub use wdk::println;
use wdk_sys::WDF_TRI_STATE;

/// A timeout value which can either be
/// relative to the current system time
/// or to January 1, 1601 (UTC)
pub enum Timeout {
    /// Timeout Relative time current system time
    Relative(Duration),
    /// Timeout relative to  January 1, 1601 (UTC)
    Absolute(Duration),
}

impl Timeout {
    /// Convert the timeout to a WDF value
    /// which in 100-nanosecond intervals
    /// and is negative for relative duration
    /// and positive for absolute
    pub(crate) fn as_wdf_timeout(&self) -> i64 {
        match self {
            Timeout::Relative(dur) => -(Self::to_100_ns_intervals(*dur)),
            Timeout::Absolute(dur) => Self::to_100_ns_intervals(*dur),
        }
    }

    fn to_100_ns_intervals(duration: Duration) -> i64 {
        duration.as_nanos() as i64 / 100
    }
}

enum_mapping! {
    infallible;
    pub enum TriState: WDF_TRI_STATE {
        False = WdfFalse,
        True = WdfTrue,
        UseDefault = WdfUseDefault
    }
}

impl Default for TriState {
    fn default() -> Self {
        TriState::UseDefault
    }
}

macro_rules! init_wdf_struct {
    ($StructName:ty) => {{
        let mut raw_struct = <$StructName>::default();
        raw_struct.Size = crate::wdf_struct_size!($StructName);
        raw_struct
    }};
}

macro_rules! wdf_struct_size {
    ($StructName:ty) => {{
        paste::paste! {
            if unsafe { wdk_sys::WdfClientVersionHigherThanFramework } != 0 {
                let index = wdk_sys::_WDFSTRUCTENUM::[<INDEX_ $StructName>] as u32;
                if index < unsafe { wdk_sys::WdfStructureCount } {
                    unsafe {wdk_sys::WdfStructures.add(index as usize) as u32 }
                } else {
                    usize::MAX as u32
                }
            } else {
                core::mem::size_of::<$StructName>() as u32
            }
        }
    }};
}

pub(crate) use init_wdf_struct;
pub(crate) use wdf_struct_size;

/// A macro to declare a Rust enum that maps one-to-one to a C enum.
///
/// It implements Copy, Clone, Debug, PartialEq, and Eq traits
/// for the Rust enum and generates a From implementation to
/// convert from the Rust enum to the C enum and back.
///
/// It also allows you to specify attributes on top of the
/// enum like #[repr(C)]
///
/// # Example
///
/// If there is a C enum
/// ```c
/// typedef enum _WDF_POWER_STATE {
///     WdfPowerRunning,
///     WdfPowerSleep,
///     WdfPowerHibernate
/// } WDF_POWER_STATE;
///
/// the macro defines a Rust enum that maps it to a Rust enum.
///
/// By default, the conversion from the C type to the Rust enum is fallible (using `TryFrom`).
/// If you want the conversion to be infallible (using `From`), specify `infallible;` as the first argument.
///
/// # Examples
///
/// Fallible (default):
/// ```rust
/// enum_mapping! {
///     #[repr(C)]
///     pub enum PowerState: WDF_POWER_STATE {
///         Value1 = WdfPowerRunning,
///         Value2 = WdfPowerSleep,
///         Value3 = WdfPowerHibernate
///     }
/// }
/// ```
/// 
/// Infallible:
/// ```rust
/// enum_mapping! {
///     infallible;
///     #[repr(C)]
///     pub enum PowerState: WDF_POWER_STATE {
///         Value1 = WdfPowerRunning,
///         Value2 = WdfPowerSleep,
///         Value3 = WdfPowerHibernate
///     }
/// }
/// ```
macro_rules! enum_mapping {
    // Helper for enum definition
    (@enumdef
        $(#[$enum_meta:meta])*
        $vis:vis enum $safe_name:ident : $raw_type:ident {
            $( $variant:ident = $raw_variant:ident ),* $(,)?
        }
    ) => {
        paste::paste! {
            $(#[$enum_meta])*
            #[derive(Copy, Clone, Debug, PartialEq, Eq)]
            $vis enum $safe_name {
                $( $variant ),*
            }
        }
    };
    // Helper for From<$safe_name> for $raw_type
    (@from_safe_to_c
        $safe_name:ident, $raw_type:ident, $( $variant:ident = $raw_variant:ident ),*
    ) => {
        paste::paste! {
            impl From<$safe_name> for $raw_type {
                fn from(value: $safe_name) -> Self {
                    match value {
                        $( $safe_name::$variant => wdk_sys::[<_ $raw_type>]::$raw_variant ),*
                    }
                }
            }
        }
    };
    // Infallible conversion
    (
        infallible;
        $(#[$enum_meta:meta])*
        $vis:vis enum $safe_name:ident : $raw_type:ident {
            $( $variant:ident = $raw_variant:ident ),* $(,)?
        }
    ) => {
        enum_mapping!(@enumdef $(#[$enum_meta])* $vis enum $safe_name : $raw_type { $( $variant = $raw_variant ),* });
        enum_mapping!(@from_safe_to_c $safe_name, $raw_type, $( $variant = $raw_variant ),*);
        paste::paste! {
            impl From<$raw_type> for $safe_name {
                fn from(value: $raw_type) -> Self {
                    match value {
                        $( v if v == wdk_sys::[<_ $raw_type>]::$raw_variant => $safe_name::$variant, )*
                        _ => unreachable!("Invalid value for {}", stringify!($safe_name)),
                    }
                }
            }
        }
    };
    // Fallible conversion (default)
    (
        $(#[$enum_meta:meta])*
        $vis:vis enum $safe_name:ident : $raw_type:ident {
            $( $variant:ident = $raw_variant:ident ),* $(,)?
        }
    ) => {
        enum_mapping!(@enumdef $(#[$enum_meta])* $vis enum $safe_name : $raw_type { $( $variant = $raw_variant ),* });
        enum_mapping!(@from_safe_to_c $safe_name, $raw_type, $( $variant = $raw_variant ),*);
        paste::paste! {
            impl TryFrom<$raw_type> for $safe_name {
                type Error = ();
                fn try_from(value: $raw_type) -> Result<Self, Self::Error> {
                    match value {
                        $( v if v == wdk_sys::[<_ $raw_type>]::$raw_variant => Ok($safe_name::$variant), )*
                        _ => Err(()),
                    }
                }
            }
        }
    };
}

pub(crate) use enum_mapping;
