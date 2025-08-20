pub mod device;
pub mod driver;
pub mod error;
pub mod guid;
pub mod io_queue;
pub mod memory;
pub mod object;
pub mod object_context;
pub mod request;
pub mod resource;
pub mod string;
pub mod sync;
pub mod timer;
pub mod tracing;

pub use device::*;
pub use driver::*;
pub use error::*;
pub use guid::*;
pub use io_queue::*;
pub use memory::*;
pub use object::*;
pub use object_context::*;
pub use request::*;
pub use resource::*;
pub use sync::*;
pub use timer::*;
pub use wdf_macros::*;
pub use wdk::println;

use wdk_sys::WDF_TRI_STATE;

safe_c_enum! {
    infallible;
    pub enum TriState: WDF_TRI_STATE {
        False = WdfFalse,
        True = WdfTrue,
        UseDefault = WdfUseDefault
    }
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
/// safe_c_enum! {
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
/// safe_c_enum! {
///     infallible;
///     #[repr(C)]
///     pub enum PowerState: WDF_POWER_STATE {
///         Value1 = WdfPowerRunning,
///         Value2 = WdfPowerSleep,
///         Value3 = WdfPowerHibernate
///     }
/// }
/// ```
macro_rules! safe_c_enum {
    // Helper for enum definition
    (@enumdef
        $(#[$enum_meta:meta])*
        $vis:vis enum $safe_name:ident : $c_type:ident {
            $( $variant:ident = $c_variant:ident ),* $(,)?
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
    // Helper for From<$safe_name> for $c_type
    (@from_safe_to_c
        $safe_name:ident, $c_type:ident, $( $variant:ident = $c_variant:ident ),*
    ) => {
        paste::paste! {
            impl From<$safe_name> for $c_type {
                fn from(value: $safe_name) -> Self {
                    match value {
                        $( $safe_name::$variant => wdk_sys::[<_ $c_type>]::$c_variant ),*
                    }
                }
            }
        }
    };
    // Infallible conversion
    (
        infallible;
        $(#[$enum_meta:meta])*
        $vis:vis enum $safe_name:ident : $c_type:ident {
            $( $variant:ident = $c_variant:ident ),* $(,)?
        }
    ) => {
        safe_c_enum!(@enumdef $(#[$enum_meta])* $vis enum $safe_name : $c_type { $( $variant = $c_variant ),* });
        safe_c_enum!(@from_safe_to_c $safe_name, $c_type, $( $variant = $c_variant ),*);
        paste::paste! {
            impl From<$c_type> for $safe_name {
                fn from(value: $c_type) -> Self {
                    match value {
                        $( v if v == wdk_sys::[<_ $c_type>]::$c_variant => $safe_name::$variant, )*
                        _ => unreachable!("Invalid value for {}", stringify!($safe_name)),
                    }
                }
            }
        }
    };
    // Fallible conversion (default)
    (
        $(#[$enum_meta:meta])*
        $vis:vis enum $safe_name:ident : $c_type:ident {
            $( $variant:ident = $c_variant:ident ),* $(,)?
        }
    ) => {
        safe_c_enum!(@enumdef $(#[$enum_meta])* $vis enum $safe_name : $c_type { $( $variant = $c_variant ),* });
        safe_c_enum!(@from_safe_to_c $safe_name, $c_type, $( $variant = $c_variant ),*);
        paste::paste! {
            impl TryFrom<$c_type> for $safe_name {
                type Error = ();
                fn try_from(value: $c_type) -> Result<Self, Self::Error> {
                    match value {
                        $( v if v == wdk_sys::[<_ $c_type>]::$c_variant => Ok($safe_name::$variant), )*
                        _ => Err(()),
                    }
                }
            }
        }
    };
}

pub(crate) use safe_c_enum;


