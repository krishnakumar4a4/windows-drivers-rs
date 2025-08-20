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


#[derive(Copy, Clone, Debug)]
pub enum TriState {
    False = 0,
    True = 1,
    UseDefault = 2,
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
/// convert from the Rust enum to the C enum and a TryFrom
/// implementation to convert in the opposite direction
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
/// ```
/// The corresponding Rust enum can be defined as:
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
macro_rules! safe_c_enum {
    (
        $(#[$enum_meta:meta])*
        $vis:vis enum $safe_name:ident : $c_type:ident {
            $(
                $variant:ident = $c_variant:ident
            ),* $(,)?
        }
    ) => {
        paste::paste! {
            $(#[$enum_meta])*
            #[derive(Copy, Clone, Debug, PartialEq, Eq)]
            $vis enum $safe_name {
                $(
                    $variant
                ),*
            }

            impl From<$safe_name> for $c_type {
                fn from(value: $safe_name) -> Self {
                    match value {
                        $(
                            $safe_name::$variant => wdk_sys::[<_ $c_type>]::$c_variant
                        ),*
                    }
                }
            }

            impl TryFrom<$c_type> for $safe_name {
                type Error = ();

                fn try_from(value: $c_type) -> Result<Self, Self::Error> {
                    match value {
                        $(
                            v if v == wdk_sys::[<_ $c_type>]::$c_variant => Ok($safe_name::$variant),
                        )*
                        _ => Err(()),
                    }
                }
            }
        }
    };
}

pub(crate) use safe_c_enum;


