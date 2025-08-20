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
pub mod utils;

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

