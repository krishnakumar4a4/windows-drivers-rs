// This module exits only to make it easy to
// apply cfg attributes for conditional compilation
// in lib.rs. Without it the cfg attributes would
// have to be applied to every mod and pub uses
// statement individually.

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
