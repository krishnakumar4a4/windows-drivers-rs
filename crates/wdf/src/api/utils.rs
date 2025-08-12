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