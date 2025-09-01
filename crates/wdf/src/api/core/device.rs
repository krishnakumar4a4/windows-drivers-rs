use core::{default::Default, sync::atomic::AtomicUsize};

use wdf_macros::object_context_with_ref_count_check;
use wdk_sys::{
    call_unsafe_wdf_function_binding,
    BOOLEAN,
    DEVICE_POWER_STATE,
    DEVICE_RELATION_TYPE,
    NTSTATUS,
    WDFCMRESLIST,
    WDFDEVICE,
    WDFDEVICE_INIT,
    WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS,
    WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS,
    WDF_NO_HANDLE,
    WDF_NO_OBJECT_ATTRIBUTES,
    WDF_PNPPOWER_EVENT_CALLBACKS,
    WDF_POWER_DEVICE_STATE,
    WDF_POWER_POLICY_IDLE_TIMEOUT_TYPE,
    WDF_POWER_POLICY_S0_IDLE_CAPABILITIES,
    WDF_POWER_POLICY_S0_IDLE_USER_CONTROL,
    WDF_POWER_POLICY_SX_WAKE_USER_CONTROL,
    WDF_SPECIAL_FILE_TYPE,
};

use super::{
    enum_mapping,
    guid::Guid,
    init_wdf_struct,
    io_queue::IoQueue,
    object::{impl_ref_counted_handle, Handle},
    resource::CmResList,
    result::{NtResult, StatusCodeExt},
    string::{to_unicode_string, to_utf16_buf},
    TriState,
};

impl_ref_counted_handle!(Device, DeviceContext);

impl Device {
    pub fn create(
        device_init: &mut DeviceInit,
        pnp_power_callbacks: Option<PnpPowerEventCallbacks>,
    ) -> NtResult<&Self> {
        if let Some(ref pnp_power_callbacks) = pnp_power_callbacks {
            let mut pnp_power_callbacks = pnp_power_callbacks.into();

            unsafe {
                call_unsafe_wdf_function_binding!(
                    WdfDeviceInitSetPnpPowerEventCallbacks,
                    device_init.as_ptr_mut(),
                    &mut pnp_power_callbacks
                );
            }
        }

        let mut device: WDFDEVICE = WDF_NO_HANDLE.cast();
        let mut device_init_ptr: *mut WDFDEVICE_INIT = device_init.as_ptr_mut();

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDeviceCreate,
                &mut device_init_ptr,
                WDF_NO_OBJECT_ATTRIBUTES,
                &mut device,
            )
        }
        .and_then(|| {
            let device = unsafe { &*(device as *mut _) };
            DeviceContext::attach(
                device,
                DeviceContext {
                    ref_count: AtomicUsize::new(0),
                    pnp_power_callbacks,
                },
            )?;
            Ok(device)
        })
    }

    pub fn create_interface(
        &self,
        interaface_class_guid: &Guid,
        reference_string: Option<&str>,
    ) -> NtResult<()> {
        let ref_str_buf = reference_string.map(to_utf16_buf);
        let unicode_ref_str = ref_str_buf.map(|b| to_unicode_string(b.as_ref()));

        unsafe {
            call_unsafe_wdf_function_binding!(
                WdfDeviceCreateDeviceInterface,
                self.as_ptr() as *mut _,
                interaface_class_guid.as_lpcguid(),
                unicode_ref_str.map_or(core::ptr::null(), |s| &s)
            )
        }
        .ok()
    }

    pub fn get_default_queue(&self) -> Option<&IoQueue> {
        let queue = unsafe {
            call_unsafe_wdf_function_binding!(WdfDeviceGetDefaultQueue, self.as_ptr() as *mut _,)
        };

        if !queue.is_null() {
            Some(unsafe { &*(queue as *mut IoQueue) })
        } else {
            None
        }
    }
}

pub struct DeviceInit(*mut WDFDEVICE_INIT);

impl DeviceInit {
    pub unsafe fn from(inner: *mut WDFDEVICE_INIT) -> Self {
        Self(inner)
    }

    pub fn as_ptr_mut(&self) -> *mut WDFDEVICE_INIT {
        self.0
    }
}

#[object_context_with_ref_count_check(Device)]
struct DeviceContext {
    ref_count: AtomicUsize,
    pnp_power_callbacks: Option<PnpPowerEventCallbacks>,
}

pub struct PnpPowerEventCallbacks {
    pub evt_device_d0_entry: Option<fn(&Device, PowerDeviceState) -> NtResult<()>>,
    pub evt_device_d0_entry_post_interrupts_enabled:
        Option<fn(&Device, PowerDeviceState) -> NtResult<()>>,
    pub evt_device_d0_exit: Option<fn(&Device, PowerDeviceState) -> NtResult<()>>,
    pub evt_device_d0_exit_pre_interrupts_disabled:
        Option<fn(&Device, PowerDeviceState) -> NtResult<()>>,
    pub evt_device_prepare_hardware: Option<fn(&Device, &CmResList, &CmResList) -> NtResult<()>>,
    pub evt_device_release_hardware: Option<fn(&Device, &CmResList) -> NtResult<()>>,
    pub evt_device_self_managed_io_cleanup: Option<fn(&Device)>,
    pub evt_device_self_managed_io_flush: Option<fn(&Device)>,
    pub evt_device_self_managed_io_init: Option<fn(&Device) -> NtResult<()>>,
    pub evt_device_self_managed_io_suspend: Option<fn(&Device) -> NtResult<()>>,
    pub evt_device_self_managed_io_restart: Option<fn(&Device) -> NtResult<()>>,
    pub evt_device_surprise_removal: Option<fn(&Device)>,
    pub evt_device_query_remove: Option<fn(&Device) -> NtResult<()>>,
    pub evt_device_query_stop: Option<fn(&Device) -> NtResult<()>>,
    pub evt_device_usage_notification: Option<fn(&Device, SpecialFileType, bool)>,
    pub evt_device_relations_query: Option<fn(&Device, DeviceRelationType)>,
    pub evt_device_usage_notification_ex:
        Option<fn(&Device, SpecialFileType, bool) -> NtResult<()>>,
}

impl Default for PnpPowerEventCallbacks {
    fn default() -> Self {
        Self {
            evt_device_d0_entry: None,
            evt_device_d0_entry_post_interrupts_enabled: None,
            evt_device_d0_exit: None,
            evt_device_d0_exit_pre_interrupts_disabled: None,
            evt_device_prepare_hardware: None,
            evt_device_release_hardware: None,
            evt_device_self_managed_io_cleanup: None,
            evt_device_self_managed_io_flush: None,
            evt_device_self_managed_io_init: None,
            evt_device_self_managed_io_suspend: None,
            evt_device_self_managed_io_restart: None,
            evt_device_surprise_removal: None,
            evt_device_query_remove: None,
            evt_device_query_stop: None,
            evt_device_usage_notification: None,
            evt_device_relations_query: None,
            evt_device_usage_notification_ex: None,
        }
    }
}

enum_mapping! {
    pub enum PowerDeviceState: WDF_POWER_DEVICE_STATE {
        Invalid = WdfPowerDeviceInvalid,
        D0 = WdfPowerDeviceD0,
        D1 = WdfPowerDeviceD1,
        D2 = WdfPowerDeviceD2,
        D3 = WdfPowerDeviceD3,
        D3Final = WdfPowerDeviceD3Final,
        PrepareForHibernation = WdfPowerDevicePrepareForHibernation,
    }
}

enum_mapping! {
    pub enum SpecialFileType: WDF_SPECIAL_FILE_TYPE {
        Paging = WdfSpecialFilePaging,
        Hibernation = WdfSpecialFileHibernation,
        Dump = WdfSpecialFileDump,
        Boot = WdfSpecialFileBoot,
        PostDisplay = WdfSpecialFilePostDisplay,
        GuestAssigned = WdfSpecialFileGuestAssigned,
        // InlineCryptoEngine = WdfSpecialFileInlineCryptoEngine,
    }
}

enum_mapping! {
    pub enum DeviceRelationType: DEVICE_RELATION_TYPE {
        Bus = BusRelations,
        Ejection = EjectionRelations,
        Power = PowerRelations,
        Removal = RemovalRelations,
        TargetDevice = TargetDeviceRelation,
        SingleBus = SingleBusRelations,
        Transport = TransportRelations,
    }
}

impl From<&PnpPowerEventCallbacks> for WDF_PNPPOWER_EVENT_CALLBACKS {
    fn from(callbacks: &PnpPowerEventCallbacks) -> Self {
        let mut raw_callbacks = init_wdf_struct!(WDF_PNPPOWER_EVENT_CALLBACKS);

        if callbacks.evt_device_d0_entry.is_some() {
            raw_callbacks.EvtDeviceD0Entry = Some(__evt_device_d0_entry);
        }

        if callbacks
            .evt_device_d0_entry_post_interrupts_enabled
            .is_some()
        {
            raw_callbacks.EvtDeviceD0EntryPostInterruptsEnabled =
                Some(__evt_device_d0_entry_post_interrupts_enabled);
        }

        if callbacks.evt_device_d0_exit.is_some() {
            raw_callbacks.EvtDeviceD0Exit = Some(__evt_device_d0_exit);
        }

        if callbacks
            .evt_device_d0_exit_pre_interrupts_disabled
            .is_some()
        {
            raw_callbacks.EvtDeviceD0ExitPreInterruptsDisabled =
                Some(__evt_device_d0_exit_pre_interrupts_disabled);
        }

        if callbacks.evt_device_prepare_hardware.is_some() {
            raw_callbacks.EvtDevicePrepareHardware = Some(__evt_device_prepare_hardware);
        }

        if callbacks.evt_device_release_hardware.is_some() {
            raw_callbacks.EvtDeviceReleaseHardware = Some(__evt_device_release_hardware);
        }

        if callbacks.evt_device_self_managed_io_cleanup.is_some() {
            raw_callbacks.EvtDeviceSelfManagedIoCleanup =
                Some(__evt_device_self_managed_io_cleanup);
        }

        if callbacks.evt_device_self_managed_io_flush.is_some() {
            raw_callbacks.EvtDeviceSelfManagedIoFlush = Some(__evt_device_self_managed_io_flush);
        }

        if callbacks.evt_device_self_managed_io_init.is_some() {
            raw_callbacks.EvtDeviceSelfManagedIoInit = Some(__evt_device_self_managed_io_init);
        }

        if callbacks.evt_device_self_managed_io_suspend.is_some() {
            raw_callbacks.EvtDeviceSelfManagedIoSuspend =
                Some(__evt_device_self_managed_io_suspend);
        }

        if callbacks.evt_device_self_managed_io_restart.is_some() {
            raw_callbacks.EvtDeviceSelfManagedIoRestart =
                Some(__evt_device_self_managed_io_restart);
        }

        if callbacks.evt_device_surprise_removal.is_some() {
            raw_callbacks.EvtDeviceSurpriseRemoval = Some(__evt_device_surprise_removal);
        }

        if callbacks.evt_device_query_remove.is_some() {
            raw_callbacks.EvtDeviceQueryRemove = Some(__evt_device_query_remove);
        }

        if callbacks.evt_device_query_stop.is_some() {
            raw_callbacks.EvtDeviceQueryStop = Some(__evt_device_query_stop);
        }

        if callbacks.evt_device_usage_notification.is_some() {
            raw_callbacks.EvtDeviceUsageNotification = Some(__evt_device_usage_notification);
        }

        if callbacks.evt_device_relations_query.is_some() {
            raw_callbacks.EvtDeviceRelationsQuery = Some(__evt_device_relations_query);
        }

        if callbacks.evt_device_usage_notification_ex.is_some() {
            raw_callbacks.EvtDeviceUsageNotificationEx = Some(__evt_device_usage_notification_ex);
        }

        raw_callbacks
    }
}

macro_rules! unsafe_pnp_power_callback {
    // Main rule: params and optional return type (default to ())
    ($callback_name:ident($($param_name:ident: $param_type:ty => $conversion:expr),*) $(-> $return_type:tt)?) => {
        paste::paste! {
            pub extern "C" fn [<__ $callback_name>](device: WDFDEVICE $(, $param_name: $param_type)*) -> unsafe_pnp_power_callback!(@ret_type $($return_type)*) {
                let device = unsafe { &*(device as *const Device) };
                if let Some(ctxt) = DeviceContext::get(&device) {
                    if let Some(callbacks) = &ctxt.pnp_power_callbacks {
                        if let Some(callback) = callbacks.$callback_name {
                            return unsafe_pnp_power_callback!(@call_and_return $($return_type)*, callback(device $(, $conversion)*));
                        }
                    }
                }
                panic!("User did not provide callback {} but we subscribed to it", stringify!($callback_name));
            }
        }
    };
    // Helper: resolve return type, default to ()
    (@ret_type) => { () };
    (@ret_type $return_type:tt) => { $return_type };
    // Helper: call and return for NTSTATUS
    (@call_and_return NTSTATUS, $call:expr) => {
        match $call {
            Ok(_) => 0,
            Err(err) => err.code(),
        }
    };
    // Helper: call and return for ()
    (@call_and_return, $call:expr) => {
        $call
    };
    (@call_and_return (), $call:expr) => {
        $call
    };
}

unsafe_pnp_power_callback!(evt_device_d0_entry(previous_state: WDF_POWER_DEVICE_STATE => to_rust_power_state_enum(previous_state)) -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_d0_entry_post_interrupts_enabled(previous_state: WDF_POWER_DEVICE_STATE => to_rust_power_state_enum(previous_state)) -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_d0_exit(target_state: WDF_POWER_DEVICE_STATE => to_rust_power_state_enum(target_state)) -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_d0_exit_pre_interrupts_disabled(target_state: WDF_POWER_DEVICE_STATE => to_rust_power_state_enum(target_state)) -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_prepare_hardware(
    resources_raw: WDFCMRESLIST => unsafe { &*(resources_raw as *const CmResList) },
    resources_translated: WDFCMRESLIST => unsafe { &*(resources_translated as *const CmResList) }
) -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_release_hardware(
    resources_translated: WDFCMRESLIST => unsafe { &*(resources_translated as *const CmResList) }
) -> NTSTATUS);

unsafe_pnp_power_callback!(evt_device_self_managed_io_cleanup());
unsafe_pnp_power_callback!(evt_device_self_managed_io_flush());
unsafe_pnp_power_callback!(evt_device_self_managed_io_init() -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_self_managed_io_suspend() -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_self_managed_io_restart() -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_surprise_removal());
unsafe_pnp_power_callback!(evt_device_query_remove() -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_query_stop() -> NTSTATUS);
unsafe_pnp_power_callback!(evt_device_usage_notification(notification_type: WDF_SPECIAL_FILE_TYPE => to_rust_special_file_type_enum(notification_type), is_in_notification_path: BOOLEAN => is_in_notification_path == 1));
unsafe_pnp_power_callback!(evt_device_relations_query(relation_type: DEVICE_RELATION_TYPE => to_rust_device_relation_type_enum(relation_type)));
unsafe_pnp_power_callback!(evt_device_usage_notification_ex(notification_type: WDF_SPECIAL_FILE_TYPE => to_rust_special_file_type_enum(notification_type), is_in_notification_path: BOOLEAN => is_in_notification_path == 1) -> NTSTATUS);

fn to_rust_power_state_enum(state: WDF_POWER_DEVICE_STATE) -> PowerDeviceState {
    PowerDeviceState::try_from(state)
        .expect("framework should not send invalid WDF_POWER_DEVICE_STATE")
}

fn to_rust_special_file_type_enum(file_type: WDF_SPECIAL_FILE_TYPE) -> SpecialFileType {
    SpecialFileType::try_from(file_type)
        .expect("framework should not send invalid WDF_SPECIAL_FILE_TYPE")
}

fn to_rust_device_relation_type_enum(relation_type: DEVICE_RELATION_TYPE) -> DeviceRelationType {
    DeviceRelationType::try_from(relation_type)
        .expect("framework should not send invalid DEVICE_RELATION_TYPE")
}

pub struct DevicePowerPolicyIdleSettings {
    pub idle_caps: PowerPolicyS0IdleCapabilities,
    pub dx_state: DevicePowerState,
    pub idle_timeout: u32,
    pub user_control_of_idle_settings: PowerPolicyS0IdleUserControl,
    pub enabled: TriState,
    pub power_up_idle_device_on_system_wake: TriState,
    pub idle_timeout_type: PowerPolicyIdleTimeoutType,
    pub exclude_d3_cold: TriState,
}

impl DevicePowerPolicyIdleSettings {
    pub fn from_caps(caps: &PowerPolicyS0IdleCapabilities) -> Self {
        let mut obj = Self::default();
        obj.idle_caps = *caps;

        obj.dx_state = match caps {
            PowerPolicyS0IdleCapabilities::CanWakeFromS0
            | PowerPolicyS0IdleCapabilities::UsbSelectiveSuspend => DevicePowerState::Maximum,
            PowerPolicyS0IdleCapabilities::CannotWakeFromS0 => DevicePowerState::D3,
        };
        obj
    }
}

impl From<&DevicePowerPolicyIdleSettings> for WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS {
    fn from(settings: &DevicePowerPolicyIdleSettings) -> Self {
        let mut raw_settings = init_wdf_struct!(WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS);
        raw_settings.IdleCaps = settings.idle_caps.into();
        raw_settings.DxState = settings.dx_state.into();
        raw_settings.IdleTimeout = settings.idle_timeout;
        raw_settings.UserControlOfIdleSettings = settings.user_control_of_idle_settings.into();
        raw_settings.Enabled = settings.enabled.into();
        raw_settings.PowerUpIdleDeviceOnSystemWake =
            settings.power_up_idle_device_on_system_wake.into();
        raw_settings.IdleTimeoutType = settings.idle_timeout_type.into();
        raw_settings.ExcludeD3Cold = settings.exclude_d3_cold.into();

        raw_settings
    }
}

impl Default for DevicePowerPolicyIdleSettings {
    fn default() -> Self {
        Self {
            idle_caps: PowerPolicyS0IdleCapabilities::CannotWakeFromS0,
            dx_state: DevicePowerState::Maximum,
            idle_timeout: 0,
            user_control_of_idle_settings: PowerPolicyS0IdleUserControl::AllowUserControl,
            enabled: TriState::default(),
            power_up_idle_device_on_system_wake: TriState::default(),
            idle_timeout_type: PowerPolicyIdleTimeoutType::DriverManagedIdleTimeout,
            exclude_d3_cold: TriState::default(),
        }
    }
}

enum_mapping! {
    pub enum PowerPolicyS0IdleCapabilities: WDF_POWER_POLICY_S0_IDLE_CAPABILITIES {
        CannotWakeFromS0 = IdleCannotWakeFromS0,
        CanWakeFromS0 = IdleCanWakeFromS0,
        UsbSelectiveSuspend = IdleUsbSelectiveSuspend
    }
}

enum_mapping! {
    pub enum DevicePowerState: DEVICE_POWER_STATE {
        Unspecified = PowerDeviceUnspecified,
        D0 = PowerDeviceD0,
        D1 = PowerDeviceD1,
        D2 = PowerDeviceD2,
        D3 = PowerDeviceD3,
        Maximum = PowerDeviceMaximum
    }
}

enum_mapping! {
    pub enum PowerPolicyS0IdleUserControl: WDF_POWER_POLICY_S0_IDLE_USER_CONTROL {
        Invalid = IdleUserControlInvalid,
        DoNotAllowUserControl = IdleDoNotAllowUserControl,
        AllowUserControl = IdleAllowUserControl
    }
}

enum_mapping! {
    pub enum PowerPolicyIdleTimeoutType: WDF_POWER_POLICY_IDLE_TIMEOUT_TYPE {
        DriverManagedIdleTimeout = DriverManagedIdleTimeout,
        SystemManagedIdleTimeout = SystemManagedIdleTimeout,
        SystemManagedIdleTimeoutWithHint = SystemManagedIdleTimeoutWithHint
    }
}

pub struct DevicePowerPolicyWakeSettings {
    pub dx_state: DevicePowerState,
    pub user_control_of_wake_settings: PowerPolicySxWakeUserControl,
    pub enabled: TriState,
    pub arm_for_wake_if_children_are_armed_for_wake: bool,
    pub indicate_child_wake_on_parent_wake: bool,
}

impl From<&DevicePowerPolicyWakeSettings> for WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS {
    fn from(settings: &DevicePowerPolicyWakeSettings) -> Self {
        let mut raw_settings = init_wdf_struct!(WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS);
        raw_settings.DxState = settings.dx_state.into();
        raw_settings.UserControlOfWakeSettings = settings.user_control_of_wake_settings.into();
        raw_settings.Enabled = settings.enabled.into();
        raw_settings.ArmForWakeIfChildrenAreArmedForWake =
            settings.arm_for_wake_if_children_are_armed_for_wake.into();
        raw_settings.IndicateChildWakeOnParentWake =
            settings.indicate_child_wake_on_parent_wake.into();

        raw_settings
    }
}

impl Default for DevicePowerPolicyWakeSettings {
    fn default() -> Self {
        Self {
            dx_state: DevicePowerState::Maximum,
            user_control_of_wake_settings: PowerPolicySxWakeUserControl::AllowUserControl,
            enabled: TriState::default(),
            arm_for_wake_if_children_are_armed_for_wake: false,
            indicate_child_wake_on_parent_wake: false,
        }
    }
}

enum_mapping! {
    pub enum PowerPolicySxWakeUserControl: WDF_POWER_POLICY_SX_WAKE_USER_CONTROL {
        DoNotAllowUserControl = WakeDoNotAllowUserControl,
        AllowUserControl = WakeAllowUserControl
    }
}
