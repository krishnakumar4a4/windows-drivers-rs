use wdk_sys::NTSTATUS;

pub struct NtError(NTSTATUS);

impl NtError {
    pub fn from_ntstatus(status: NTSTATUS) -> Self {
        Self(status)
    }

    pub fn ntstatus(&self) -> NTSTATUS {
        self.0
    }
}

impl From<NTSTATUS> for NtError {
    fn from(status: NTSTATUS) -> Self {
        Self(status)
    }
}
