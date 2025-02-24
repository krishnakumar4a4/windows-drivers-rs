use wdk_sys::NTSTATUS;

pub struct NtError(NTSTATUS);

impl NtError {
    pub fn nt_status(&self) -> NTSTATUS {
        self.0
    }
}

impl From<NTSTATUS> for NtError {
    fn from(status: NTSTATUS) -> Self {
        Self(status)
    }
}

pub type NtResult<T> = Result<T, NtError>;


pub enum NtStatus {
    Success,
    Error(NtError)
}

impl NtStatus {
    pub fn nt_status(&self) -> NTSTATUS {
        match self {
            Self::Success => 0,
            Self::Error(err) => err.nt_status()
        }
    }
}

impl From<NTSTATUS> for NtStatus {
    fn from(status: NTSTATUS) -> Self {
        match status {
            0 => Self::Success,
            _ => Self::Error(NtError::from(status))
        }
    }
}

