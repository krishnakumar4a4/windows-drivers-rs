use wdk_sys::NTSTATUS;

// TODO: this needs a different design as per the
// description here: https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/87fba13e-bf06-450e-83b1-9241dc81e781

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NtStatus {
    Success,
    Error(NtError),
}

impl NtStatus {
    pub fn nt_status(&self) -> NTSTATUS {
        match self {
            Self::Success => 0,
            Self::Error(err) => err.nt_status(),
        }
    }

    pub fn cancelled() -> Self {
        Self::Error(NtError(0xC0000120_u32 as NTSTATUS))
    }
}

impl From<NTSTATUS> for NtStatus {
    fn from(status: NTSTATUS) -> Self {
        match status {
            0 => Self::Success,
            _ => Self::Error(NtError::from(status)),
        }
    }
}
