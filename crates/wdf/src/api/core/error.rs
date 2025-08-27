use wdk_sys::{NT_INFORMATION, NT_WARNING, NT_ERROR};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NtStatus {
    Success(i32),
    Informational(i32),
    Warning(i32),
    Error(NtStatusError),
}

impl NtStatus {
    pub fn from(code: i32) -> Self {
        if NT_INFORMATION(code) {
            Self::Informational(code)
        } else if NT_WARNING(code) {
            Self::Warning(code)
        } else if NT_ERROR(code) {
            Self::Error(NtStatusError::from(code))
        } else {
            // We don't use NT_SUCCESS because it returns
            // TRUE For both success and informational types.
            // Instead we select `Success` let all the other
            // checks above have returned FALSE
            // See this page for details:
            // https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/using-ntstatus-values
            Self::Success(code)
        }
    }

    /// Return the raw NTSTATUS value for this typed status.
    pub fn code(&self) -> i32 {
        match *self {
            Self::Success(c) => c,
            Self::Informational(c) => c,
            Self::Warning(c) => c,
            Self::Error(e) => e.code(),
        }
    }

    pub fn is_success(&self) -> bool {
        match *self {
            Self::Success(_) | Self::Informational(_) => true,
            _ => false,
        }
    }
}

impl From<i32> for NtStatus {
    fn from(code: i32) -> Self {
        Self::from(code)
    }
}


// The actual error sub-enum. It contains a few common errors and Unknown for any
// other error values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NtStatusError {
    InvalidParameter,
    Cancelled,
    Other(i32),
}

impl NtStatusError {
    const INVALID_PARAMETER: i32 = 0xC000000D_u32 as i32;
    const CANCELLED: i32 = 0xC0000120_u32 as i32;

    pub fn from(code: i32) -> Self {
        if !NT_ERROR(code) {
            panic!("NtStatusError::fromcalled with non-error code: {:#X}", code);
        }

        match code {
            Self::INVALID_PARAMETER => Self::InvalidParameter,
            Self::CANCELLED => Self::Cancelled,
            _ => Self::Other(code),
        }
    }

    pub const fn code(&self) -> i32 {
        match *self {
            Self::InvalidParameter => Self::INVALID_PARAMETER,
            Self::Cancelled => Self::CANCELLED,
            Self::Other(v) => v,
        }
    }
}

impl From<i32> for NtStatusError {
    fn from(code: i32) -> Self {
        Self::from(code)
    }
}

impl Into<i32> for NtStatusError {
    fn into(self) -> i32 {
        self.code()
    }
}

pub type NtResult<T> = Result<T, NtStatusError>;