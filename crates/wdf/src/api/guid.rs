use wdk_sys::GUID;

#[derive(Debug)]
pub struct Guid(GUID);

impl Guid {
     pub fn parse(guid_str: &str) -> Result<Self, &'static str> {
        // Remove dashes from the input string
        let guid_str = guid_str.replace("-", "");

        let err = "Invalid GUID format";

        if guid_str.len() != 32 {
            return Err(err);
        }

        let data1 = u32::from_str_radix(&guid_str[0..8], 16).map_err(|_| err)?;
        let data2 = u16::from_str_radix(&guid_str[8..12], 16).map_err(|_| err)?;
        let data3 = u16::from_str_radix(&guid_str[12..16], 16).map_err(|_| err)?;

        let mut data4 = [0u8; 8];
        for i in 0..8 {
            data4[i] = u8::from_str_radix(&guid_str[16 + i * 2..18 + i * 2], 16).map_err(|_| err)?;
        }

        Ok(Guid(GUID {
                Data1: data1,
                Data2: data2,
                Data3: data3,
                Data4: data4,
            },
        ))
    }

    pub fn as_lpcguid(&self) -> *const GUID {
        &self.0
    }
}

