// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trace manifest record types and PE section-based distributed collection.
//!
//! Each `trace!` macro expansion generates a const static `TraceManifestRecord`
//! placed into a dedicated PE section (`.trman$m`) via `#[link_section]`.
//! At link time, the MSVC linker merges all `$`-suffixed subsections into a
//! single contiguous `.trman` section:
//!
//! ```text
//! .trman$a  →  [START sentinel]     (defined in driver crate via #[driver_entry])
//! .trman$m  →  [record1][record2]…  (from all crates, via trace! macro)
//! .trman$z  →  [END sentinel]       (defined in driver crate via #[driver_entry])
//! ```
//!
//! The driver crate's proc-macro atexit handler patches the PDB with
//! S_ANNOTATION symbols. The `.trman` section in the final binary serves
//! as an embedded, self-describing manifest of all trace points.

// ---------------------------------------------------------------------------
// Fixed-size buffer constants
// ---------------------------------------------------------------------------

/// Maximum length of a crate/driver name.
pub const MAX_CRATE_NAME: usize = 64;
/// Maximum length of a source file path.
pub const MAX_FILE_NAME: usize = 256;
/// Maximum length of a format string.
pub const MAX_FORMAT_STR: usize = 512;
/// Maximum length of an argument name.
pub const MAX_ARG_NAME: usize = 32;
/// Maximum length of an item type name (e.g. "ItemLong").
pub const MAX_ITEM_TYPE: usize = 24;
/// Maximum number of trace arguments per entry.
pub const MAX_ARGS: usize = 16;
/// Maximum length of a flag name.
pub const MAX_FLAG_NAME: usize = 32;
/// Maximum length of a level name.
pub const MAX_LEVEL_NAME: usize = 16;

/// Magic value to identify valid records.
pub const TRACE_MANIFEST_MAGIC: u32 = 0x5452_4D4E; // "TRMN"
/// Magic value for the start sentinel.
pub const TRACE_MANIFEST_SENTINEL_START: u32 = 0xAAAA_AAAA;
/// Magic value for the end sentinel.
pub const TRACE_MANIFEST_SENTINEL_END: u32 = 0x5A5A_5A5A;

/// Current record version.
pub const TRACE_MANIFEST_VERSION: u32 = 1;

// ---------------------------------------------------------------------------
// Trace argument descriptor
// ---------------------------------------------------------------------------

/// A single trace argument's metadata (fixed-size, const-compatible).
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TraceArgRecord {
    /// Argument name (NUL-padded).
    pub name: [u8; MAX_ARG_NAME],
    /// WPP item type (e.g. "ItemLong", NUL-padded).
    pub item_type: [u8; MAX_ITEM_TYPE],
    /// 1-based parameter number.
    pub param_number: u32,
    /// Reserved padding for alignment.
    pub _pad: u32,
}

impl TraceArgRecord {
    /// Creates a zeroed (empty) argument record.
    pub const fn zeroed() -> Self {
        Self {
            name: [0u8; MAX_ARG_NAME],
            item_type: [0u8; MAX_ITEM_TYPE],
            param_number: 0,
            _pad: 0,
        }
    }
}

// ---------------------------------------------------------------------------
// Trace manifest record
// ---------------------------------------------------------------------------

/// Fixed-size metadata for a single `trace!` invocation.
///
/// Each record is placed into the `.trman$m` PE section via
/// `#[link_section = ".trman$m"]`. At link time, the MSVC linker merges
/// all `.trman$a`, `.trman$m`, `.trman$z` subsections into a single
/// contiguous `.trman` section in the final binary.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TraceManifestRecord {
    /// Magic value — must be `TRACE_MANIFEST_MAGIC` for real records.
    /// Start sentinel uses `TRACE_MANIFEST_SENTINEL_START`.
    /// End sentinel uses `TRACE_MANIFEST_SENTINEL_END`.
    pub magic: u32,
    /// Record version — must be `TRACE_MANIFEST_VERSION`.
    pub version: u32,
    /// Unique message ID for this trace point.
    pub message_id: u32,
    /// Line number in the source file.
    pub line_number: u32,
    /// Number of arguments (0..MAX_ARGS).
    pub arg_count: u32,
    /// Reserved for future use.
    pub _reserved: u32,
    /// Crate/driver name (NUL-padded).
    pub crate_name: [u8; MAX_CRATE_NAME],
    /// Source file path (NUL-padded).
    pub source_file: [u8; MAX_FILE_NAME],
    /// Rust format string (NUL-padded).
    pub format_string: [u8; MAX_FORMAT_STR],
    /// Flag name (NUL-padded), empty if none.
    pub flag_name: [u8; MAX_FLAG_NAME],
    /// Level name (NUL-padded), empty if none.
    pub level_name: [u8; MAX_LEVEL_NAME],
    /// Argument descriptors (unused slots are zeroed).
    pub args: [TraceArgRecord; MAX_ARGS],
}

impl TraceManifestRecord {
    /// Creates a zeroed record (not valid — used as a base for const init).
    pub const fn zeroed() -> Self {
        Self {
            magic: 0,
            version: 0,
            message_id: 0,
            line_number: 0,
            arg_count: 0,
            _reserved: 0,
            crate_name: [0u8; MAX_CRATE_NAME],
            source_file: [0u8; MAX_FILE_NAME],
            format_string: [0u8; MAX_FORMAT_STR],
            flag_name: [0u8; MAX_FLAG_NAME],
            level_name: [0u8; MAX_LEVEL_NAME],
            args: [TraceArgRecord::zeroed(); MAX_ARGS],
        }
    }

    /// Creates a sentinel record with the given magic value.
    pub const fn sentinel(magic: u32) -> Self {
        let mut r = Self::zeroed();
        r.magic = magic;
        r.version = TRACE_MANIFEST_VERSION;
        r
    }

    /// Returns `true` if this record has a valid magic and version.
    pub const fn is_valid(&self) -> bool {
        self.magic == TRACE_MANIFEST_MAGIC && self.version == TRACE_MANIFEST_VERSION
    }

    /// Returns `true` if this is a sentinel (start or end).
    pub const fn is_sentinel(&self) -> bool {
        self.magic == TRACE_MANIFEST_SENTINEL_START
            || self.magic == TRACE_MANIFEST_SENTINEL_END
    }
}

/// Copies a string literal into a fixed-size byte array at const time.
///
/// This is used by proc-macro generated code to populate
/// `TraceManifestRecord` fields.
pub const fn fixed_str<const N: usize>(s: &str) -> [u8; N] {
    let mut buf = [0u8; N];
    let bytes = s.as_bytes();
    let len = if bytes.len() < N { bytes.len() } else { N - 1 };
    let mut i = 0;
    while i < len {
        buf[i] = bytes[i];
        i += 1;
    }
    buf
}

/// Extracts a NUL-terminated string from a fixed-size byte buffer.
///
/// Returns the string up to the first NUL byte (or the full buffer if
/// no NUL is found).
pub fn str_from_fixed(buf: &[u8]) -> &str {
    let end = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    core::str::from_utf8(&buf[..end]).unwrap_or("")
}
