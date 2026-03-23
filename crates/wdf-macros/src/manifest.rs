// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trace manifest infrastructure — PE binary `.trman` section reader + atexit
//! PDB patcher.
//!
//! Instead of accumulating trace entries in proc-macro memory and writing
//! intermediate TMF files, this module reads the final linked binary's
//! `.trman` PE section at `atexit` time.  Each `trace!` expansion embeds a
//! fixed-size [`TraceManifestRecord`](wdf::__internal::TraceManifestRecord)
//! into `.trman$m` via `#[link_section]`.  The MSVC linker merges all
//! `$`-suffixed subsections into a single `.trman` section.
//!
//! ## Flow
//!
//! 1. `#[driver_entry]` stores the control GUID and driver name, then
//!    registers the `atexit` callback.
//! 2. `trace!` generates `#[link_section = ".trman$m"]` statics (no
//!    proc-macro state accumulation).
//! 3. When `rustc` exits, the atexit handler:
//!    a. Finds the PDB that was just produced.
//!    b. Finds the companion binary (DLL/SYS) next to the PDB.
//!    c. Reads the `.trman` PE section from the binary.
//!    d. Parses fixed-size `TraceManifestRecord` entries (skipping sentinels).
//!    e. Converts them to `TraceManifestEntry` values.
//!    f. Delegates to `pdb_annotation::patch_pdb_with_annotations`.

use std::path::{Path, PathBuf};
use std::sync::{Mutex, Once};

use crate::pdb_annotation;

// ===========================================================================
// Public types (consumed by lib.rs and pdb_annotation.rs)
// ===========================================================================

/// Information about a single trace control definition (GUID + flags).
#[derive(Clone)]
pub(crate) struct TraceControlInfo {
    /// The control GUID string.
    pub guid: String,
    /// Flag names associated with this control.
    pub flags: Vec<String>,
}

/// A single trace argument's metadata.
#[derive(Debug, Clone)]
pub(crate) struct TraceManifestArg {
    /// Argument name.
    pub name: String,
    /// WPP item type (e.g. `"ItemLong"`, `"ItemString"`).
    pub item_type: String,
    /// 1-based parameter number.
    pub param_number: usize,
}

/// A single trace entry's metadata (built from PE section records).
#[derive(Debug, Clone)]
pub(crate) struct TraceManifestEntry {
    /// Unique message ID for this trace point.
    pub message_id: u32,
    /// Source file path.
    pub source_file: String,
    /// Line number in the source file.
    pub line_number: usize,
    /// Optional WPP flag name.
    pub flag: Option<String>,
    /// Optional WPP trace level name.
    pub level: Option<String>,
    /// Rust format string.
    pub format_string: String,
    /// Trace argument descriptors.
    pub args: Vec<TraceManifestArg>,
    /// Crate/driver name.
    pub driver_name: String,
}

// ===========================================================================
// Static state (set by #[driver_entry])
// ===========================================================================

/// Trace control definitions (GUID + flags) from `#[driver_entry]`.
pub(crate) static TRACE_CONTROL_INFO: Mutex<Vec<TraceControlInfo>> = Mutex::new(Vec::new());

/// Driver/crate name from `#[driver_entry]`.
pub(crate) static DRIVER_NAME: Mutex<String> = Mutex::new(String::new());

/// Ensures the atexit flusher is registered exactly once per proc-macro load.
static FLUSHER_REGISTERED: Once = Once::new();

// ===========================================================================
// C atexit binding
// ===========================================================================

unsafe extern "C" {
    fn atexit(callback: extern "C" fn()) -> std::os::raw::c_int;
}

/// Registers the atexit callback.  Called once from `#[driver_entry]`.
pub(crate) fn ensure_flusher_registered() {
    FLUSHER_REGISTERED.call_once(|| {
        // SAFETY: `atexit` is a standard C library function provided by the CRT.
        // The proc-macro runs inside the compiler process, which links against
        // the CRT.  The callback fires when the compiler process exits — after
        // the linker has produced the binary and PDB.
        unsafe {
            atexit(atexit_flush_manifest);
        }
    });
}

extern "C" fn atexit_flush_manifest() {
    if let Err(e) = std::panic::catch_unwind(flush_manifest) {
        eprintln!("wdf-macros: [manifest] Panic during flush: {e:?}");
    }
}

// ===========================================================================
// WPP format string generation
// ===========================================================================

/// Maps a WPP item type name to a C-style printf format specifier.
fn item_type_to_format_spec(item_type: &str) -> &'static str {
    match item_type {
        "ItemLong" => "d",
        "ItemULong" => "u",
        "ItemLongLong" => "I64d",
        "ItemULongLong" => "I64u",
        "ItemString" => "s",
        _ => "x",
    }
}

/// Converts a Rust-style format string and argument metadata into a WPP TMF
/// format string with numbered placeholders.
///
/// Example: `"hello {} world {}"` with two args → `"hello %10!d! world %11!s!"`
pub(crate) fn generate_tmf_format_string_pub(
    format_str: &str,
    args: &[TraceManifestArg],
) -> String {
    let mut result = String::new();
    let mut arg_idx = 0;
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                // Escaped {{ → literal {
                chars.next();
                result.push('{');
            } else {
                // Format placeholder — skip to closing }
                while let Some(inner) = chars.next() {
                    if inner == '}' {
                        break;
                    }
                }
                if arg_idx < args.len() {
                    let param_num = arg_idx + 1;
                    let spec = item_type_to_format_spec(&args[arg_idx].item_type);
                    result.push_str(&format!("%{param_num}!{spec}!"));
                    arg_idx += 1;
                }
            }
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
                // Escaped }} → literal }
                chars.next();
                result.push('}');
            } else {
                result.push(c);
            }
        } else {
            result.push(c);
        }
    }

    result
}

// ===========================================================================
// PE .trman section reader
// ===========================================================================

// Record layout constants — **must** match `wdf::__internal::trace_manifest`.
const TRACE_MANIFEST_MAGIC: u32 = 0x5452_4D4E; // "TRMN"
const TRACE_MANIFEST_SENTINEL_START: u32 = 0xAAAA_AAAA;
const TRACE_MANIFEST_SENTINEL_END: u32 = 0x5A5A_5A5A;
const TRACE_MANIFEST_VERSION: u32 = 1;

const MAX_CRATE_NAME: usize = 64;
const MAX_FILE_NAME: usize = 256;
const MAX_FORMAT_STR: usize = 512;
const MAX_ARG_NAME: usize = 32;
const MAX_ITEM_TYPE: usize = 24;
const MAX_ARGS: usize = 16;
const MAX_FLAG_NAME: usize = 32;
const MAX_LEVEL_NAME: usize = 16;

/// Size of one `TraceArgRecord` in the PE section (no hidden padding in
/// `#[repr(C)]` since all u32 fields land on 4-byte boundaries).
const ARG_RECORD_SIZE: usize = MAX_ARG_NAME + MAX_ITEM_TYPE + 4 + 4; // 64

/// Size of one `TraceManifestRecord` in the PE section.
const RECORD_SIZE: usize = 24 // magic(4) + version(4) + message_id(4) + line_number(4) + arg_count(4) + _reserved(4)
    + MAX_CRATE_NAME   //  64
    + MAX_FILE_NAME    // 256
    + MAX_FORMAT_STR   // 512
    + MAX_FLAG_NAME    //  32
    + MAX_LEVEL_NAME   //  16
    + MAX_ARGS * ARG_RECORD_SIZE; // 1024  → total 1928

/// Reads a `u32` from little-endian bytes at `offset`.
fn read_u32(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([
        data[offset],
        data[offset + 1],
        data[offset + 2],
        data[offset + 3],
    ])
}

/// Reads a `u16` from little-endian bytes at `offset`.
fn read_u16(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([data[offset], data[offset + 1]])
}

/// Extracts a NUL-terminated UTF-8 string from a fixed-size byte buffer.
fn str_from_fixed(buf: &[u8]) -> String {
    let end = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    String::from_utf8_lossy(&buf[..end]).to_string()
}

/// Reads the raw bytes of the `.trman` PE section from a binary file.
fn read_trman_section_bytes(
    binary_path: &Path,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let data = std::fs::read(binary_path)?;

    // --- DOS header ---
    if data.len() < 64 || data[0] != b'M' || data[1] != b'Z' {
        return Err("Not a valid PE file (bad MZ signature)".into());
    }
    let pe_offset = read_u32(&data, 0x3C) as usize;
    if pe_offset + 24 > data.len() {
        return Err("PE header offset out of bounds".into());
    }

    // --- PE signature ---
    if &data[pe_offset..pe_offset + 4] != b"PE\0\0" {
        return Err("Not a valid PE file (bad PE signature)".into());
    }

    // --- COFF header ---
    let num_sections = read_u16(&data, pe_offset + 6) as usize;
    let optional_header_size = read_u16(&data, pe_offset + 20) as usize;

    // Section table immediately follows PE sig (4) + COFF header (20) +
    // optional header.
    let section_table_offset = pe_offset + 24 + optional_header_size;

    // --- Scan section headers for ".trman" ---
    for i in 0..num_sections {
        let sh_offset = section_table_offset + i * 40;
        if sh_offset + 40 > data.len() {
            break;
        }

        let name = &data[sh_offset..sh_offset + 8];
        // ".trman" is 6 bytes; remaining 2 must be NUL.
        if name.starts_with(b".trman") && name[6..].iter().all(|&b| b == 0) {
            let raw_data_size = read_u32(&data, sh_offset + 16) as usize;
            let raw_data_ptr = read_u32(&data, sh_offset + 20) as usize;

            if raw_data_ptr + raw_data_size > data.len() {
                return Err(format!(
                    ".trman section raw data [{:#x}..{:#x}] exceeds file size ({:#x})",
                    raw_data_ptr,
                    raw_data_ptr + raw_data_size,
                    data.len()
                )
                .into());
            }

            return Ok(data[raw_data_ptr..raw_data_ptr + raw_data_size].to_vec());
        }
    }

    Err("No .trman section found in PE binary".into())
}

/// Parses `TraceManifestEntry` records from raw `.trman` section bytes.
///
/// Sentinel records (start/end) and records with bad magic/version are
/// silently skipped.
fn parse_trman_records(section_data: &[u8]) -> Vec<TraceManifestEntry> {
    let mut entries = Vec::new();
    let num_records = section_data.len() / RECORD_SIZE;

    for i in 0..num_records {
        let base = i * RECORD_SIZE;
        if base + RECORD_SIZE > section_data.len() {
            break;
        }

        let rec = &section_data[base..base + RECORD_SIZE];
        let magic = read_u32(rec, 0);
        let version = read_u32(rec, 4);

        // Skip sentinels
        if magic == TRACE_MANIFEST_SENTINEL_START
            || magic == TRACE_MANIFEST_SENTINEL_END
        {
            continue;
        }
        // Skip invalid records
        if magic != TRACE_MANIFEST_MAGIC || version != TRACE_MANIFEST_VERSION {
            continue;
        }

        let message_id = read_u32(rec, 8);
        let line_number = read_u32(rec, 12) as usize;
        let arg_count = (read_u32(rec, 16) as usize).min(MAX_ARGS);

        // Field offsets — matching #[repr(C)] layout.  No hidden padding
        // because all u32 fields land on 4-byte boundaries after byte-array
        // fields whose sizes are multiples of 4.
        let off_crate = 24;
        let off_file = off_crate + MAX_CRATE_NAME; //  88
        let off_fmt = off_file + MAX_FILE_NAME; // 344
        let off_flag = off_fmt + MAX_FORMAT_STR; // 856
        let off_level = off_flag + MAX_FLAG_NAME; // 888
        let off_args = off_level + MAX_LEVEL_NAME; // 904

        let crate_name =
            str_from_fixed(&rec[off_crate..off_crate + MAX_CRATE_NAME]);
        let source_file =
            str_from_fixed(&rec[off_file..off_file + MAX_FILE_NAME]);
        let format_string =
            str_from_fixed(&rec[off_fmt..off_fmt + MAX_FORMAT_STR]);
        let flag_name =
            str_from_fixed(&rec[off_flag..off_flag + MAX_FLAG_NAME]);
        let level_name =
            str_from_fixed(&rec[off_level..off_level + MAX_LEVEL_NAME]);

        let mut args = Vec::with_capacity(arg_count);
        for j in 0..arg_count {
            let arg_base = off_args + j * ARG_RECORD_SIZE;
            let name =
                str_from_fixed(&rec[arg_base..arg_base + MAX_ARG_NAME]);
            let item_type = str_from_fixed(
                &rec[arg_base + MAX_ARG_NAME
                    ..arg_base + MAX_ARG_NAME + MAX_ITEM_TYPE],
            );
            let param_number =
                read_u32(rec, arg_base + MAX_ARG_NAME + MAX_ITEM_TYPE) as usize;

            args.push(TraceManifestArg {
                name,
                item_type,
                param_number,
            });
        }

        entries.push(TraceManifestEntry {
            message_id,
            source_file,
            line_number,
            flag: if flag_name.is_empty() {
                None
            } else {
                Some(flag_name)
            },
            level: if level_name.is_empty() {
                None
            } else {
                Some(level_name)
            },
            format_string,
            args,
            driver_name: crate_name,
        });
    }

    entries
}

// ===========================================================================
// Binary discovery
// ===========================================================================

/// Finds the linked binary (DLL/SYS/EXE) next to a PDB file.
fn find_binary_near_pdb(pdb_path: &Path, driver_name: &str) -> Option<PathBuf> {
    let dir = pdb_path.parent()?;
    let stem = pdb_path.file_stem()?.to_str()?;

    // Try the PDB stem first (most likely match), then the driver name.
    for name in [stem, driver_name] {
        for ext in ["dll", "sys", "exe"] {
            let candidate = dir.join(format!("{name}.{ext}"));
            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }

    None
}

// ===========================================================================
// Atexit flush handler
// ===========================================================================

/// Main atexit callback.  Reads the binary's `.trman` section, converts the
/// records to `TraceManifestEntry` values, and patches the PDB.
fn flush_manifest() {
    let driver_name = DRIVER_NAME.lock().unwrap().clone();
    let controls = TRACE_CONTROL_INFO.lock().unwrap().clone();

    if driver_name.is_empty() || controls.is_empty() {
        return;
    }

    let control_guid = &controls[0].guid;

    eprintln!(
        "wdf-macros: [manifest] Flushing trace manifest for '{}' (GUID: {})",
        driver_name, control_guid,
    );

    // --- Find PDB ---
    let pdb_path = match pdb_annotation::find_recent_pdb(
        &PathBuf::new(), // scratch_dir unused in current implementation
        &driver_name,
    ) {
        Some(p) => p,
        None => {
            eprintln!(
                "wdf-macros: [manifest] No recent PDB found for '{}'",
                driver_name,
            );
            return;
        }
    };

    eprintln!(
        "wdf-macros: [manifest] Found PDB: {}",
        pdb_path.display(),
    );

    // --- Find binary ---
    let binary_path = match find_binary_near_pdb(&pdb_path, &driver_name) {
        Some(p) => p,
        None => {
            eprintln!(
                "wdf-macros: [manifest] No binary found near PDB '{}'",
                pdb_path.display(),
            );
            return;
        }
    };

    eprintln!(
        "wdf-macros: [manifest] Found binary: {}",
        binary_path.display(),
    );

    // --- Read .trman section ---
    let section_data = match read_trman_section_bytes(&binary_path) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("wdf-macros: [manifest] Failed to read .trman section: {e}");
            return;
        }
    };

    eprintln!(
        "wdf-macros: [manifest] Read .trman section: {} bytes ({} potential records)",
        section_data.len(),
        section_data.len() / RECORD_SIZE,
    );

    // --- Parse records ---
    let mut entries = parse_trman_records(&section_data);
    entries.sort_by_key(|e| e.message_id);

    eprintln!(
        "wdf-macros: [manifest] Parsed {} trace entries from binary",
        entries.len(),
    );

    if entries.is_empty() {
        return;
    }

    // --- Patch PDB ---
    match pdb_annotation::patch_pdb_with_annotations(
        &pdb_path,
        &entries,
        control_guid,
        &driver_name,
    ) {
        Ok(()) => {
            eprintln!(
                "wdf-macros: [manifest] PDB patched successfully with {} S_ANNOTATION records",
                entries.len(),
            );
        }
        Err(e) => {
            eprintln!("wdf-macros: [manifest] PDB patching failed: {e}");
        }
    }
}
