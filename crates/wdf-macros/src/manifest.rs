// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP manifest (TMF) file generation for `trace!` macro metadata.
//!
//! ## Architecture
//!
//! `rustc` re-expands **every** proc macro on every compilation — even during
//! incremental builds.  This means the in-memory `TRACE_ENTRIES` map always
//! holds the complete, up-to-date set of trace entries for the entire crate
//! after macro expansion finishes.
//!
//! We leverage this by writing the final `.tmf` file directly from
//! `TRACE_ENTRIES` in a single `atexit` handler — no intermediate partial
//! files, no dep-info parsing, no stale-entry pruning.
//!
//! ## Flush strategy
//!
//! Each `trace!` expansion accumulates its entry into `TRACE_ENTRIES`
//! (a `HashMap<String, Vec<TraceManifestEntry>>` keyed by source file).
//! The first expansion also registers an `atexit` handler via
//! [`ensure_flusher_registered`].  When `rustc` exits, the handler
//! collects all entries, sorts by `message_id`, generates TMF content,
//! and writes the `.tmf` file under a file-system lock.

use std::collections::HashMap;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    LazyLock, Mutex, Once,
};

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

/// A single argument in a trace manifest entry.
#[derive(Debug, Clone)]
pub(crate) struct TraceManifestArg {
    pub name: String,
    pub item_type: String,
    pub param_number: usize,
}

/// Metadata for a single `trace!` invocation.
#[derive(Debug, Clone)]
pub(crate) struct TraceManifestEntry {
    pub message_id: u32,
    pub source_file: String,
    pub line_number: usize,
    pub flag: Option<String>,
    pub level: Option<String>,
    pub format_string: String,
    pub args: Vec<TraceManifestArg>,
    pub driver_name: String,
}

/// Trace control information (GUID + flags) captured from `#[driver_entry]`.
#[derive(Debug, Clone)]
pub(crate) struct TraceControlInfo {
    pub guid: String,
    pub flags: Vec<String>,
}

// ---------------------------------------------------------------------------
// Global statics
// ---------------------------------------------------------------------------

/// Trace entries keyed by source file name.
pub(crate) static TRACE_ENTRIES: LazyLock<Mutex<HashMap<String, Vec<TraceManifestEntry>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Trace control definitions captured from `#[driver_entry]`.
pub(crate) static TRACE_CONTROL_INFO: Mutex<Vec<TraceControlInfo>> = Mutex::new(Vec::new());

/// Prevents double-flushing.
static MANIFEST_FLUSHED: AtomicBool = AtomicBool::new(false);

// ---------------------------------------------------------------------------
// TMF generation helpers
// ---------------------------------------------------------------------------

fn item_type_to_format_specifier(item_type: &str) -> &'static str {
    match item_type {
        "ItemLong" => "d",
        "ItemULong" => "u",
        "ItemLongLong" => "I64d",
        "ItemULongLong" => "I64u",
        "ItemString" => "s",
        _ => "d",
    }
}

fn generate_tmf_format_string(format_str: &str, args: &[TraceManifestArg]) -> String {
    let mut result = String::new();
    let mut arg_idx = 0;
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                chars.next();
                result.push('{');
            } else {
                while let Some(inner) = chars.next() {
                    if inner == '}' {
                        break;
                    }
                }
                if arg_idx < args.len() {
                    let param_num = arg_idx + 1;
                    let fmt_spec = item_type_to_format_specifier(&args[arg_idx].item_type);
                    result.push_str(&format!("%{param_num}!{fmt_spec}!"));
                    arg_idx += 1;
                }
            }
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
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

fn generate_tmf_content(
    entries: &[TraceManifestEntry],
    controls: &[TraceControlInfo],
) -> String {
    if entries.is_empty() {
        return String::new();
    }

    let control_guid = controls
        .first()
        .map(|c| c.guid.as_str())
        .unwrap_or("00000000-0000-0000-0000-000000000000");

    let mut tmf = String::new();

    for entry in entries {
        tmf.push_str(&format!(
            "{} {} // SRC={} MJ= MN=\n",
            control_guid, entry.driver_name, entry.source_file
        ));

        let wpp_fmt = generate_tmf_format_string(&entry.format_string, &entry.args);

        let mut comment_parts = Vec::new();
        if let Some(ref level) = entry.level {
            comment_parts.push(level.clone());
        }
        if let Some(ref flag) = entry.flag {
            comment_parts.push(flag.clone());
        }
        let comment = if comment_parts.is_empty() {
            String::new()
        } else {
            comment_parts.join(" ")
        };

        tmf.push_str(&format!(
            "#typev {}_{} {} \"%0{}\" // {}\n",
            entry.driver_name, entry.line_number, entry.message_id, wpp_fmt, comment
        ));

        if !entry.args.is_empty() {
            tmf.push_str("{\n");
            for arg in &entry.args {
                tmf.push_str(&format!(
                    "  {}, {} -- {}\n",
                    arg.name, arg.item_type, arg.param_number
                ));
            }
            tmf.push_str("}\n");
        }

        tmf.push('\n');
    }

    tmf
}

// ---------------------------------------------------------------------------
// Flush logic
// ---------------------------------------------------------------------------

/// Writes the final `.tmf` manifest directly from in-memory `TRACE_ENTRIES`.
///
/// Because `rustc` re-expands every proc macro on every compilation,
/// `TRACE_ENTRIES` always contains the complete set of trace entries for the
/// crate.  No partial files or dep-info parsing needed.
fn flush_manifest(caller: &str) {
    if MANIFEST_FLUSHED.swap(true, Ordering::SeqCst) {
        return;
    }

    eprintln!("wdf-macros: manifest flush invoked by {caller}");

    let entries = TRACE_ENTRIES.lock().unwrap();
    let controls = TRACE_CONTROL_INFO.lock().unwrap();

    if entries.is_empty() {
        eprintln!("wdf-macros: no trace entries accumulated — skipping TMF generation");
        return;
    }

    // Collect all entries across all source files, sorted by message_id
    let mut all_entries: Vec<TraceManifestEntry> = entries
        .values()
        .flatten()
        .cloned()
        .collect();
    all_entries.sort_by_key(|e| e.message_id);

    let driver_name = all_entries
        .first()
        .map(|e| e.driver_name.clone())
        .unwrap_or_else(|| {
            std::env::var("CARGO_PKG_NAME")
                .unwrap_or_else(|_| "unknown_driver".to_string())
                .replace('-', "_")
        });

    let tmf_content = generate_tmf_content(&all_entries, &controls);

    if tmf_content.is_empty() {
        return;
    }

    let scratch_dir = scratch::path("wdf_macros_wpp");
    let tmf_path = scratch_dir.join(format!("{driver_name}.tmf"));

    let lock_path = scratch_dir.join(".wpp_manifest.lock");
    match std::fs::File::create(&lock_path) {
        Ok(lock_file) => {
            use fs4::fs_std::FileExt;
            if FileExt::lock_exclusive(&lock_file).is_ok() {
                match std::fs::write(&tmf_path, &tmf_content) {
                    Ok(()) => {
                        let mut files: Vec<&str> = all_entries
                            .iter()
                            .map(|e| e.source_file.as_str())
                            .collect();
                        files.sort();
                        files.dedup();
                        let timestamp = std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .map(|d| d.as_secs())
                            .unwrap_or(0);
                        eprintln!(
                            "wdf-macros: [{}] [{}] WPP TMF manifest written to {} ({} entries from {} modules)",
                            timestamp,
                            caller,
                            tmf_path.display(),
                            all_entries.len(),
                            files.len(),
                        );
                    }
                    Err(e) => {
                        eprintln!("wdf-macros: failed to write WPP TMF manifest: {e}");
                    }
                }
                let _ = FileExt::unlock(&lock_file);
            }
        }
        Err(e) => {
            eprintln!("wdf-macros: failed to create manifest lock file: {e}");
        }
    }
}

// ---------------------------------------------------------------------------
// atexit registration
// ---------------------------------------------------------------------------

extern "C" fn atexit_flush_manifest() {
    flush_manifest("atexit handler");
}

static FLUSHER_REGISTERED: Once = Once::new();

pub(crate) fn ensure_flusher_registered() {
    FLUSHER_REGISTERED.call_once(|| {
        unsafe extern "C" {
            fn atexit(func: extern "C" fn()) -> core::ffi::c_int;
        }

        // SAFETY: Registering a valid extern "C" function pointer with atexit.
        unsafe {
            atexit(atexit_flush_manifest);
        }
    });
}
