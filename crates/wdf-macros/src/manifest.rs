// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP manifest (TMF) file generation for `trace!` macro metadata.
//!
//! This module accumulates trace entry metadata from `trace!` macro expansions
//! and writes a TMF (Trace Message Format) file when the proc-macro host
//! process exits.  The manifest file enables WPP trace decoding tools
//! (`tracefmt`, `traceview`) to decode ETW events produced by the driver's
//! runtime tracing calls (`wpp_trace_message` / `WppAutoLogTrace`).
//!
//! ## Flush strategy
//!
//! Trace metadata is accumulated in-process via [`TRACE_ENTRIES`] (a
//! `Mutex<Vec<…>>`).  Two complementary mechanisms guarantee the manifest is
//! written exactly once after **all** `trace!` expansions have completed:
//!
//! 1. **`atexit` handler** (primary) – registered via the C standard library's
//!    `atexit` function.  This fires when the `rustc` process exits (including
//!    via `std::process::exit`).
//! 2. **Thread-local `ManifestFlusher` with `Drop`** (secondary) – fires when
//!    the compilation thread exits normally (i.e. when `main()` returns without
//!    calling `process::exit`).
//!
//! An [`AtomicBool`] guard prevents double-flushing.

use std::sync::{
    atomic::{AtomicBool, Ordering},
    Mutex, Once,
};

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

/// A single argument in a trace manifest entry.
#[derive(Debug, Clone)]
pub(crate) struct TraceManifestArg {
    /// The argument name (e.g. `"my_var"`, `"literal0"`).
    pub name: String,
    /// The WPP item type (e.g. `"ItemLong"`, `"ItemString"`).
    pub item_type: String,
    /// The 1-based parameter number for TMF format strings.
    pub param_number: usize,
}

/// Metadata for a single `trace!` invocation.
#[derive(Debug, Clone)]
pub(crate) struct TraceManifestEntry {
    /// The WPP message ID (matches the runtime `id` parameter passed to
    /// `wpp_trace_message`).
    pub message_id: u32,
    /// Source file name (e.g. `"lib.rs"`).
    pub source_file: String,
    /// Source line number.
    pub line_number: usize,
    /// Optional WPP flag name (e.g. `"FLAG_ONE"`).
    pub flag: Option<String>,
    /// Optional trace level name (e.g. `"Information"`).
    pub level: Option<String>,
    /// The original Rust format string (e.g. `"Hello {} world {}"`).
    pub format_string: String,
    /// The trace arguments.
    pub args: Vec<TraceManifestArg>,
    /// The driver/crate name (dashes replaced by underscores).
    pub driver_name: String,
}

/// Trace control information (GUID + flags) captured from `#[driver_entry]`.
#[derive(Debug, Clone)]
pub(crate) struct TraceControlInfo {
    /// The control GUID string.
    pub guid: String,
    /// The WPP flag names for this control.
    pub flags: Vec<String>,
}

// ---------------------------------------------------------------------------
// Global accumulation statics
// ---------------------------------------------------------------------------

/// Accumulated trace entries from all `trace!` invocations in this
/// compilation session.
pub(crate) static TRACE_ENTRIES: Mutex<Vec<TraceManifestEntry>> = Mutex::new(Vec::new());

/// Trace control definitions captured from `#[driver_entry]`.
pub(crate) static TRACE_CONTROL_INFO: Mutex<Vec<TraceControlInfo>> = Mutex::new(Vec::new());

/// Prevents double-flushing from both `atexit` and `Drop`.
static MANIFEST_FLUSHED: AtomicBool = AtomicBool::new(false);

// ---------------------------------------------------------------------------
// TMF generation helpers
// ---------------------------------------------------------------------------

/// Maps a WPP item type name to its C-style format specifier.
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

/// Generates a WPP format string with **1-based** argument numbering suitable
/// for a TMF file.
///
/// Converts Rust `{}` placeholders to WPP `%N!type!` format specifiers where
/// `N` starts at 1.
fn generate_tmf_format_string(format_str: &str, args: &[TraceManifestArg]) -> String {
    let mut result = String::new();
    let mut arg_idx = 0;
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                // Escaped `{{` → literal `{`
                chars.next();
                result.push('{');
            } else {
                // Format placeholder `{}`  – skip until closing `}`
                while let Some(inner) = chars.next() {
                    if inner == '}' {
                        break;
                    }
                }
                if arg_idx < args.len() {
                    let param_num = arg_idx + 1; // 1-based for TMF
                    let fmt_spec = item_type_to_format_specifier(&args[arg_idx].item_type);
                    result.push_str(&format!("%{param_num}!{fmt_spec}!"));
                    arg_idx += 1;
                }
            }
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
                // Escaped `}}` → literal `}`
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

/// Generates the complete TMF file content from the provided entries and
/// controls.
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
        // Line 1: <GUID> <driver_name> // SRC=<file> MJ= MN=
        tmf.push_str(&format!(
            "{} {} // SRC={} MJ= MN=\n",
            control_guid, entry.driver_name, entry.source_file
        ));

        // Generate 1-based WPP format string
        let wpp_fmt = generate_tmf_format_string(&entry.format_string, &entry.args);

        // Build level/flag comment
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

        // Line 2: #typev <driver>_<line> <msg_id> "%0<fmt>" // <level> <flag>
        tmf.push_str(&format!(
            "#typev {}_{} {} \"%0{}\" // {}\n",
            entry.driver_name, entry.line_number, entry.message_id, wpp_fmt, comment
        ));

        // Argument block
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

        // Blank line between entries
        tmf.push('\n');
    }

    tmf
}

// ---------------------------------------------------------------------------
// Flush logic
// ---------------------------------------------------------------------------

/// Writes the accumulated manifest to the scratch directory as a TMF file.
///
/// Guarded by [`MANIFEST_FLUSHED`] so it executes at most once.
fn flush_accumulated_manifest() {
    // Prevent double-flushing
    if MANIFEST_FLUSHED.swap(true, Ordering::SeqCst) {
        return;
    }

    // Build the TMF content while holding the locks, then release before I/O.
    let (tmf_content, driver_name) = {
        let entries = TRACE_ENTRIES.lock().unwrap();
        let controls = TRACE_CONTROL_INFO.lock().unwrap();

        if entries.is_empty() {
            return;
        }

        let driver_name = entries
            .first()
            .map(|e| e.driver_name.clone())
            .unwrap_or_else(|| "unknown_driver".to_string());

        let content = generate_tmf_content(&entries, &controls);
        (content, driver_name)
    };

    if tmf_content.is_empty() {
        return;
    }

    let scratch_dir = scratch::path("wdf_macros_wpp");
    let tmf_path = scratch_dir.join(format!("{driver_name}.tmf"));

    // Use file locking for safety (following the wdk-macros pattern).
    let lock_path = scratch_dir.join(".wpp_manifest.lock");
    match std::fs::File::create(&lock_path) {
        Ok(lock_file) => {
            use fs4::fs_std::FileExt;
            if FileExt::lock_exclusive(&lock_file).is_ok() {
                match std::fs::write(&tmf_path, &tmf_content) {
                    Ok(()) => {
                        eprintln!(
                            "wdf-macros: WPP TMF manifest written to {}",
                            tmf_path.display()
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
// Drop-based flusher + atexit registration
// ---------------------------------------------------------------------------

/// Drop-based manifest flusher.
///
/// When dropped (e.g. on thread exit), writes the accumulated trace metadata
/// to a TMF manifest file.  An `atexit` handler is also registered as a
/// fallback to handle cases where thread-local destructors don't run (e.g.
/// when `rustc` calls `std::process::exit`).
pub(crate) struct ManifestFlusher;

impl Drop for ManifestFlusher {
    fn drop(&mut self) {
        flush_accumulated_manifest();
    }
}

/// `atexit` callback registered once per process.
extern "C" fn atexit_flush_manifest() {
    flush_accumulated_manifest();
}

/// Guards one-time registration of the `atexit` handler.
static FLUSHER_REGISTERED: Once = Once::new();

thread_local! {
    /// Thread-local flusher whose `Drop` fires on thread exit.
    static THREAD_FLUSHER: std::cell::RefCell<Option<ManifestFlusher>> =
        const { std::cell::RefCell::new(None) };
}

/// Ensures the manifest flusher is registered.
///
/// This is called from each `trace!` expansion.  It registers:
///
/// 1. An `atexit` handler (primary – runs when `rustc` process exits).
/// 2. A thread-local [`ManifestFlusher`] (secondary – runs when thread exits
///    via `Drop`).
///
/// Both are guarded by [`MANIFEST_FLUSHED`] to prevent double-flushing.
pub(crate) fn ensure_flusher_registered() {
    // Register atexit handler (once)
    FLUSHER_REGISTERED.call_once(|| {
        // SAFETY: `atexit` is a C standard library function that registers a
        // callback to be called on normal process exit.  The callback
        // `atexit_flush_manifest` has a compatible `extern "C"` ABI and
        // remains valid for the lifetime of the process since it is a
        // statically-linked function in this proc-macro dylib.
        unsafe extern "C" {
            fn atexit(func: extern "C" fn()) -> core::ffi::c_int;
        }

        // SAFETY: Calling `atexit` with a valid function pointer that has a
        // compatible `extern "C"` ABI.
        unsafe {
            atexit(atexit_flush_manifest);
        }
    });

    // Register thread-local flusher as defense-in-depth
    THREAD_FLUSHER.with(|f| {
        let mut guard = f.borrow_mut();
        if guard.is_none() {
            *guard = Some(ManifestFlusher);
        }
    });
}
