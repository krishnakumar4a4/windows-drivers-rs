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
    generate_tmf_format_string_pub(format_str, args)
}

/// Public helper for `pdb_annotation` module to generate WPP format strings.
pub(crate) fn generate_tmf_format_string_pub(format_str: &str, args: &[TraceManifestArg]) -> String {
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

    // -----------------------------------------------------------------
    // PDB CodeView annotation injection.
    //
    // Only the driver crate's rustc produces a PDB (via the linker).
    // If we find a recently-produced PDB, inject S_ANNOTATION records.
    // For rlib crates no PDB exists -> this is a no-op.
    // -----------------------------------------------------------------
    attempt_pdb_annotation(&all_entries, &controls, &driver_name, &scratch_dir);
}

/// Parses a `.tmf` file produced by a dependent crate and returns the trace
/// entries it contains.
///
/// TMF format (per entry):
/// ```text
/// <guid> <driver_name> // SRC=<source_file> MJ= MN=
/// #typev <driver_name>_<line> <msg_id> "%0<wpp_format>" // <level> <flag>
/// {                           <-- optional arg block
///   <name>, <ItemType> -- <n>
/// }
/// ```
fn parse_tmf_file(path: &std::path::Path) -> Vec<TraceManifestEntry> {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };

    let mut entries = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Look for the header line: <guid> <driver_name> // SRC=<file> MJ= MN=
        if !line.contains("// SRC=") || !line.contains("MJ=") {
            i += 1;
            continue;
        }

        // Parse source file from header
        let source_file = line
            .split("// SRC=")
            .nth(1)
            .and_then(|s| s.split(" MJ=").next())
            .unwrap_or("")
            .trim()
            .to_string();

        // Parse driver_name from header (second token)
        let header_driver_name = line
            .split_whitespace()
            .nth(1)
            .unwrap_or("unknown")
            .to_string();

        i += 1;
        if i >= lines.len() {
            break;
        }

        // Next line should be #typev
        let typev_line = lines[i].trim();
        if !typev_line.starts_with("#typev ") {
            continue;
        }

        // Parse: #typev <name>_<line> <msg_id> "%0<format>" // <comment>
        // Extract message_id
        let parts: Vec<&str> = typev_line.splitn(4, ' ').collect();
        if parts.len() < 4 {
            i += 1;
            continue;
        }

        // parts[1] = "driver_name_LINE"
        let name_line = parts[1];
        let line_number: usize = name_line
            .rsplit('_')
            .next()
            .and_then(|s| s.parse().ok())
            .unwrap_or(0);

        let message_id: u32 = parts[2].parse().unwrap_or(0);

        // Everything after msg_id: "%0<format>" // <comment>
        let rest = parts[3];

        // Extract format string between "%0 and the closing "
        let wpp_format = rest
            .strip_prefix("\"%0")
            .and_then(|s| s.split('"').next())
            .unwrap_or("")
            .to_string();

        // Parse the format string back to a Rust-like format string
        // by reversing WPP specifiers like %1!d! back to {}
        let format_string = reverse_wpp_format(&wpp_format);

        // Extract level and flag from comment (after //)
        let comment = rest
            .split("// ")
            .nth(1)
            .unwrap_or("")
            .trim();

        let comment_parts: Vec<&str> = comment.split_whitespace().collect();
        let (level, flag) = parse_level_flag(&comment_parts);

        i += 1;

        // Parse optional arg block
        let mut args = Vec::new();
        if i < lines.len() && lines[i].trim() == "{" {
            i += 1;
            while i < lines.len() && lines[i].trim() != "}" {
                let arg_line = lines[i].trim();
                // Format: "name, ItemType -- param_number"
                if let Some((name_type, param)) = arg_line.split_once(" -- ") {
                    if let Some((name, item_type)) = name_type.split_once(", ") {
                        args.push(TraceManifestArg {
                            name: name.trim().to_string(),
                            item_type: item_type.trim().to_string(),
                            param_number: param.trim().parse().unwrap_or(0),
                        });
                    }
                }
                i += 1;
            }
            if i < lines.len() {
                i += 1; // skip "}"
            }
        }

        entries.push(TraceManifestEntry {
            message_id,
            source_file,
            line_number,
            flag,
            level,
            format_string,
            args,
            driver_name: header_driver_name.clone(),
        });
    }

    entries
}

/// Reverses WPP format specifiers like `%1!d!` back to `{}`.
fn reverse_wpp_format(wpp: &str) -> String {
    let mut result = String::new();
    let mut chars = wpp.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '%' {
            // Skip the param number
            while chars.peek().map_or(false, |ch| ch.is_ascii_digit()) {
                chars.next();
            }
            // Skip !specifier!
            if chars.peek() == Some(&'!') {
                chars.next(); // first '!'
                while chars.peek().map_or(false, |ch| *ch != '!') {
                    chars.next();
                }
                if chars.peek() == Some(&'!') {
                    chars.next(); // closing '!'
                }
            }
            result.push_str("{}");
        } else {
            result.push(c);
        }
    }
    result
}

/// Parses level and flag from a TMF comment like ["Information", "FLAG_DEVICE"]
/// or ["Verbose"] or ["FLAG_DEVICE"].
fn parse_level_flag(parts: &[&str]) -> (Option<String>, Option<String>) {
    let known_levels = [
        "Verbose",
        "Information",
        "Warning",
        "Error",
        "Critical",
    ];
    let mut level = None;
    let mut flag = None;
    for &part in parts {
        if known_levels.iter().any(|l| l.eq_ignore_ascii_case(part)) {
            level = Some(part.to_string());
        } else if !part.is_empty() {
            flag = Some(part.to_string());
        }
    }
    (level, flag)
}

/// Reads all `.tmf` files from the scratch directory *other* than the driver's
/// own TMF, parses them, and returns the merged entries.
fn collect_dependent_entries(
    scratch_dir: &std::path::Path,
    driver_name: &str,
) -> Vec<TraceManifestEntry> {
    let driver_tmf = format!("{driver_name}.tmf");
    let mut all = Vec::new();

    let entries = match std::fs::read_dir(scratch_dir) {
        Ok(e) => e,
        Err(_) => return all,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("tmf") {
            continue;
        }
        // Skip the driver's own TMF — those entries are already in-memory
        if path.file_name().and_then(|n| n.to_str()) == Some(&driver_tmf) {
            continue;
        }
        let parsed = parse_tmf_file(&path);
        if !parsed.is_empty() {
            eprintln!(
                "wdf-macros: [pdb_annotation] Read {} trace(s) from dependent TMF: {}",
                parsed.len(),
                path.file_name().and_then(|n| n.to_str()).unwrap_or("?"),
            );
        }
        all.extend(parsed);
    }

    all
}

/// Attempts to find a PDB and inject CodeView annotations into it.
///
/// When a PDB is found (i.e. this is the driver crate), this function also
/// reads **all** `.tmf` files from the scratch directory (produced by
/// dependent crates) and merges their trace entries into the PDB alongside
/// the driver's own entries.  This ensures the PDB contains a complete set
/// of annotations for every `trace!` call across the entire dependency tree.
///
/// This is a no-op when:
/// - No PDB is found (rlib crates, failed builds)
/// - No trace entries exist
/// - PDB patching fails (falls back silently after logging)
fn attempt_pdb_annotation(
    driver_entries: &[TraceManifestEntry],
    controls: &[TraceControlInfo],
    driver_name: &str,
    scratch_dir: &std::path::Path,
) {
    if driver_entries.is_empty() {
        return;
    }

    let control_guid = controls
        .first()
        .map(|c| c.guid.as_str())
        .unwrap_or("00000000-0000-0000-0000-000000000000");

    let pdb_path = match crate::pdb_annotation::find_recent_pdb(scratch_dir, driver_name) {
        Some(p) => p,
        None => {
            eprintln!(
                "wdf-macros: [pdb_annotation] No recent PDB found — \
                 '{}' is likely a dependent crate (rlib), skipping PDB annotation",
                driver_name
            );
            return;
        }
    };

    // -----------------------------------------------------------------
    // Merge: driver's own entries + entries from dependent crate TMFs.
    // -----------------------------------------------------------------
    let dependent_entries = collect_dependent_entries(scratch_dir, driver_name);

    let mut all_entries: Vec<TraceManifestEntry> = Vec::new();
    all_entries.extend_from_slice(driver_entries);
    all_entries.extend(dependent_entries);
    all_entries.sort_by_key(|e| e.message_id);

    eprintln!(
        "wdf-macros: [pdb_annotation] PDB found: '{}' — \
         injecting {} annotation(s) ({} driver + {} dependent)",
        pdb_path.display(),
        all_entries.len(),
        driver_entries.len(),
        all_entries.len() - driver_entries.len(),
    );

    match crate::pdb_annotation::patch_pdb_with_annotations(
        &pdb_path,
        &all_entries,
        control_guid,
        driver_name,
    ) {
        Ok(()) => {
            eprintln!(
                "wdf-macros: [pdb_annotation] PDB patched successfully with CodeView S_ANNOTATION records"
            );
        }
        Err(e) => {
            eprintln!(
                "wdf-macros: [pdb_annotation] PDB patching failed ({}), TMF sidecar is still available",
                e
            );
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
