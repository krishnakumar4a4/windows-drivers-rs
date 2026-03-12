// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! WPP manifest (TMF) file generation for `trace!` macro metadata.
//!
//! ## Per-module partial manifest design
//!
//! Each `trace!` expansion records its entry keyed by source file name into
//! `TRACE_ENTRIES` (`HashMap<String, Vec<TraceManifestEntry>>`).  On flush:
//!
//! 1. Each source file that had `trace!` expansions writes a **partial**
//!    manifest file (JSON) to the scratch directory.
//! 2. All partial files (including unchanged ones from prior builds) are
//!    assembled into the final `.tmf` file.
//! 3. The `atexit` handler reads the rustc dep-info (`.d`) file — whose
//!    path was written by `build.rs` — to obtain the **authoritative**,
//!    cfg-aware list of source modules.  Stale partials for deleted or
//!    removed modules are pruned before assembly.
//!
//! This survives incremental compilation: only re-expanded modules update
//! their partials; unchanged modules retain their on-disk partials.
//!
//! ## Flush strategy
//!
//! The work is split into two phases:
//!
//! 1. **Partial-write phase** — Each `trace!` expansion accumulates entries
//!    in `TRACE_ENTRIES`.  Writing partial JSON files happens eagerly during
//!    `ensure_flusher_registered` or is deferred to the atexit handler.
//! 2. **`atexit` handler** — fires when the `rustc` process exits (after
//!    the `.d` file has been written).  It writes any remaining partials,
//!    reads the `.d` file, prunes stale partials, and assembles the final
//!    `.tmf`.

use std::collections::HashMap;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    LazyLock, Mutex, Once,
};

use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

/// A single argument in a trace manifest entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TraceManifestArg {
    pub name: String,
    pub item_type: String,
    pub param_number: usize,
}

/// Metadata for a single `trace!` invocation.
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TraceControlInfo {
    pub guid: String,
    pub flags: Vec<String>,
}

/// Per-module partial manifest stored as JSON on disk.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct PartialManifest {
    source_file: String,
    controls: Vec<TraceControlInfo>,
    entries: Vec<TraceManifestEntry>,
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
// Partial file helpers
// ---------------------------------------------------------------------------

/// Converts a source file name to a safe filename for the partial manifest.
fn source_file_to_partial_name(source_file: &str) -> String {
    source_file
        .replace(['/', '\\', '.'], "_")
        .to_lowercase()
}

/// Returns the scratch subdirectory for per-module partial manifests.
fn partials_dir() -> std::path::PathBuf {
    scratch::path("wdf_macros_wpp").join("partials")
}

/// Writes a per-module partial manifest file to disk.
fn write_partial(source_file: &str, entries: &[TraceManifestEntry], controls: &[TraceControlInfo]) {
    let dir = partials_dir();
    if std::fs::create_dir_all(&dir).is_err() {
        eprintln!("wdf-macros: failed to create partials directory");
        return;
    }

    let partial = PartialManifest {
        source_file: source_file.to_string(),
        controls: controls.to_vec(),
        entries: entries.to_vec(),
    };

    let file_name = format!("{}.json", source_file_to_partial_name(source_file));
    let path = dir.join(&file_name);

    match serde_json::to_string_pretty(&partial) {
        Ok(json) => {
            eprintln!("wdf-macros: partial manifest JSON for '{source_file}':\n{json}");
            if let Err(e) = std::fs::write(&path, &json) {
            eprintln!("wdf-macros: failed to write partial manifest {}: {e}", path.display());
            } else {
            eprintln!("wdf-macros: partial manifest written for module '{source_file}' -> {}", path.display());
            }
        }
        Err(e) => {
            eprintln!("wdf-macros: failed to serialize partial manifest: {e}");
        }
    }
}

/// Reads the dep-info path hint written by `build.rs` and returns the
/// absolute path to the rustc `.d` file.
fn read_dep_info_path_hint() -> Option<std::path::PathBuf> {
    let hint_path = scratch::path("wdf_macros_wpp").join("dep_info_path.txt");
    let content = std::fs::read_to_string(&hint_path).ok()?;
    let path = std::path::PathBuf::from(content.trim());
    if path.exists() {
        Some(path)
    } else {
        eprintln!(
            "wdf-macros: dep-info path hint points to non-existent file: {}",
            path.display()
        );
        None
    }
}

/// Parses a rustc dep-info (`.d`) file and extracts the list of `.rs` source
/// files as relative paths (e.g. `src/lib.rs`, `src/io/mod.rs`).
///
/// The `.d` file uses Makefile-style syntax:
///
/// ```text
/// /abs/path/to/target/deps/crate.d: src\lib.rs src\io\mod.rs ...
///
/// /abs/path/to/target/deps/crate.dll: src\lib.rs src\io\mod.rs ...
///
/// src\lib.rs:
/// src\io\mod.rs:
/// ```
///
/// We parse the first non-empty line (the dep rule for the `.d` file itself)
/// and extract everything after the first `:` as space-separated file paths.
/// Paths are normalized to forward slashes for cross-platform consistency.
fn parse_dep_info(d_file_path: &std::path::Path) -> Option<Vec<String>> {
    let content = match std::fs::read_to_string(d_file_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!(
                "wdf-macros: failed to read dep-info file {}: {e}",
                d_file_path.display()
            );
            return None;
        }
    };

    // Find the first non-empty line that contains ": " (the dep rule)
    let first_rule = content
        .lines()
        .find(|line| !line.trim().is_empty() && line.contains(": "))?;

    // Everything after the first ": " is space-separated source paths
    let deps_part = first_rule.splitn(2, ": ").nth(1)?;

    let modules: Vec<String> = deps_part
        .split_whitespace()
        .filter(|p| p.ends_with(".rs"))
        .map(|p| p.replace('\\', "/"))
        .collect();

    if modules.is_empty() {
        eprintln!("wdf-macros: dep-info file contained no .rs source files");
        None
    } else {
        eprintln!(
            "wdf-macros: dep-info parsed {} source modules from {}",
            modules.len(),
            d_file_path.display()
        );
        Some(modules)
    }
}

/// Reads the authoritative list of source modules from the rustc dep-info
/// file (via the path hint written by `build.rs`).
fn read_module_list_from_dep_info() -> Option<Vec<String>> {
    let d_file_path = read_dep_info_path_hint()?;
    parse_dep_info(&d_file_path)
}

/// Reads all partial manifest files from the partials directory.
fn read_all_partials() -> Vec<PartialManifest> {
    let dir = partials_dir();
    let mut partials = Vec::new();

    let entries = match std::fs::read_dir(&dir) {
        Ok(e) => e,
        Err(_) => return partials,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "json") {
            if let Ok(content) = std::fs::read_to_string(&path) {
                if let Ok(partial) = serde_json::from_str::<PartialManifest>(&content) {
                    partials.push(partial);
                }
            }
        }
    }

    partials
}

/// Prunes stale partial manifests for modules that no longer exist in the
/// authoritative module list (from the rustc dep-info file).
fn prune_stale_partials(active_modules: &[String]) {
    let dir = partials_dir();
    let entries = match std::fs::read_dir(&dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "json") {
            if let Ok(content) = std::fs::read_to_string(&path) {
                if let Ok(partial) = serde_json::from_str::<PartialManifest>(&content) {
                    if !active_modules.contains(&partial.source_file) {
                        eprintln!(
                            "wdf-macros: pruning stale partial for '{}' (not in dep-info module list)",
                            partial.source_file
                        );
                        let _ = std::fs::remove_file(&path);
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Flush logic
// ---------------------------------------------------------------------------

/// Guards against writing partials more than once.
static PARTIALS_WRITTEN: AtomicBool = AtomicBool::new(false);

/// Phase 1: Write per-module partial JSON files for all modules that had
/// `trace!` expansions in this compilation pass.
///
/// This can be called from the Drop flusher (early, before `.d` exists) or
/// from the atexit handler.  The `AtomicBool` guard ensures it runs at most
/// once.
fn write_partials_phase() {
    if PARTIALS_WRITTEN.swap(true, Ordering::SeqCst) {
        return;
    }

    let entries = TRACE_ENTRIES.lock().unwrap();
    let controls = TRACE_CONTROL_INFO.lock().unwrap();

    if entries.is_empty() {
        eprintln!("wdf-macros: no trace entries accumulated in this compilation pass (partials phase)");
        return;
    }

    for (source_file, file_entries) in entries.iter() {
        write_partial(source_file, file_entries, &controls);
    }

    eprintln!(
        "wdf-macros: partials phase complete — wrote {} module partial(s)",
        entries.len()
    );
}

/// Phase 2 (atexit only): Read the `.d` file, prune stale partials, and
/// assemble the final `.tmf`.
///
/// This MUST run from the `atexit` handler because the `.d` file is only
/// guaranteed to exist after `rustc` finishes compilation.
fn flush_and_assemble_manifest(caller: &str) {
    if MANIFEST_FLUSHED.swap(true, Ordering::SeqCst) {
        return;
    }

    eprintln!("wdf-macros: manifest assembly invoked by {caller}");

    // Ensure partials are written first (no-op if already done)
    write_partials_phase();

    let (controls, driver_name) = {
        let entries = TRACE_ENTRIES.lock().unwrap();
        let controls = TRACE_CONTROL_INFO.lock().unwrap();

        let dn = entries
            .values()
            .flat_map(|v| v.first())
            .map(|e| e.driver_name.clone())
            .next()
            .unwrap_or_else(|| {
                std::env::var("CARGO_PKG_NAME")
                    .unwrap_or_else(|_| "unknown_driver".to_string())
                    .replace('-', "_")
            });

        (controls.clone(), dn)
    };

    assemble_final_tmf(&driver_name, &controls, caller);
}

fn assemble_final_tmf(driver_name: &str, controls: &[TraceControlInfo], caller: &str) {
    // Prune stale partials using the authoritative module list from rustc's
    // dep-info file.  This is cfg-aware and handles include!(), macro-generated
    // mod declarations, #[path] with expressions, etc.
    if let Some(active_modules) = read_module_list_from_dep_info() {
        eprintln!(
            "wdf-macros: dep-info lists {} active source modules, pruning stale partials",
            active_modules.len()
        );
        prune_stale_partials(&active_modules);
    } else {
        eprintln!("wdf-macros: no dep-info available, skipping stale module pruning");
    }

    // Read all surviving partial manifests
    let partials = read_all_partials();

    if partials.is_empty() {
        eprintln!("wdf-macros: no partial manifests found, skipping TMF generation");
        return;
    }

    // Collect all entries from all partials, sorted by message_id for stable output
    let mut all_entries: Vec<TraceManifestEntry> = partials
        .into_iter()
        .flat_map(|p| p.entries)
        .collect();
    all_entries.sort_by_key(|e| e.message_id);

    let tmf_content = generate_tmf_content(&all_entries, controls);

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
                        eprintln!(
                            "wdf-macros: [{}] WPP TMF manifest written to {} ({} entries from {} modules)",
                            caller,
                            tmf_path.display(),
                            all_entries.len(),
                            {
                                let mut files: Vec<&str> = all_entries.iter().map(|e| e.source_file.as_str()).collect();
                                files.sort();
                                files.dedup();
                                files.len()
                            }
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

// TODO: Re-evaluate whether the Drop flusher is still needed now that we
// depend on the `.d` file.  The Drop flusher fires when the macro-expansion
// thread is torn down — which is *before* `rustc` writes the `.d` file.
// If we re-enable it, it should ONLY call `write_partials_phase()` (phase 1)
// and never attempt to read the `.d` file or assemble the TMF.
//
// pub(crate) struct ManifestFlusher;
//
// impl Drop for ManifestFlusher {
//     fn drop(&mut self) {
//         // Phase 1 only — write partials, do NOT assemble TMF here.
//         write_partials_phase();
//     }
// }

extern "C" fn atexit_flush_manifest() {
    flush_and_assemble_manifest("atexit handler");
}

static FLUSHER_REGISTERED: Once = Once::new();

// TODO: Re-evaluate thread-local Drop flusher (see comment above).
// thread_local! {
//     static THREAD_FLUSHER: std::cell::RefCell<Option<ManifestFlusher>> =
//         const { std::cell::RefCell::new(None) };
// }

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

    // TODO: Re-evaluate thread-local Drop flusher.
    // THREAD_FLUSHER.with(|f| {
    //     let mut guard = f.borrow_mut();
    //     if guard.is_none() {
    //         *guard = Some(ManifestFlusher);
    //     }
    // });
}
