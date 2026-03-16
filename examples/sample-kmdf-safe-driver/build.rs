// // Copyright (c) Microsoft Corporation
// // License: MIT OR Apache-2.0

// //! Build script for the `sample-kmdf-safe-driver` crate.
// //!
// //! Based on the [`wdk_build::Config`] parsed from the build tree, this build
// //! script will provide `Cargo` with the necessary information to build the
// //! driver binary (ex. linker flags)

// use std::{env, fs, path::PathBuf};

// use object::{
//     write::{Object, Relocation, Symbol, SymbolSection},
//     Architecture, BinaryFormat, Endianness, RelocationFlags, SectionFlags, SectionKind,
//     SymbolFlags, SymbolKind, SymbolScope,
// };
// use quote::ToTokens;
// use syn::{
//     visit::Visit,
//     Expr, ExprLit, File, ItemFn, Lit, Macro, Meta,
// };

// // CodeView constants
// const CV_SIGNATURE_C13: u32 = 4;
// const DEBUG_S_SYMBOLS: u32 = 0xF1;
// const S_ANNOTATION_TYPE: u16 = 0x1019;

// // COFF relocation types (AMD64)
// const IMAGE_REL_AMD64_SECREL: u16 = 0x000B;
// const IMAGE_REL_AMD64_SECTION: u16 = 0x000A;

// // COFF relocation types (ARM64)
// const IMAGE_REL_ARM64_SECREL: u16 = 0x000F;
// const IMAGE_REL_ARM64_SECTION: u16 = 0x000E;

// // COFF section characteristics for .debug$S
// // IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_DISCARDABLE | IMAGE_SCN_MEM_READ |
// // IMAGE_SCN_ALIGN_1BYTES
// const DEBUG_SECTION_CHARACTERISTICS: u32 = 0x4210_0040;

// /// Collected trace statement information for a single `trace!` invocation.
// struct TraceEntry {
//     file: String,
//     line: usize,
//     flag: Option<String>,
//     level: Option<String>,
//     format_string: String,
//     args: Vec<String>,
// }

// /// AST visitor that walks a parsed Rust source file and collects `trace!`
// /// macro invocations.
// struct TraceFinder {
//     file_path: String,
//     entries: Vec<TraceEntry>,
// }

// impl TraceFinder {
//     fn new(file_path: String) -> Self {
//         Self {
//             file_path,
//             entries: Vec::new(),
//         }
//     }

//     /// Parse the token stream inside a `trace!(...)` invocation into a
//     /// [`TraceEntry`].  The macro supports the following forms:
//     ///   - `trace!("fmt", args...)`
//     ///   - `trace!(FLAG, "fmt", args...)`
//     ///   - `trace!(LEVEL, "fmt", args...)`
//     ///   - `trace!(FLAG, LEVEL, "fmt", args...)`
//     fn parse_trace_args(&self, mac: &Macro) -> Option<TraceEntry> {
//         let tokens = mac.tokens.clone();
//         let line = mac.path.segments.first()?.ident.span().start().line;

//         // Parse as comma-separated expressions.
//         let parsed: syn::punctuated::Punctuated<Expr, syn::Token![,]> =
//             syn::parse::Parser::parse2(
//                 syn::punctuated::Punctuated::<Expr, syn::Token![,]>::parse_terminated,
//                 tokens,
//             )
//             .ok()?;

//         let exprs: Vec<&Expr> = parsed.iter().collect();
//         if exprs.is_empty() {
//             return None;
//         }

//         // Walk from the front: every leading identifier before the first string
//         // literal is a flag or level.  The first string literal is the format
//         // string.  Everything after is an argument.
//         let mut flag: Option<String> = None;
//         let mut level: Option<String> = None;
//         let mut format_string = String::new();
//         let mut args: Vec<String> = Vec::new();
//         let mut found_fmt = false;

//         for expr in &exprs {
//             if found_fmt {
//                 // Everything after the format string is an argument.
//                 args.push(expr.to_token_stream().to_string());
//                 continue;
//             }
//             match expr {
//                 // String literal → this is the format string.
//                 Expr::Lit(ExprLit {
//                     lit: Lit::Str(s), ..
//                 }) => {
//                     format_string = s.value();
//                     found_fmt = true;
//                 }
//                 // Identifier before the format string → flag or level.
//                 Expr::Path(p) => {
//                     let ident = p.to_token_stream().to_string();
//                     if flag.is_none() {
//                         flag = Some(ident);
//                     } else {
//                         level = Some(ident);
//                     }
//                 }
//                 _ => {}
//             }
//         }

//         if format_string.is_empty() {
//             return None;
//         }

//         // Distinguish flag vs level when only one identifier was provided.
//         // Known WPP levels:
//         let known_levels = [
//             "Verbose",
//             "Information",
//             "Warning",
//             "Error",
//             "Fatal",
//             "None",
//         ];
//         if level.is_none() {
//             if let Some(ref f) = flag {
//                 if known_levels.iter().any(|l| l == f) {
//                     level = flag.take();
//                 }
//             }
//         }

//         Some(TraceEntry {
//             file: self.file_path.clone(),
//             line,
//             flag,
//             level,
//             format_string,
//             args,
//         })
//     }
// }

// impl<'ast> Visit<'ast> for TraceFinder {
//     fn visit_macro(&mut self, mac: &'ast Macro) {
//         // Match only macros whose path ends with `trace`.
//         if mac
//             .path
//             .segments
//             .last()
//             .map_or(false, |seg| seg.ident == "trace")
//         {
//             if let Some(entry) = self.parse_trace_args(mac) {
//                 self.entries.push(entry);
//             }
//         }
//         // Continue visiting nested items.
//         syn::visit::visit_macro(self, mac);
//     }
// }

// /// Convert a Rust `{}` format specifier index into a WPP `%N!type!`
// /// placeholder.
// fn rust_fmt_to_wpp(format_string: &str, args: &[String]) -> String {
//     let mut wpp = String::new();
//     let mut arg_idx: usize = 0;
//     let mut chars = format_string.chars().peekable();

//     // TMF format strings use %0 for the trace prefix.
//     wpp.push_str("%0");

//     while let Some(ch) = chars.next() {
//         if ch == '{' && chars.peek() == Some(&'}') {
//             chars.next(); // consume '}'
//             // WPP argument numbering is 1-based; slot 0 is the prefix.
//             let slot = arg_idx + 1;
//             // Attempt to infer the WPP type suffix from the argument token.
//             let type_suffix = args
//                 .get(arg_idx)
//                 .map(|a| infer_wpp_type(a))
//                 .unwrap_or("d".to_string());
//             wpp.push_str(&format!("%{slot}!{type_suffix}!"));
//             arg_idx += 1;
//         } else {
//             wpp.push(ch);
//         }
//     }
//     wpp
// }

// /// Heuristic: derive a WPP format type from the argument expression.
// fn infer_wpp_type(arg: &str) -> String {
//     let arg_trimmed = arg.trim();
//     // Check for explicit type annotations like `var : i32`
//     if let Some((_name, ty)) = arg_trimmed.split_once(':') {
//         let ty = ty.trim();
//         return match ty {
//             "i8" | "i16" | "i32" | "isize" => "d".to_string(),
//             "u8" | "u16" | "u32" | "usize" => "u".to_string(),
//             "i64" => "I64d".to_string(),
//             "u64" => "I64u".to_string(),
//             "f32" | "f64" => "f".to_string(),
//             "&str" | "String" => "s".to_string(),
//             _ => "d".to_string(),
//         };
//     }
//     // If the arg is a string literal, use %s
//     if arg_trimmed.starts_with('"') {
//         return "s".to_string();
//     }
//     // Default to signed integer
//     "d".to_string()
// }

// /// Build the annotation strings for a single trace entry.
// /// These strings are embedded in a CodeView S_ANNOTATION record.
// fn build_annotation_strings(
//     entry: &TraceEntry,
//     guid: &str,
//     module: &str,
//     index: usize,
// ) -> Vec<String> {
//     let wpp_fmt = rust_fmt_to_wpp(&entry.format_string, &entry.args);
//     let func_name = format!("{module}_{}", entry.line);
//     let msg_no = index + 1;

//     let mut strings = Vec::new();

//     // First string: TMF marker
//     strings.push("TMF:".to_string());

//     // Second string: GUID, module, source info
//     strings.push(format!(
//         "{guid} {module} // SRC={} MJ= MN=",
//         entry.file
//     ));

//     // Third string: typev declaration with format string
//     strings.push(format!("#typev {func_name} {msg_no} \"{wpp_fmt}\""));

//     // Parameter block (only if there are arguments)
//     if !entry.args.is_empty() {
//         strings.push("{".to_string());
//         for (i, arg) in entry.args.iter().enumerate() {
//             let slot = i + 1;
//             let wpp_ty = infer_wpp_type(arg);
//             let item_type = match wpp_ty.as_str() {
//                 "s" => "ItemString",
//                 "u" => "ItemULong",
//                 "I64d" => "ItemLongLong",
//                 "I64u" => "ItemULongLong",
//                 "f" => "ItemDouble",
//                 _ => "ItemLong",
//             };
//             // Generate a descriptive parameter name
//             let param_name = if wpp_ty == "s" {
//                 format!("str{i}")
//             } else {
//                 format!("literal{i}")
//             };
//             strings.push(format!("{param_name}, {item_type} -- {slot}"));
//         }
//         strings.push("}".to_string());
//     }

//     strings
// }

// /// Generate a COFF object file with a `.debug$S` section containing
// /// CodeView S_ANNOTATION records for each trace entry.
// ///
// /// The linker will merge these records into the PDB so that trace
// /// decoders can read the TMF data directly from the symbol file.
// fn generate_annotation_obj(entries: &[TraceEntry], guid: &str, module: &str) -> Vec<u8> {
//     // Determine target architecture from Cargo
//     let arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_else(|_| "x86_64".to_string());
//     let (obj_arch, secrel_type, section_type) = match arch.as_str() {
//         "aarch64" => (
//             Architecture::Aarch64,
//             IMAGE_REL_ARM64_SECREL,
//             IMAGE_REL_ARM64_SECTION,
//         ),
//         _ => (
//             Architecture::X86_64,
//             IMAGE_REL_AMD64_SECREL,
//             IMAGE_REL_AMD64_SECTION,
//         ),
//     };

//     let mut obj = Object::new(BinaryFormat::Coff, obj_arch, Endianness::Little);

//     // ---- .text$wpp section with a tiny anchor symbol ----
//     let text_section = obj.add_section(vec![], b".text$wpp".to_vec(), SectionKind::Text);
//     obj.append_section_data(text_section, &[0xCC], 1); // int3

//     let anchor_sym = obj.add_symbol(Symbol {
//         name: b"__wpp_annotation_anchor".to_vec(),
//         value: 0,
//         size: 1,
//         kind: SymbolKind::Label,
//         scope: SymbolScope::Compilation,
//         weak: false,
//         section: SymbolSection::Section(text_section),
//         flags: SymbolFlags::None,
//     });

//     // ---- Build .debug$S section content ----
//     let mut debug_data: Vec<u8> = Vec::new();
//     let mut reloc_offsets: Vec<(u64, u16)> = Vec::new();

//     // CV_SIGNATURE_C13
//     debug_data.extend_from_slice(&CV_SIGNATURE_C13.to_le_bytes());

//     // Build all S_ANNOTATION symbol records
//     let mut symbols_data: Vec<u8> = Vec::new();

//     for (index, entry) in entries.iter().enumerate() {
//         let annotation_strings = build_annotation_strings(entry, guid, module, index);

//         // Calculate total NUL-terminated strings byte size
//         let strings_bytes: usize = annotation_strings.iter().map(|s| s.len() + 1).sum();

//         // Record data (after reclen): rectyp(2) + off(4) + seg(2) + csz(2) + strings
//         let data_size = 2 + 4 + 2 + 2 + strings_bytes;
//         let total_unpadded = 2 + data_size; // reclen field (2) + data
//         let padded_total = (total_unpadded + 3) & !3; // align up to 4
//         let padding = padded_total - total_unpadded;
//         let reclen = (data_size + padding) as u16;

//         // Track relocation offsets (relative to start of .debug$S section).
//         // Section header: signature(4) + subsection_type(4) + subsection_length(4) = 12 bytes
//         let record_start_in_section = 12 + symbols_data.len();
//         let off_field_offset = record_start_in_section + 4; // after reclen(2) + rectyp(2)
//         let seg_field_offset = record_start_in_section + 8; // after + off(4)

//         reloc_offsets.push((off_field_offset as u64, secrel_type));
//         reloc_offsets.push((seg_field_offset as u64, section_type));

//         // Write the S_ANNOTATION record
//         symbols_data.extend_from_slice(&reclen.to_le_bytes()); // reclen
//         symbols_data.extend_from_slice(&S_ANNOTATION_TYPE.to_le_bytes()); // rectyp
//         symbols_data.extend_from_slice(&0u32.to_le_bytes()); // off  (filled by linker)
//         symbols_data.extend_from_slice(&0u16.to_le_bytes()); // seg  (filled by linker)
//         symbols_data
//             .extend_from_slice(&(annotation_strings.len() as u16).to_le_bytes()); // csz

//         for s in &annotation_strings {
//             symbols_data.extend_from_slice(s.as_bytes());
//             symbols_data.push(0); // NUL terminator
//         }

//         // Pad to 4-byte alignment
//         for _ in 0..padding {
//             symbols_data.push(0);
//         }
//     }

//     // Write the DEBUG_S_SYMBOLS subsection header + data
//     debug_data.extend_from_slice(&DEBUG_S_SYMBOLS.to_le_bytes());
//     debug_data.extend_from_slice(&(symbols_data.len() as u32).to_le_bytes());
//     debug_data.extend_from_slice(&symbols_data);

//     // ---- Create .debug$S section ----
//     let debug_section = obj.add_section(vec![], b".debug$S".to_vec(), SectionKind::Other);
//     obj.section_mut(debug_section).flags = SectionFlags::Coff {
//         characteristics: DEBUG_SECTION_CHARACTERISTICS,
//     };
//     obj.append_section_data(debug_section, &debug_data, 1);

//     // Add SECREL + SECTION relocations for each S_ANNOTATION record
//     for (offset, typ) in reloc_offsets {
//         obj.add_relocation(
//             debug_section,
//             Relocation {
//                 offset,
//                 symbol: anchor_sym,
//                 addend: 0,
//                 flags: RelocationFlags::Coff { typ },
//             },
//         )
//         .expect("Failed to add relocation to .debug$S section");
//     }

//     obj.write().expect("Failed to write COFF object")
// }

// /// AST visitor that extracts the trace GUID from a
// /// `#[driver_entry(trace_control = ("GUID", ...))]` attribute.
// struct GuidFinder {
//     guid: Option<String>,
// }

// impl GuidFinder {
//     fn new() -> Self {
//         Self { guid: None }
//     }

//     /// Given the token stream of `driver_entry(...)`, look for
//     /// `trace_control = ("<guid>", ...)` and extract the GUID string.
//     fn extract_guid_from_attr_tokens(&mut self, tokens: proc_macro2::TokenStream) {
//         // Parse as: trace_control = ("GUID", [flags...])
//         // We walk the token tree looking for the string literal that follows
//         // `trace_control` `=` `(`.
//         let token_vec: Vec<proc_macro2::TokenTree> = tokens.into_iter().collect();
//         for (i, tok) in token_vec.iter().enumerate() {
//             if let proc_macro2::TokenTree::Ident(ident) = tok {
//                 if ident == "trace_control" {
//                     // Find the group "(...)" that follows `=`.
//                     // Pattern: trace_control = (...)
//                     // token_vec[i] = trace_control
//                     // token_vec[i+1] = =
//                     // token_vec[i+2] = (...)
//                     if let Some(proc_macro2::TokenTree::Group(group)) = token_vec.get(i + 2) {
//                         // Inside the group, the first token should be the GUID string literal.
//                         for inner in group.stream() {
//                             if let proc_macro2::TokenTree::Literal(lit) = inner {
//                                 let raw = lit.to_string();
//                                 // Strip surrounding quotes.
//                                 if raw.starts_with('"') && raw.ends_with('"') {
//                                     self.guid = Some(raw[1..raw.len() - 1].to_string());
//                                     return;
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//     }
// }

// impl<'ast> Visit<'ast> for GuidFinder {
//     fn visit_item_fn(&mut self, node: &'ast ItemFn) {
//         for attr in &node.attrs {
//             // Match #[driver_entry(...)] attributes.
//             if let Meta::List(meta_list) = &attr.meta {
//                 if meta_list
//                     .path
//                     .segments
//                     .last()
//                     .map_or(false, |seg| seg.ident == "driver_entry")
//                 {
//                     self.extract_guid_from_attr_tokens(meta_list.tokens.clone());
//                 }
//             }
//         }
//         // Continue visiting nested items.
//         syn::visit::visit_item_fn(self, node);
//     }
// }

// /// Scan all `.rs` files under `src/`, parse them with `syn`, extract
// /// `trace!` macro invocations, and generate a COFF object containing
// /// CodeView S_ANNOTATION records.  The object is written to `OUT_DIR`
// /// and linked into the final binary so the annotations land in the PDB.
// fn generate_wpp_annotation_coff() -> PathBuf {
//     let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
//     let out_dir = env::var("OUT_DIR").unwrap();
//     let src_dir = PathBuf::from(&manifest_dir).join("src");

//     // Derive module name from the package name.
//     let module = env::var("CARGO_PKG_NAME").unwrap_or_else(|_| "driver".to_string());

//     let mut all_entries: Vec<TraceEntry> = Vec::new();
//     let mut guid_finder = GuidFinder::new();

//     if src_dir.is_dir() {
//         for entry in fs::read_dir(&src_dir).unwrap() {
//             let entry = entry.unwrap();
//             let path = entry.path();
//             if path.extension().map_or(false, |ext| ext == "rs") {
//                 let content = fs::read_to_string(&path).unwrap();
//                 let syntax: File = syn::parse_file(&content).unwrap_or_else(|e| {
//                     panic!("Failed to parse {}: {e}", path.display());
//                 });

//                 // Look for the trace GUID in this file.
//                 guid_finder.visit_file(&syntax);

//                 let file_name = path
//                     .file_name()
//                     .unwrap_or_default()
//                     .to_string_lossy()
//                     .to_string();
//                 let mut finder = TraceFinder::new(file_name);
//                 finder.visit_file(&syntax);
//                 all_entries.extend(finder.entries);
//             }
//         }
//     }

//     let guid = guid_finder
//         .guid
//         .unwrap_or_else(|| "00000000-0000-0000-0000-000000000000".to_string());

//     // Generate the COFF object with S_ANNOTATION records.
//     let obj_bytes = generate_annotation_obj(&all_entries, &guid, &module);
//     let obj_path = PathBuf::from(&out_dir).join("wpp_annotations.obj");
//     fs::write(&obj_path, &obj_bytes).expect("Failed to write wpp_annotations.obj");

//     println!("cargo:rerun-if-changed={}", src_dir.display());
//     println!(
//         "cargo:warning=WPP annotation object generated at {} ({} trace entries)",
//         obj_path.display(),
//         all_entries.len()
//     );

//     obj_path
// }

fn main() -> Result<(), wdk_build::ConfigError> {
    // let obj_path = generate_wpp_annotation_coff();

    // Tell the linker to include the annotation object so that
    // S_ANNOTATION records end up in the final PDB.
    // println!("cargo::rustc-cdylib-link-arg={}", obj_path.display());
    wdk_build::configure_wdk_binary_build()
}
