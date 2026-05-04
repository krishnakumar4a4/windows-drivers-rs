// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Trace argument parsing for the `trace!` macro.

use proc_macro::TokenStream;
use syn::{Error, Lit, Token};

use crate::format_spec::{CFormatSpec, parse_c_format_string};

/// Represents a parsed trace argument (expression paired with its format spec)
pub(crate) struct TraceArg {
    pub name: String,
    pub spec: CFormatSpec,
    pub expr: syn::Expr,
}

/// Result of parsing trace macro arguments
pub(crate) struct ParsedTraceArgs {
    pub flag: Option<syn::Ident>,
    pub level: Option<syn::Ident>,
    pub format_str: String,
    pub format_specs: Vec<CFormatSpec>,
    pub args: Vec<TraceArg>,
}

/// Known trace level names for validation
const TRACE_LEVEL_NAMES: &[&str] = &[
    "None", "Critical", "Error", "Warning",
    "Information", "Verbose", "Reserved6", "Reserved7", "Reserved8", "Reserved9"
];

/// Parses the trace! macro input with C-style format specifiers.
pub(crate) fn parse_trace_args(item: TokenStream) -> Result<ParsedTraceArgs, Error> {
    use syn::{Expr, ExprLit, punctuated::Punctuated};

    let args = syn::parse::Parser::parse(
        Punctuated::<Expr, Token![,]>::parse_terminated,
        item,
    )?;

    let all_args: Vec<Expr> = args.into_iter().collect();

    if all_args.is_empty() {
        return Err(Error::new(proc_macro2::Span::call_site(), "trace! requires at least a format string"));
    }

    let mut flag: Option<syn::Ident> = None;
    let mut level: Option<syn::Ident> = None;
    let format_str: String;
    let remaining_start_idx: usize;

    let first_is_path = matches!(&all_args[0], Expr::Path(_));
    let first_is_string = matches!(&all_args[0], Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));

    if first_is_string {
        format_str = match &all_args[0] {
            Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
            _ => unreachable!(),
        };
        remaining_start_idx = 1;
    } else if first_is_path && all_args.len() >= 2 {
        let second_is_string = matches!(&all_args[1], Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));
        let second_is_path = matches!(&all_args[1], Expr::Path(_));

        if second_is_string {
            let ident = extract_path_ident(&all_args[0])?;
            let ident_str = ident.to_string();

            if TRACE_LEVEL_NAMES.contains(&ident_str.as_str()) {
                level = Some(ident);
            } else {
                flag = Some(ident);
            }

            format_str = match &all_args[1] {
                Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
                _ => unreachable!(),
            };
            remaining_start_idx = 2;
        } else if second_is_path && all_args.len() >= 3 {
            let third_is_string = matches!(&all_args[2], Expr::Lit(ExprLit { lit: Lit::Str(_), .. }));

            if third_is_string {
                let first_ident = extract_path_ident(&all_args[0])?;
                let second_ident = extract_path_ident(&all_args[1])?;

                let second_str = second_ident.to_string();
                if !TRACE_LEVEL_NAMES.contains(&second_str.as_str()) {
                    return Err(Error::new_spanned(&all_args[1],
                        format!("Expected a trace level (one of: {:?}), got '{}'", TRACE_LEVEL_NAMES, second_str)));
                }

                flag = Some(first_ident);
                level = Some(second_ident);

                format_str = match &all_args[2] {
                    Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => s.value(),
                    _ => unreachable!(),
                };
                remaining_start_idx = 3;
            } else {
                return Err(Error::new_spanned(&all_args[0], "Expected a format string"));
            }
        } else {
            return Err(Error::new_spanned(&all_args[0], "Expected a format string"));
        }
    } else {
        return Err(Error::new_spanned(&all_args[0], "Expected a format string or flag identifier"));
    }

    let format_specs = match parse_c_format_string(&format_str) {
        Ok(specs) => specs,
        Err(e) => return Err(Error::new(proc_macro2::Span::call_site(), e)),
    };

    let arg_count = all_args.len() - remaining_start_idx;
    let spec_count = format_specs.len();
    if arg_count != spec_count {
        return Err(Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "Format string has {} specifier(s) but {} argument(s) were provided",
                spec_count, arg_count
            ),
        ));
    }

    let mut trace_args = Vec::new();
    for (idx, expr) in all_args.into_iter().skip(remaining_start_idx).enumerate() {
        let name = extract_arg_name(&expr, idx);
        trace_args.push(TraceArg {
            name,
            spec: format_specs[idx].clone(),
            expr,
        });
    }

    Ok(ParsedTraceArgs {
        flag,
        level,
        format_str,
        format_specs,
        args: trace_args,
    })
}

/// Extracts a human-readable name from an expression for TMF annotations
fn extract_arg_name(expr: &syn::Expr, idx: usize) -> String {
    use syn::Expr;
    match expr {
        Expr::Path(p) => p.path.segments.last()
            .map(|s| s.ident.to_string())
            .unwrap_or_else(|| format!("arg{}", idx)),
        Expr::Lit(lit) => match &lit.lit {
            Lit::Int(_) => format!("literal{}", idx),
            Lit::Str(_) => format!("str{}", idx),
            _ => format!("arg{}", idx),
        },
        Expr::Reference(r) => {
            if let Expr::Path(p) = r.expr.as_ref() {
                p.path.segments.last()
                    .map(|s| s.ident.to_string())
                    .unwrap_or_else(|| format!("arg{}", idx))
            } else {
                format!("arg{}", idx)
            }
        }
        Expr::Field(f) => match &f.member {
            syn::Member::Named(ident) => ident.to_string(),
            syn::Member::Unnamed(index) => format!("field{}", index.index),
        },
        Expr::MethodCall(m) => m.method.to_string(),
        _ => format!("arg{}", idx),
    }
}

/// Extracts an identifier from a path expression
fn extract_path_ident(expr: &syn::Expr) -> Result<syn::Ident, Error> {
    if let syn::Expr::Path(path) = expr {
        if let Some(seg) = path.path.segments.last() {
            return Ok(seg.ident.clone());
        }
    }
    Err(Error::new_spanned(expr, "Expected an identifier"))
}
