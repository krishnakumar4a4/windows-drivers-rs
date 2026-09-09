// Copyright (c) Microsoft Corporation
// License: MIT OR Apache-2.0

//! Implementation of the `WppField` derive macro.

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Expr, Fields, Lit, Result, parse2};

pub fn generate(input: TokenStream) -> Result<TokenStream> {
    let input: DeriveInput = parse2(input)?;
    let enum_name = &input.ident;

    if !input.generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            &input.generics,
            "WppField does not support generic enums",
        ));
    }

    let (wire_type, list_type, max_value) = parse_repr(&input)?;
    let Data::Enum(data) = &input.data else {
        return Err(syn::Error::new_spanned(
            enum_name,
            "WppField can only be derived for enums",
        ));
    };
    if data.variants.is_empty() {
        return Err(syn::Error::new_spanned(
            enum_name,
            "WppField cannot be derived for an empty enum",
        ));
    }

    let mut next_value = 0_u64;
    let mut values = Vec::with_capacity(data.variants.len());
    let mut entries = Vec::with_capacity(data.variants.len());

    for variant in &data.variants {
        if !matches!(variant.fields, Fields::Unit) {
            return Err(syn::Error::new_spanned(
                &variant.fields,
                "WppField only supports fieldless enum variants",
            ));
        }

        let value = if let Some((_, expression)) = &variant.discriminant {
            parse_discriminant(expression)?
        } else {
            next_value
        };

        if value > max_value {
            return Err(syn::Error::new_spanned(
                variant,
                format!("enum discriminant {value} exceeds the selected repr"),
            ));
        }
        if values.contains(&value) {
            return Err(syn::Error::new_spanned(
                variant,
                format!("duplicate enum discriminant {value}"),
            ));
        }

        let name = variant.ident.to_string();
        if value == next_value {
            entries.push(name);
        } else {
            entries.push(format!("{name}={value}"));
        }
        values.push(value);
        next_value = value.checked_add(1).unwrap_or(value);
    }

    let type_name = format!("{list_type}({})", entries.join(","));

    Ok(quote! {
        unsafe impl ::wpp::WppValue for #enum_name {
            type Wire = #wire_type;

            const TYPE_NAME: &'static str = #type_name;
            const FORMAT_SPEC: &'static str = "!s!";

            fn into_wire(self) -> Self::Wire {
                self as #wire_type
            }
        }
    })
}

fn parse_repr(input: &DeriveInput) -> Result<(syn::Type, &'static str, u64)> {
    let mut repr = None;

    for attribute in &input.attrs {
        if !attribute.path().is_ident("repr") {
            continue;
        }
        attribute.parse_nested_meta(|meta| {
            let candidate = if meta.path.is_ident("u8") {
                Some((syn::parse_quote!(u8), "ItemListByte", u8::MAX as u64))
            } else if meta.path.is_ident("u16") {
                Some((syn::parse_quote!(u16), "ItemListShort", u16::MAX as u64))
            } else if meta.path.is_ident("u32") {
                Some((syn::parse_quote!(u32), "ItemListLong", u32::MAX as u64))
            } else {
                None
            };

            if candidate.is_some() {
                if repr.is_some() {
                    return Err(meta.error("WppField requires exactly one integer repr"));
                }
                repr = candidate;
            }
            Ok(())
        })?;
    }

    repr.ok_or_else(|| {
        syn::Error::new_spanned(
            &input.ident,
            "WppField requires #[repr(u8)], #[repr(u16)], or #[repr(u32)]",
        )
    })
}

fn parse_discriminant(expression: &Expr) -> Result<u64> {
    let Expr::Lit(expression) = expression else {
        return Err(syn::Error::new_spanned(
            expression,
            "WppField enum discriminants must be non-negative integer literals",
        ));
    };
    let Lit::Int(value) = &expression.lit else {
        return Err(syn::Error::new_spanned(
            expression,
            "WppField enum discriminants must be integer literals",
        ));
    };
    value.base10_parse()
}

#[cfg(test)]
mod tests {
    use super::generate;

    #[test]
    fn derives_contiguous_byte_enum() {
        let output = generate(syn::parse_quote! {
            #[repr(u8)]
            enum State { Unknown, Starting, Ready }
        })
        .unwrap()
        .to_string();

        assert!(output.contains("type Wire = u8"));
        assert!(output.contains("ItemListByte(Unknown,Starting,Ready)"));
    }

    #[test]
    fn derives_sparse_short_enum() {
        let output = generate(syn::parse_quote! {
            #[repr(u16)]
            enum State { Unknown = 0, Warning = 10, Failed = 100 }
        })
        .unwrap()
        .to_string();

        assert!(output.contains("type Wire = u16"));
        assert!(output.contains("ItemListShort(Unknown,Warning=10,Failed=100)"));
    }

    #[test]
    fn derives_long_enum() {
        let output = generate(syn::parse_quote! {
            #[repr(u32)]
            enum State { Offline, Online, Recovering }
        })
        .unwrap()
        .to_string();

        assert!(output.contains("type Wire = u32"));
        assert!(output.contains("ItemListLong(Offline,Online,Recovering)"));
    }

    #[test]
    fn rejects_payload_variant() {
        assert!(generate(syn::parse_quote! {
            #[repr(u8)]
            enum State { Value(u8) }
        })
        .is_err());
    }
}
