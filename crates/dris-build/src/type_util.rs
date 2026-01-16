use anyhow::{Context, Result};
use quote::ToTokens;
use syn::Type;
use syn::visit_mut::{self, VisitMut};

use crate::model::{ScanCtx, TypeRef};

pub(crate) fn rewrite_type_crate_prefix(ty: &Type, crate_ident: &str) -> Result<Type> {
    if crate_ident == "crate" {
        return Ok(ty.clone());
    }

    struct Rewriter<'a> {
        crate_ident: &'a str,
    }

    impl VisitMut for Rewriter<'_> {
        fn visit_path_mut(&mut self, path: &mut syn::Path) {
            if let Some(first) = path.segments.first_mut() {
                if first.ident == "crate" {
                    let span = first.ident.span();
                    first.ident = syn::Ident::new(self.crate_ident, span);
                }
            }
            visit_mut::visit_path_mut(self, path);
        }
    }

    let mut cloned = ty.clone();
    Rewriter { crate_ident }.visit_type_mut(&mut cloned);
    Ok(cloned)
}

pub(crate) fn type_key(ty: &Type) -> Result<String> {
    Ok(ty.to_token_stream().to_string())
}

pub(crate) fn type_ref_from_type(
    ty: &Type,
    ctx: &ScanCtx,
    qualify_single: bool,
) -> Result<TypeRef> {
    let mut ty = rewrite_type_crate_prefix(ty, &ctx.crate_ident)?;

    if qualify_single {
        if let Type::Path(tp) = &mut ty {
            if tp.qself.is_none()
                && tp.path.leading_colon.is_none()
                && tp.path.segments.len() == 1
                && tp.path.segments[0].ident != "Self"
            {
                let original = tp.path.segments[0].clone();
                let mut segments = Vec::<syn::PathSegment>::new();
                segments.push(syn::PathSegment {
                    ident: syn::Ident::new(&ctx.crate_ident, original.ident.span()),
                    arguments: syn::PathArguments::None,
                });
                for m in &ctx.module_path {
                    segments.push(syn::PathSegment {
                        ident: syn::Ident::new(m, original.ident.span()),
                        arguments: syn::PathArguments::None,
                    });
                }
                segments.push(original);
                tp.path.segments = syn::punctuated::Punctuated::from_iter(segments);
            }
        }
    }

    let key = type_key(&ty)?;
    let simple_name = match &ty {
        Type::Path(tp) => tp.path.segments.last().map(|s| s.ident.to_string()),
        _ => None,
    };

    Ok(TypeRef {
        crate_ident: ctx.crate_ident.clone(),
        key,
        simple_name,
    })
}

pub(crate) fn type_key_from_qualified_ident(
    crate_ident: &str,
    module_path: &[String],
    ident: &str,
) -> Result<String> {
    let mut s = String::new();
    s.push_str(crate_ident);
    for m in module_path {
        s.push_str("::");
        s.push_str(m);
    }
    s.push_str("::");
    s.push_str(ident);
    let ty: Type = syn::parse_str(&s)?;
    type_key(&ty)
}

pub(crate) fn trait_key_from_path(path: &syn::Path, ctx: &ScanCtx) -> Result<String> {
    let mut path = path.clone();
    if path.leading_colon.is_none() && path.segments.len() == 1 {
        let original = path.segments[0].clone();
        let mut segments = Vec::<syn::PathSegment>::new();
        segments.push(syn::PathSegment {
            ident: syn::Ident::new(&ctx.crate_ident, original.ident.span()),
            arguments: syn::PathArguments::None,
        });
        for m in &ctx.module_path {
            segments.push(syn::PathSegment {
                ident: syn::Ident::new(m, original.ident.span()),
                arguments: syn::PathArguments::None,
            });
        }
        segments.push(original);
        path.segments = syn::punctuated::Punctuated::from_iter(segments);
    }
    let ty = Type::Path(syn::TypePath { qself: None, path });
    type_key(&rewrite_type_crate_prefix(&ty, &ctx.crate_ident)?)
}

pub(crate) fn snake_ident_from_type(type_key: &str) -> Result<String> {
    let ty: Type = syn::parse_str(type_key)
        .with_context(|| format!("无法解析类型用于生成字段名: {type_key}"))?;
    let Type::Path(type_path) = ty else {
        return Ok(String::new());
    };
    let last = type_path.path.segments.last().map(|s| s.ident.to_string());
    let Some(last) = last else {
        return Ok(String::new());
    };
    Ok(to_snake(&last))
}

pub(crate) fn to_snake(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 4);
    let mut prev_is_lower_or_digit = false;
    for ch in s.chars() {
        if ch.is_ascii_uppercase() {
            if prev_is_lower_or_digit {
                out.push('_');
            }
            out.push(ch.to_ascii_lowercase());
            prev_is_lower_or_digit = false;
        } else {
            out.push(ch);
            prev_is_lower_or_digit = ch.is_ascii_lowercase() || ch.is_ascii_digit();
        }
    }
    out
}

pub(crate) fn stable_hash_u64(s: &str) -> u64 {
    let mut h: u64 = 14695981039346656037;
    for b in s.as_bytes() {
        h ^= *b as u64;
        h = h.wrapping_mul(1099511628211);
    }
    h
}
