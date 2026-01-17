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
    let last = type_path
        .path
        .segments
        .last()
        .expect("TypePath 不应为空")
        .ident
        .to_string();
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

#[cfg(test)]
mod tests {
    use super::*;
    use syn::Type;

    fn ty(s: &str) -> Type {
        syn::parse_str::<Type>(s).unwrap()
    }

    #[test]
    fn rewrite_type_crate_prefix_覆盖不改与改写() {
        let original = ty("crate::a::Foo");
        let rewritten = rewrite_type_crate_prefix(&original, "crate").unwrap();
        assert_eq!(type_key(&rewritten).unwrap(), type_key(&original).unwrap());

        let rewritten = rewrite_type_crate_prefix(&original, "dep").unwrap();
        assert_eq!(
            type_key(&rewritten).unwrap(),
            type_key(&ty("dep::a::Foo")).unwrap()
        );

        let original = ty("Option<crate::Foo>");
        let rewritten = rewrite_type_crate_prefix(&original, "dep").unwrap();
        assert_eq!(
            type_key(&rewritten).unwrap(),
            type_key(&ty("Option<dep::Foo>")).unwrap()
        );
    }

    #[test]
    fn type_ref_from_type_覆盖单段限定与例外() {
        let ctx = ScanCtx {
            crate_ident: "my_crate".to_string(),
            module_path: vec!["a".to_string(), "b".to_string()],
        };

        let r = type_ref_from_type(&ty("Foo"), &ctx, false).unwrap();
        assert_eq!(r.key, type_key(&ty("Foo")).unwrap());
        assert_eq!(r.simple_name.as_deref(), Some("Foo"));

        let r = type_ref_from_type(&ty("Foo"), &ctx, true).unwrap();
        assert_eq!(r.key, type_key(&ty("my_crate::a::b::Foo")).unwrap());
        assert_eq!(r.simple_name.as_deref(), Some("Foo"));

        let r = type_ref_from_type(&ty("Self"), &ctx, true).unwrap();
        assert_eq!(r.key, type_key(&ty("Self")).unwrap());
        assert_eq!(r.simple_name.as_deref(), Some("Self"));

        let r = type_ref_from_type(&ty("other::Foo"), &ctx, true).unwrap();
        assert_eq!(r.key, type_key(&ty("other::Foo")).unwrap());

        let r = type_ref_from_type(&ty("::Foo"), &ctx, true).unwrap();
        assert_eq!(r.key, type_key(&ty("::Foo")).unwrap());

        let r = type_ref_from_type(&ty("&Foo"), &ctx, true).unwrap();
        assert!(r.simple_name.is_none());

        // 覆盖 qself（<T as Trait>::Assoc）分支：不应触发 qualify_single。
        let r = type_ref_from_type(&ty("<Foo as Bar>::Baz"), &ctx, true).unwrap();
        assert_eq!(r.key, type_key(&ty("<Foo as Bar>::Baz")).unwrap());
        assert!(r.simple_name.is_some());
    }

    #[test]
    fn type_key_from_qualified_ident_与_trait_key_from_path() {
        let out = type_key_from_qualified_ident(
            "my_crate",
            &[String::from("m1"), String::from("m2")],
            "Foo",
        )
        .unwrap();
        assert_eq!(out, type_key(&ty("my_crate::m1::m2::Foo")).unwrap());

        let ctx = ScanCtx {
            crate_ident: "my_crate".to_string(),
            module_path: vec!["m".to_string()],
        };

        let path: syn::Path = syn::parse_str("Trait").unwrap();
        let k = trait_key_from_path(&path, &ctx).unwrap();
        assert_eq!(k, type_key(&ty("my_crate::m::Trait")).unwrap());

        let path: syn::Path = syn::parse_str("crate::Trait").unwrap();
        let k = trait_key_from_path(&path, &ctx).unwrap();
        assert_eq!(k, type_key(&ty("my_crate::Trait")).unwrap());

        let path: syn::Path = syn::parse_str("::Trait").unwrap();
        let k = trait_key_from_path(&path, &ctx).unwrap();
        assert_eq!(k, type_key(&ty("::Trait")).unwrap());
    }

    #[test]
    fn snake_ident_from_type_与_to_snake_覆盖边界() {
        assert_eq!(snake_ident_from_type("crate::FooBar").unwrap(), "foo_bar");
        assert_eq!(snake_ident_from_type("&Foo").unwrap(), "");

        let err = snake_ident_from_type("###").unwrap_err().to_string();
        assert!(err.contains("无法解析类型用于生成字段名"));

        assert_eq!(to_snake("Foo"), "foo");
        assert_eq!(to_snake("FooBar"), "foo_bar");
        assert_eq!(to_snake("FOOBar"), "foobar");
        assert_eq!(to_snake("Foo2Bar"), "foo2_bar");
        assert_eq!(to_snake("fooBar"), "foo_bar");
    }

    #[test]
    fn stable_hash_u64_结果稳定() {
        assert_eq!(stable_hash_u64(""), 0xcbf29ce484222325);
        assert_eq!(stable_hash_u64("hello"), 0xa430d84680aabd0b);
    }
}
