use anyhow::{Result, anyhow};
use quote::ToTokens;
use syn::Type;

use crate::model::{InjectParamRaw, ScanCtx, SharedKind, TraitObjectInfo};
use crate::type_util::{rewrite_type_crate_prefix, type_key, type_ref_from_type};

pub(crate) fn parse_inject_field(field_ty: &Type, ctx: &ScanCtx) -> Result<InjectParamRaw> {
    let field_ty = rewrite_type_crate_prefix(field_ty, &ctx.crate_ident)?;

    if let Type::Reference(tr) = &field_ty {
        if tr.mutability.is_some() {
            return Err(anyhow!(
                "普通依赖不支持 &mut 注入，请使用内部可变性（RefCell/Mutex/RwLock 等）"
            ));
        }
        let dep = type_ref_from_type(tr.elem.as_ref(), ctx, true)?;
        return Ok(InjectParamRaw::SingleBorrow(dep));
    }

    if let Some((kind, inner)) = extract_shared_inner_type(&field_ty) {
        if matches!(inner, Type::TraitObject(_)) {
            let info = trait_object_info(inner, ctx)?;
            return Ok(InjectParamRaw::SingleTraitRef { kind, info });
        }
        let dep = type_ref_from_type(inner, ctx, true)?;
        return Ok(InjectParamRaw::SingleRef { kind, dep });
    }

    if let Some(item_ty) = extract_dris_all_inner_type(&field_ty) {
        if let Some((kind, item_inner)) = extract_shared_inner_type(item_ty) {
            let info = trait_object_info(item_inner, ctx)?;
            return Ok(InjectParamRaw::AllList { kind, info });
        }

        if let Type::Tuple(tup) = item_ty {
            if tup.elems.len() != 2 {
                return Err(anyhow!(
                    "dris_rt::All 注入 Item 必须是 (dris_rt::Type, Arc/Rc<dyn Trait>)"
                ));
            }
            let k = &tup.elems[0];
            let v = &tup.elems[1];
            if !is_dris_type_key(k, ctx)? {
                return Err(anyhow!(
                    "dris_rt::All Map 注入 Item 的第一个元素必须是 dris_rt::Type"
                ));
            }
            let Some((kind, v_inner)) = extract_shared_inner_type(v) else {
                return Err(anyhow!(
                    "dris_rt::All Map 注入 Item 的第二个元素必须是 Arc/Rc<dyn Trait>"
                ));
            };
            let info = trait_object_info(v_inner, ctx)?;
            return Ok(InjectParamRaw::AllMap { kind, info });
        }

        return Err(anyhow!(
            "多组件字段注入必须是 dris_rt::All<Arc/Rc<dyn Trait>> 或 dris_rt::All<(dris_rt::Type, Arc/Rc<dyn Trait>)>"
        ));
    }

    if let Some(item_ty) = extract_iter_inner_type(&field_ty) {
        let mut looks_like_multi = extract_shared_inner_type(item_ty).is_some();
        if !looks_like_multi {
            if let Type::Tuple(tup) = item_ty {
                if tup.elems.len() == 2
                    && is_dris_type_key(&tup.elems[0], ctx)?
                    && extract_shared_inner_type(&tup.elems[1]).is_some()
                {
                    looks_like_multi = true;
                }
            }
        }

        if looks_like_multi {
            return Err(anyhow!(
                "多组件字段注入请使用 dris_rt::All，不再支持 Iter/Iterator 形式"
            ));
        }
    }

    if let Some(inner) = extract_vec_inner_type(&field_ty) {
        let Some((_kind, inner_ref)) = extract_shared_inner_type(inner) else {
            return Err(anyhow!("多组件字段注入请使用 dris_rt::All，而不是 Vec"));
        };
        let info = trait_object_info(inner_ref, ctx)?;
        return Err(anyhow!(
            "多组件字段注入请使用 dris_rt::All<Arc/Rc<dyn {}>>，不要使用 Vec",
            info.primary_trait_key
        ));
    }

    if let Some((k, v)) = extract_hashmap_types(&field_ty) {
        if !is_dris_type_key(k, ctx)? {
            return Err(anyhow!("Map 注入 key 必须是 dris_rt::Type"));
        }
        let Some((_kind, v_ref)) = extract_shared_inner_type(v) else {
            return Err(anyhow!("多组件字段注入请使用 dris_rt::All，而不是 HashMap"));
        };
        let info = trait_object_info(v_ref, ctx)?;
        return Err(anyhow!(
            "多组件字段注入请使用 dris_rt::All<(dris_rt::Type, Arc/Rc<dyn {}>)>，不要使用 HashMap",
            info.primary_trait_key
        ));
    }

    if matches!(field_ty, Type::Path(_)) {
        let dep = type_ref_from_type(&field_ty, ctx, true)?;
        return Ok(InjectParamRaw::SingleOwned(dep));
    }

    Err(anyhow!("不支持的字段类型：{}", field_ty.to_token_stream()))
}

pub(crate) fn parse_inject_param(param_ty: &Type, ctx: &ScanCtx) -> Result<InjectParamRaw> {
    let param_ty = rewrite_type_crate_prefix(param_ty, &ctx.crate_ident)?;

    if let Type::Reference(tr) = &param_ty {
        if tr.mutability.is_some() {
            return Err(anyhow!(
                "普通依赖不支持 &mut 注入，请使用内部可变性（RefCell/Mutex/RwLock 等）"
            ));
        }
        let dep = type_ref_from_type(tr.elem.as_ref(), ctx, true)?;
        return Ok(InjectParamRaw::SingleBorrow(dep));
    }

    if let Some((kind, inner)) = extract_shared_inner_type(&param_ty) {
        if matches!(inner, Type::TraitObject(_)) {
            let info = trait_object_info(inner, ctx)?;
            return Ok(InjectParamRaw::SingleTraitRef { kind, info });
        }
        let dep = type_ref_from_type(inner, ctx, true)?;
        return Ok(InjectParamRaw::SingleRef { kind, dep });
    }

    if let Some(item_ty) = extract_dris_all_inner_type(&param_ty) {
        if let Some((kind, item_inner)) = extract_shared_inner_type(item_ty) {
            let info = trait_object_info(item_inner, ctx)?;
            return Ok(InjectParamRaw::AllList { kind, info });
        }

        if let Type::Tuple(tup) = item_ty {
            if tup.elems.len() != 2 {
                return Err(anyhow!(
                    "dris_rt::All 注入 Item 必须是 (dris_rt::Type, Arc/Rc<dyn Trait>)"
                ));
            }
            let k = &tup.elems[0];
            let v = &tup.elems[1];
            if !is_dris_type_key(k, ctx)? {
                return Err(anyhow!(
                    "dris_rt::All Map 注入 Item 的第一个元素必须是 dris_rt::Type"
                ));
            }
            let Some((kind, v_inner)) = extract_shared_inner_type(v) else {
                return Err(anyhow!(
                    "dris_rt::All Map 注入 Item 的第二个元素必须是 Arc/Rc<dyn Trait>"
                ));
            };
            let info = trait_object_info(v_inner, ctx)?;
            return Ok(InjectParamRaw::AllMap { kind, info });
        }

        return Err(anyhow!(
            "多组件注入必须是 dris_rt::All<Arc/Rc<dyn Trait>> 或 dris_rt::All<(dris_rt::Type, Arc/Rc<dyn Trait>)>"
        ));
    }

    if let Some(item_ty) = extract_iter_inner_type(&param_ty) {
        let mut looks_like_multi = extract_shared_inner_type(item_ty).is_some();
        if !looks_like_multi {
            if let Type::Tuple(tup) = item_ty {
                if tup.elems.len() == 2
                    && is_dris_type_key(&tup.elems[0], ctx)?
                    && extract_shared_inner_type(&tup.elems[1]).is_some()
                {
                    looks_like_multi = true;
                }
            }
        }

        if looks_like_multi {
            return Err(anyhow!(
                "多组件注入请使用 dris_rt::All，不再支持 Iter/Iterator 形式"
            ));
        }
    }

    if let Some(inner) = extract_vec_inner_type(&param_ty) {
        let Some((_kind, inner_ref)) = extract_shared_inner_type(inner) else {
            return Err(anyhow!("多组件注入请使用 dris_rt::All，而不是 Vec"));
        };
        let info = trait_object_info(inner_ref, ctx)?;
        return Err(anyhow!(
            "多组件注入请使用 dris_rt::All<Arc/Rc<dyn {}>>，不要使用 Vec",
            info.primary_trait_key
        ));
    }

    if let Some((k, v)) = extract_hashmap_types(&param_ty) {
        if !is_dris_type_key(k, ctx)? {
            return Err(anyhow!("Map 注入 key 必须是 dris_rt::Type"));
        }
        let Some((_kind, v_ref)) = extract_shared_inner_type(v) else {
            return Err(anyhow!("多组件注入请使用 dris_rt::All，而不是 HashMap"));
        };
        let info = trait_object_info(v_ref, ctx)?;
        return Err(anyhow!(
            "多组件注入请使用 dris_rt::All<(dris_rt::Type, Arc/Rc<dyn {}>)>，不要使用 HashMap",
            info.primary_trait_key
        ));
    }

    if let Some((kind, _)) = extract_iter_item_type(&param_ty) {
        let kind = match kind {
            IterTraitKind::Iterator => "Iterator",
            IterTraitKind::IntoIterator => "IntoIterator",
        };
        return Err(anyhow!(
            "多组件注入请使用 dris_rt::All，不要使用 impl {kind}"
        ));
    }

    if matches!(param_ty, Type::Path(_)) {
        let dep = type_ref_from_type(&param_ty, ctx, true)?;
        return Ok(InjectParamRaw::SingleOwned(dep));
    }

    Err(anyhow!(
        "不支持的 #[constructor] 参数类型：{}",
        param_ty.to_token_stream()
    ))
}

fn trait_object_info(ty: &Type, ctx: &ScanCtx) -> Result<TraitObjectInfo> {
    let mut ty = rewrite_type_crate_prefix(ty, &ctx.crate_ident)?;
    let Type::TraitObject(trait_obj) = &mut ty else {
        return Err(anyhow!("期望 dyn Trait"));
    };

    let mut primary: Option<String> = None;
    for bound in trait_obj.bounds.iter_mut() {
        let syn::TypeParamBound::Trait(tb) = bound else {
            continue;
        };
        if primary.is_some() {
            continue;
        }

        let is_single = tb.path.leading_colon.is_none() && tb.path.segments.len() == 1;
        let ident = tb
            .path
            .segments
            .first()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();
        if is_single && matches!(ident.as_str(), "Send" | "Sync") {
            return Err(anyhow!(
                "List/Map 注入必须提供明确的主 Trait（不能只有 Send/Sync）"
            ));
        }

        if is_single {
            let mut segments = Vec::<syn::PathSegment>::new();
            segments.push(syn::PathSegment {
                ident: syn::Ident::new(&ctx.crate_ident, tb.path.segments[0].ident.span()),
                arguments: syn::PathArguments::None,
            });
            for m in &ctx.module_path {
                segments.push(syn::PathSegment {
                    ident: syn::Ident::new(m, tb.path.segments[0].ident.span()),
                    arguments: syn::PathArguments::None,
                });
            }
            segments.extend(tb.path.segments.clone());
            tb.path.segments = syn::punctuated::Punctuated::from_iter(segments);
        }

        primary = Some(tb.path.to_token_stream().to_string());
    }

    let Some(primary_trait_key) = primary else {
        return Err(anyhow!("List/Map 注入必须包含 Trait bound"));
    };
    let trait_object_key = type_key(&ty)?;
    Ok(TraitObjectInfo {
        primary_trait_key,
        trait_object_key,
    })
}

fn is_dris_type_key(ty: &Type, ctx: &ScanCtx) -> Result<bool> {
    let ty = rewrite_type_crate_prefix(ty, &ctx.crate_ident)?;
    let Type::Path(tp) = ty else {
        return Ok(false);
    };
    let Some(last) = tp.path.segments.last() else {
        return Ok(false);
    };
    if last.ident != "Type" {
        return Ok(false);
    }
    Ok(true)
}

fn extract_vec_inner_type(ty: &Type) -> Option<&Type> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let last = type_path.path.segments.last()?;
    if last.ident != "Vec" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return None;
    };
    if args.args.len() != 1 {
        return None;
    }
    let syn::GenericArgument::Type(inner) = args.args.first()? else {
        return None;
    };
    Some(inner)
}

fn extract_hashmap_types(ty: &Type) -> Option<(&Type, &Type)> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let last = type_path.path.segments.last()?;
    if last.ident != "HashMap" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return None;
    };
    if args.args.len() != 2 {
        return None;
    }
    let mut it = args.args.iter();
    let syn::GenericArgument::Type(k) = it.next()? else {
        return None;
    };
    let syn::GenericArgument::Type(v) = it.next()? else {
        return None;
    };
    Some((k, v))
}

fn extract_dris_all_inner_type(ty: &Type) -> Option<&Type> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let last = type_path.path.segments.last()?;
    if last.ident != "All" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return None;
    };
    if args.args.len() != 1 {
        return None;
    }
    let syn::GenericArgument::Type(inner) = args.args.first()? else {
        return None;
    };
    Some(inner)
}

fn extract_iter_inner_type(ty: &Type) -> Option<&Type> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let last = type_path.path.segments.last()?;
    if last.ident != "Iter" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return None;
    };
    if args.args.len() != 1 {
        return None;
    }
    let syn::GenericArgument::Type(inner) = args.args.first()? else {
        return None;
    };
    Some(inner)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IterTraitKind {
    Iterator,
    IntoIterator,
}

fn extract_iter_item_type(ty: &Type) -> Option<(IterTraitKind, &Type)> {
    let Type::ImplTrait(it) = ty else {
        return None;
    };
    for bound in &it.bounds {
        let syn::TypeParamBound::Trait(tb) = bound else {
            continue;
        };
        let Some(last) = tb.path.segments.last() else {
            continue;
        };
        let kind = match last.ident.to_string().as_str() {
            "Iterator" => IterTraitKind::Iterator,
            "IntoIterator" => IterTraitKind::IntoIterator,
            _ => continue,
        };
        let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
            continue;
        };
        for arg in &args.args {
            let syn::GenericArgument::AssocType(at) = arg else {
                continue;
            };
            if at.ident == "Item" {
                return Some((kind, &at.ty));
            }
        }
    }
    None
}

pub(crate) fn extract_shared_inner_type(ty: &Type) -> Option<(SharedKind, &Type)> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let last = type_path.path.segments.last()?;
    let kind = match last.ident.to_string().as_str() {
        "Arc" => SharedKind::Arc,
        "Rc" => SharedKind::Rc,
        _ => return None,
    };
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return None;
    };
    if args.args.len() != 1 {
        return None;
    }
    let syn::GenericArgument::Type(inner) = args.args.first()? else {
        return None;
    };
    Some((kind, inner))
}
