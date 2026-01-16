use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, anyhow};
use quote::ToTokens;
use syn::{Item, ReturnType, Type};

use crate::{
    inject::{extract_shared_inner_type, parse_inject_field, parse_inject_param},
    model::{
        ComponentDef, ComponentFieldRaw, ComponentFieldsRaw, ComponentScope, InjectCtorRaw,
        ScanCtx, ScanOut, TraitImplRaw, TypeRef,
    },
    type_util::{
        rewrite_type_crate_prefix, trait_key_from_path, type_key_from_qualified_ident,
        type_ref_from_type,
    },
};

pub(crate) fn scan_crate(
    crate_dir: &Path,
    crate_ident: &str,
) -> Result<(ScanOut, BTreeSet<PathBuf>)> {
    let src_root = crate_dir.join("src");
    let mut touched_files = BTreeSet::<PathBuf>::new();
    let mut out = ScanOut::default();
    touched_files.insert(crate_dir.join("Cargo.toml"));

    if !src_root.exists() {
        return Ok((out, touched_files));
    }

    let mut rs_files = Vec::new();
    collect_rs_files(&src_root, &mut rs_files)
        .with_context(|| format!("遍历源码目录失败: {}", src_root.display()))?;

    for path in rs_files {
        touched_files.insert(path.clone());
        let content = fs::read_to_string(&path)
            .with_context(|| format!("读取源码失败: {}", path.display()))?;
        let module_path = module_path_from_file(&src_root, &path)?;

        let ast = syn::parse_file(&content)
            .with_context(|| format!("解析 Rust AST 失败: {}", path.display()))?;
        let ctx = ScanCtx {
            crate_ident: crate_ident.to_string(),
            module_path,
        };
        scan_items(&ast.items, &ctx, &mut out)?;
    }

    Ok((out, touched_files))
}

fn scan_items(items: &[Item], ctx: &ScanCtx, out: &mut ScanOut) -> Result<()> {
    for item in items {
        match item {
            Item::Struct(item_struct) => {
                if !has_attr(&item_struct.attrs, "component") {
                    continue;
                }
                let scope_override = parse_scope_override(&item_struct.attrs, "component")?;
                let type_key = type_key_from_qualified_ident(
                    &ctx.crate_ident,
                    &ctx.module_path,
                    &item_struct.ident.to_string(),
                )?;

                let fields = match &item_struct.fields {
                    syn::Fields::Unit => ComponentFieldsRaw::Unit,
                    syn::Fields::Unnamed(_) => ComponentFieldsRaw::Tuple,
                    syn::Fields::Named(named) => {
                        let mut out_fields = Vec::new();
                        for field in &named.named {
                            let Some(ident) = field.ident.as_ref() else {
                                continue;
                            };
                            let name = ident.to_string();
                            let is_pub = matches!(field.vis, syn::Visibility::Public(_));
                            let ty = field.ty.to_token_stream().to_string();
                            let (inject, inject_error) = match parse_inject_field(&field.ty, ctx) {
                                Ok(raw) => (Some(raw), None),
                                Err(e) => (None, Some(e.to_string())),
                            };
                            out_fields.push(ComponentFieldRaw {
                                name,
                                is_pub,
                                ty,
                                inject,
                                inject_error,
                            });
                        }
                        ComponentFieldsRaw::Named(out_fields)
                    }
                };

                out.components.push(ComponentDef {
                    crate_ident: ctx.crate_ident.clone(),
                    type_key,
                    struct_name: item_struct.ident.to_string(),
                    fields,
                    scope_override,
                });
            }
            Item::Impl(item_impl) => {
                let self_ty = type_ref_from_type(&item_impl.self_ty, ctx, true)?;

                if let Some((_, trait_path, _)) = &item_impl.trait_ {
                    let trait_key = trait_key_from_path(trait_path, ctx)?;
                    out.trait_impls.push(TraitImplRaw {
                        trait_key,
                        self_ty: self_ty.clone(),
                    });
                } else {
                    for impl_item in &item_impl.items {
                        let syn::ImplItem::Fn(impl_fn) = impl_item else {
                            continue;
                        };
                        let is_ctor = has_attr(&impl_fn.attrs, "constructor");
                        if !is_ctor {
                            continue;
                        }
                        out.injects.push(parse_inject_ctor(impl_fn, &self_ty, ctx)?);
                    }
                }
            }
            Item::Fn(item_fn) => {
                if has_attr(&item_fn.attrs, "bean") || has_attr(&item_fn.attrs, "component") {
                    return Err(anyhow!(
                        "已移除函数 provider：请用 newtype/struct + #[component] + #[constructor] 替代：{}",
                        item_fn.sig.ident
                    ));
                }
            }
            Item::Mod(item_mod) => {
                let Some((_, inline_items)) = &item_mod.content else {
                    continue;
                };
                let mut next = ctx.clone();
                next.module_path.push(item_mod.ident.to_string());
                scan_items(inline_items, &next, out)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn parse_inject_ctor(
    impl_fn: &syn::ImplItemFn,
    self_ty: &TypeRef,
    ctx: &ScanCtx,
) -> Result<InjectCtorRaw> {
    if !matches!(impl_fn.vis, syn::Visibility::Public(_)) {
        return Err(anyhow!(
            "#[constructor] 构造函数必须是 pub：{}",
            impl_fn.sig.ident
        ));
    }

    if !impl_fn.sig.generics.params.is_empty() {
        return Err(anyhow!(
            "暂不支持泛型 #[constructor] 构造函数：{}",
            impl_fn.sig.ident
        ));
    }

    for arg in &impl_fn.sig.inputs {
        if matches!(arg, syn::FnArg::Receiver(_)) {
            return Err(anyhow!(
                "#[constructor] 构造函数不支持 self 参数：{}",
                impl_fn.sig.ident
            ));
        }
    }

    let return_ty = normalize_inject_return(&impl_fn.sig.output, ctx)?;
    if extract_shared_inner_type(&return_ty).is_some() {
        return Err(anyhow!(
            "不支持 #[constructor] 返回 Arc<Self>/Rc<Self>（请让构造函数返回 Self，并用 #[component(singleton)] 显式声明单例）：{}",
            impl_fn.sig.ident
        ));
    }
    if !return_matches_self(&return_ty, self_ty, ctx)? {
        return Err(anyhow!(
            "#[constructor] 返回类型必须是 Self（或当前组件类型）：{}",
            impl_fn.sig.ident
        ));
    }

    let mut params = Vec::new();
    for arg in &impl_fn.sig.inputs {
        let syn::FnArg::Typed(pat_type) = arg else {
            continue;
        };
        params.push(parse_inject_param(&pat_type.ty, ctx)?);
    }

    Ok(InjectCtorRaw {
        self_ty: self_ty.clone(),
        call_path: format!("{}::{}", self_ty.key, impl_fn.sig.ident),
        params,
    })
}

fn normalize_inject_return(output: &ReturnType, ctx: &ScanCtx) -> Result<Type> {
    let ReturnType::Type(_, ty) = output else {
        return Err(anyhow!("#[constructor] 必须显式声明返回类型"));
    };
    rewrite_type_crate_prefix(ty, &ctx.crate_ident)
}

fn return_matches_self(return_ty: &Type, self_ty: &TypeRef, ctx: &ScanCtx) -> Result<bool> {
    let return_ty = rewrite_type_crate_prefix(return_ty, &ctx.crate_ident)?;

    if let Type::Path(tp) = &return_ty {
        if tp.qself.is_none()
            && tp.path.leading_colon.is_none()
            && tp.path.segments.len() == 1
            && tp.path.segments[0].ident == "Self"
        {
            return Ok(true);
        }
    }

    let return_ref = type_ref_from_type(&return_ty, ctx, true)?;
    Ok(return_ref.key == self_ty.key)
}

fn has_attr(attrs: &[syn::Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| {
        let last = attr.path().segments.last().map(|s| s.ident.to_string());
        matches!(last.as_deref(), Some(n) if n == name)
    })
}

fn parse_scope_override(
    attrs: &[syn::Attribute],
    attr_name: &str,
) -> Result<Option<ComponentScope>> {
    let mut seen = false;
    let mut out = None;

    for attr in attrs {
        let last = attr.path().segments.last().map(|s| s.ident.to_string());
        let Some(last) = last else {
            continue;
        };
        if last != attr_name {
            continue;
        }
        if seen {
            return Err(anyhow!("同一个 item 上不允许重复标记 #[{attr_name}]"));
        }
        seen = true;

        match &attr.meta {
            syn::Meta::Path(_) => {}
            syn::Meta::NameValue(_) => {
                return Err(anyhow!(
                    "#[{attr_name}] 不支持 name-value 形式，请使用 #[{attr_name}] 或 #[{attr_name}(singleton)] / #[{attr_name}(prototype)] / #[{attr_name}(scope = \"singleton\")]"
                ));
            }
            syn::Meta::List(_) => {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("singleton") {
                        if out.is_some() {
                            return Err(meta.error("重复指定 scope"));
                        }
                        out = Some(ComponentScope::Singleton);
                        return Ok(());
                    }
                    if meta.path.is_ident("prototype") {
                        if out.is_some() {
                            return Err(meta.error("重复指定 scope"));
                        }
                        out = Some(ComponentScope::Prototype);
                        return Ok(());
                    }

                    if meta.path.is_ident("scope") {
                        let value: syn::LitStr = meta.value()?.parse()?;
                        let value = value.value().to_lowercase();
                        let scope = match value.as_str() {
                            "singleton" => ComponentScope::Singleton,
                            "prototype" => ComponentScope::Prototype,
                            other => {
                                return Err(meta.error(format!(
                                    "未知的 scope：{other}（只支持 singleton/prototype）"
                                )));
                            }
                        };
                        if out.is_some() {
                            return Err(meta.error("重复指定 scope"));
                        }
                        out = Some(scope);
                        return Ok(());
                    }

                    Err(meta.error(
                        "未知的参数：只支持 singleton/prototype 或 scope = \"singleton\"/\"prototype\"",
                    ))
                })?;
            }
        }
    }

    Ok(out)
}

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let entries = fs::read_dir(dir)?;
    for entry in entries {
        let entry = entry?;
        let ty = entry.file_type()?;
        let path = entry.path();
        if ty.is_dir() {
            collect_rs_files(&path, out)?;
            continue;
        }
        if ty.is_file() && path.extension().and_then(|e| e.to_str()) == Some("rs") {
            out.push(path);
        }
    }
    Ok(())
}

fn module_path_from_file(src_root: &Path, file: &Path) -> Result<Vec<String>> {
    let rel = file
        .strip_prefix(src_root)
        .with_context(|| format!("计算相对路径失败: {}", file.display()))?;

    let mut parts: Vec<String> = rel
        .components()
        .map(|c| c.as_os_str().to_string_lossy().to_string())
        .collect();

    if parts.is_empty() {
        return Ok(Vec::new());
    }

    let file_name = parts.pop().ok_or_else(|| anyhow!("空文件名"))?;
    let file_stem = file_name.strip_suffix(".rs").unwrap_or(&file_name);
    match file_stem {
        "lib" | "main" => {}
        "mod" => {}
        other => parts.push(other.to_string()),
    }
    Ok(parts)
}
