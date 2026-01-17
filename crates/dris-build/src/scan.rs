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
        scan_items(&ast.items, &ctx, &mut out, false)?;
    }

    Ok((out, touched_files))
}

fn scan_items(items: &[Item], ctx: &ScanCtx, out: &mut ScanOut, cfg_guarded: bool) -> Result<()> {
    for item in items {
        match item {
            Item::Struct(item_struct) => {
                if cfg_attr_mentions_marker(&item_struct.attrs, "component") {
                    return Err(anyhow!(
                        "不支持用 #[cfg_attr] 条件启用 #[component]：dris-build 不做 cfg 条件裁剪，也无法在 build.rs 阶段判断该组件是否存在；请把组件定义写成普通源码，并把 cfg 放到组件内部实现细节里：{}",
                        item_struct.ident
                    ));
                }
                if !has_attr(&item_struct.attrs, "component") {
                    continue;
                }
                if cfg_guarded || has_cfg_like_attr(&item_struct.attrs) {
                    return Err(anyhow!(
                        "不支持在 cfg/cfg_attr 作用域里声明组件：{}（dris-build 扫描不理解 cfg，可能生成不存在的代码）。建议：把 cfg 放到组件内部实现细节里，不要切换组件类型/构造函数本身。",
                        item_struct.ident
                    ));
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
                let impl_cfg_guarded = cfg_guarded || has_cfg_like_attr(&item_impl.attrs);
                if impl_cfg_guarded && cfg_attr_mentions_marker(&item_impl.attrs, "constructor") {
                    return Err(anyhow!(
                        "不支持用 #[cfg_attr] 条件启用 #[constructor]：{}",
                        self_ty.key
                    ));
                }

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
                        if cfg_attr_mentions_marker(&impl_fn.attrs, "constructor") {
                            return Err(anyhow!(
                                "不支持用 #[cfg_attr] 条件启用 #[constructor]：{}::{}",
                                self_ty.key,
                                impl_fn.sig.ident
                            ));
                        }
                        let is_ctor = has_attr(&impl_fn.attrs, "constructor");
                        if !is_ctor {
                            continue;
                        }
                        if impl_cfg_guarded || has_cfg_like_attr(&impl_fn.attrs) {
                            return Err(anyhow!(
                                "不支持在 cfg/cfg_attr 作用域里声明构造函数：{}::{}（dris-build 扫描不理解 cfg，可能生成不存在的代码）。建议：把 cfg 放到构造函数内部实现细节里，不要切换构造函数本身。",
                                self_ty.key,
                                impl_fn.sig.ident
                            ));
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
                let next_cfg_guarded = cfg_guarded || has_cfg_like_attr(&item_mod.attrs);
                scan_items(inline_items, &next, out, next_cfg_guarded)?;
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

    let mut params = Vec::new();
    for arg in &impl_fn.sig.inputs {
        match arg {
            syn::FnArg::Typed(pat_type) => {
                params.push(parse_inject_param(&pat_type.ty, ctx)?);
            }
            syn::FnArg::Receiver(_) => {
                return Err(anyhow!(
                    "#[constructor] 构造函数不支持 self 参数：{}",
                    impl_fn.sig.ident
                ));
            }
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

fn has_cfg_like_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| {
        let last = attr.path().segments.last().map(|s| s.ident.to_string());
        matches!(last.as_deref(), Some("cfg") | Some("cfg_attr"))
    })
}

fn cfg_attr_mentions_marker(attrs: &[syn::Attribute], marker: &str) -> bool {
    attrs.iter().any(|attr| {
        let last = attr.path().segments.last().map(|s| s.ident.to_string());
        if last.as_deref() != Some("cfg_attr") {
            return false;
        }
        attr.to_token_stream().to_string().contains(marker)
    })
}

fn parse_scope_override(
    attrs: &[syn::Attribute],
    attr_name: &str,
) -> Result<Option<ComponentScope>> {
    let mut seen = false;
    let mut out = None;

    for attr in attrs {
        let last = attr
            .path()
            .segments
            .last()
            .expect("Attribute path 不应为空")
            .ident
            .to_string();
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

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn ctx() -> ScanCtx {
        ScanCtx {
            crate_ident: "my_crate".to_string(),
            module_path: vec!["m".to_string()],
        }
    }

    #[test]
    fn attrs_helpers_覆盖常见判断() {
        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component])];
        assert!(has_attr(&attrs, "component"));
        assert!(!has_attr(&attrs, "constructor"));

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[cfg(feature = "x")])];
        assert!(has_cfg_like_attr(&attrs));

        let attrs: Vec<syn::Attribute> =
            vec![syn::parse_quote!(#[cfg_attr(feature = "x", component)])];
        assert!(has_cfg_like_attr(&attrs));
        assert!(cfg_attr_mentions_marker(&attrs, "component"));
        assert!(!cfg_attr_mentions_marker(&attrs, "constructor"));
    }

    #[test]
    fn parse_scope_override_覆盖无参数_重复_不支持形式_合法参数_错误参数() {
        assert_eq!(parse_scope_override(&[], "component").unwrap(), None);

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component])];
        assert_eq!(parse_scope_override(&attrs, "component").unwrap(), None);

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component = "x"])];
        let err = parse_scope_override(&attrs, "component")
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持 name-value"));

        let attrs: Vec<syn::Attribute> = vec![
            syn::parse_quote!(#[component(singleton)]),
            syn::parse_quote!(#[component]),
        ];
        let err = parse_scope_override(&attrs, "component")
            .unwrap_err()
            .to_string();
        assert!(err.contains("不允许重复标记"));

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component(singleton)])];
        assert_eq!(
            parse_scope_override(&attrs, "component").unwrap(),
            Some(ComponentScope::Singleton)
        );

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component(prototype)])];
        assert_eq!(
            parse_scope_override(&attrs, "component").unwrap(),
            Some(ComponentScope::Prototype)
        );

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component(scope = "Singleton")])];
        assert_eq!(
            parse_scope_override(&attrs, "component").unwrap(),
            Some(ComponentScope::Singleton)
        );

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component(scope = "unknown")])];
        let err = parse_scope_override(&attrs, "component")
            .unwrap_err()
            .to_string();
        assert!(err.contains("未知的 scope"));

        let attrs: Vec<syn::Attribute> =
            vec![syn::parse_quote!(#[component(singleton, prototype)])];
        let err = parse_scope_override(&attrs, "component")
            .unwrap_err()
            .to_string();
        assert!(err.contains("重复指定 scope"));

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[derive(Debug)])];
        assert_eq!(parse_scope_override(&attrs, "component").unwrap(), None);

        let attrs: Vec<syn::Attribute> =
            vec![syn::parse_quote!(#[component(singleton, singleton)])];
        let err = parse_scope_override(&attrs, "component")
            .unwrap_err()
            .to_string();
        assert!(err.contains("重复指定 scope"));

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(
            #[component(scope = "singleton", scope = "prototype")]
        )];
        let err = parse_scope_override(&attrs, "component")
            .unwrap_err()
            .to_string();
        assert!(err.contains("重复指定 scope"));

        let attrs: Vec<syn::Attribute> = vec![syn::parse_quote!(#[component(xxx)])];
        let err = parse_scope_override(&attrs, "component")
            .unwrap_err()
            .to_string();
        assert!(err.contains("未知的参数"));
    }

    #[test]
    fn module_path_from_file_覆盖常见路径规则() {
        let tmp = TempDir::new().unwrap();
        let src = tmp.path().join("src");
        std::fs::create_dir_all(src.join("foo")).unwrap();

        assert_eq!(
            module_path_from_file(&src, &src).unwrap(),
            Vec::<String>::new()
        );

        let p = src.join("lib.rs");
        assert_eq!(
            module_path_from_file(&src, &p).unwrap(),
            Vec::<String>::new()
        );

        let p = src.join("main.rs");
        assert_eq!(
            module_path_from_file(&src, &p).unwrap(),
            Vec::<String>::new()
        );

        let p = src.join("foo.rs");
        assert_eq!(
            module_path_from_file(&src, &p).unwrap(),
            vec!["foo".to_string()]
        );

        let p = src.join("foo").join("mod.rs");
        assert_eq!(
            module_path_from_file(&src, &p).unwrap(),
            vec!["foo".to_string()]
        );

        let p = src.join("foo").join("bar.rs");
        assert_eq!(
            module_path_from_file(&src, &p).unwrap(),
            vec!["foo".to_string(), "bar".to_string()]
        );

        let other = tmp.path().join("other.rs");
        let err = module_path_from_file(&src, &other).unwrap_err().to_string();
        assert!(err.contains("计算相对路径失败"));
    }

    #[test]
    fn return_and_ctor_helpers_覆盖关键约束() {
        let ctx = ctx();
        let self_ty = type_ref_from_type(&syn::parse_str("Foo").unwrap(), &ctx, true).unwrap();

        assert!(return_matches_self(&syn::parse_str("Self").unwrap(), &self_ty, &ctx).unwrap());
        assert!(return_matches_self(&syn::parse_str("Foo").unwrap(), &self_ty, &ctx).unwrap());
        assert!(
            return_matches_self(&syn::parse_str("my_crate::m::Foo").unwrap(), &self_ty, &ctx)
                .unwrap()
        );
        assert!(!return_matches_self(&syn::parse_str("&Foo").unwrap(), &self_ty, &ctx).unwrap());
        assert!(!return_matches_self(&syn::parse_str("::Foo").unwrap(), &self_ty, &ctx).unwrap());
        assert!(
            !return_matches_self(
                &syn::parse_str("<Foo as Bar>::Baz").unwrap(),
                &self_ty,
                &ctx
            )
            .unwrap()
        );
        assert!(!return_matches_self(&syn::parse_str("Bar").unwrap(), &self_ty, &ctx).unwrap());

        let err = normalize_inject_return(&ReturnType::Default, &ctx)
            .err()
            .unwrap()
            .to_string();
        assert!(err.contains("必须显式声明返回类型"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            fn new() -> Self { Self }
        };
        let err = parse_inject_ctor(&f, &self_ty, &ctx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("必须是 pub"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            pub fn new<T>() -> Self { Self }
        };
        let err = parse_inject_ctor(&f, &self_ty, &ctx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持泛型"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            pub fn new(&self) -> Self { Self }
        };
        let err = parse_inject_ctor(&f, &self_ty, &ctx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持 self 参数"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            pub fn new() { }
        };
        let err = parse_inject_ctor(&f, &self_ty, &ctx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("必须显式声明返回类型"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            pub fn new() -> std::sync::Arc<Self> { std::sync::Arc::new(Self) }
        };
        let err = parse_inject_ctor(&f, &self_ty, &ctx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持 #[constructor] 返回 Arc<Self>/Rc<Self>"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            pub fn new() -> u8 { 1 }
        };
        let err = parse_inject_ctor(&f, &self_ty, &ctx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("返回类型必须是 Self"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            pub fn new(_dep: Vec<u8>) -> Self { Self }
        };
        let err = parse_inject_ctor(&f, &self_ty, &ctx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("而不是 Vec"));

        let f: syn::ImplItemFn = syn::parse_quote! {
            #[constructor]
            pub fn new(_dep: std::sync::Arc<u8>) -> Self { Self }
        };
        let out = parse_inject_ctor(&f, &self_ty, &ctx).unwrap();
        assert!(out.call_path.contains("Foo"));
        assert!(out.call_path.contains("new"));
        assert_eq!(out.params.len(), 1);
    }

    #[test]
    fn collect_rs_files_覆盖非rs文件与symlink() {
        let tmp = TempDir::new().unwrap();
        let src = tmp.path().join("src");
        std::fs::create_dir_all(&src).unwrap();

        std::fs::write(src.join("a.rs"), "pub struct A;").unwrap();
        std::fs::write(src.join("note.txt"), "x").unwrap();

        #[cfg(unix)]
        {
            use std::os::unix::fs::symlink;
            symlink(src.join("a.rs"), src.join("a_link")).unwrap();
        }

        let mut files = Vec::new();
        collect_rs_files(&src, &mut files).unwrap();
        assert!(files.iter().any(|p| p.ends_with("a.rs")));
        assert!(!files.iter().any(|p| p.ends_with("note.txt")));
        assert!(!files.iter().any(|p| p.ends_with("a_link")));
    }

    #[test]
    fn scan_crate_覆盖无src与基本扫描() {
        let tmp = TempDir::new().unwrap();
        std::fs::write(
            tmp.path().join("Cargo.toml"),
            "[package]\nname=\"t\"\nversion=\"0.1.0\"\n",
        )
        .unwrap();

        let (out, touched) = scan_crate(tmp.path(), "crate").unwrap();
        assert!(out.components.is_empty());
        assert!(touched.contains(&tmp.path().join("Cargo.toml")));

        let src = tmp.path().join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::create_dir_all(src.join("sub")).unwrap();
        std::fs::write(
            src.join("lib.rs"),
            r#"
use dris_rt::{component, constructor};

#[component(singleton)]
pub struct A;
impl A {
    #[constructor]
    pub fn new() -> Self { Self }
}

mod inner {
    use dris_rt::{component, constructor};

    #[component]
    pub struct B;
    impl B {
        #[constructor]
        pub fn new() -> Self { Self }
    }
}
"#,
        )
        .unwrap();
        std::fs::write(
            src.join("sub/mod.rs"),
            r#"
use dris_rt::component;

#[component]
pub struct C {
    pub ok: u8,
    bad: u8,
    pub xs: Vec<u8>,
}
"#,
        )
        .unwrap();

        let (out, touched) = scan_crate(tmp.path(), "crate").unwrap();
        assert_eq!(out.components.len(), 3);
        assert!(touched.contains(&tmp.path().join("Cargo.toml")));
        assert!(touched.contains(&src.join("lib.rs")));
        assert!(touched.contains(&src.join("sub/mod.rs")));
        assert!(
            out.components
                .iter()
                .any(|c| c.type_key.contains("crate :: A"))
        );
        assert!(
            out.components
                .iter()
                .any(|c| c.type_key.contains("crate :: inner :: B"))
        );
        assert!(
            out.components
                .iter()
                .any(|c| c.type_key.contains("crate :: sub :: C"))
        );
    }

    #[test]
    fn scan_items_禁止cfg_attr条件启用component() {
        let ast: syn::File = syn::parse_quote! {
            #[cfg_attr(feature = "x", component)]
            pub struct A;
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持用 #[cfg_attr] 条件启用 #[component]"));
    }

    #[test]
    fn scan_items_覆盖cfg作用域组件与构造函数限制() {
        // 组件声明在 cfg/cfg_attr 作用域里。
        let ast: syn::File = syn::parse_quote! {
            #[cfg(feature = "x")]
            #[component]
            pub struct A;
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持在 cfg/cfg_attr 作用域里声明组件"));

        // 覆盖 cfg_guarded=true 的分支。
        let ast: syn::File = syn::parse_quote! {
            #[component]
            pub struct A;
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, true)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持在 cfg/cfg_attr 作用域里声明组件"));

        // impl 级别 cfg：允许存在（只要不放 constructor）。
        let ast: syn::File = syn::parse_quote! {
            #[cfg(feature = "x")]
            impl A {}
        };
        let mut out = ScanOut::default();
        scan_items(&ast.items, &ctx(), &mut out, false).unwrap();

        // 覆盖 cfg_guarded=true 时的 impl_cfg_guarded 分支。
        let ast: syn::File = syn::parse_quote! {
            impl A {}
        };
        let mut out = ScanOut::default();
        scan_items(&ast.items, &ctx(), &mut out, true).unwrap();

        // impl 级别 cfg_attr 条件启用 constructor。
        let ast: syn::File = syn::parse_quote! {
            #[cfg_attr(feature = "x", constructor)]
            impl A {}
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持用 #[cfg_attr] 条件启用 #[constructor]"));

        // fn 级别 cfg_attr 条件启用 constructor。
        let ast: syn::File = syn::parse_quote! {
            impl A {
                #[cfg_attr(feature = "x", constructor)]
                pub fn new() -> Self { Self }
            }
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持用 #[cfg_attr] 条件启用 #[constructor]"));

        // 在 cfg 作用域里声明构造函数。
        let ast: syn::File = syn::parse_quote! {
            impl A {
                #[constructor]
                #[cfg(feature = "x")]
                pub fn new() -> Self { Self }
            }
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持在 cfg/cfg_attr 作用域里声明构造函数"));

        // 覆盖：impl_cfg_guarded=true 但 cfg_attr_mentions_marker=false 的分支。
        let ast: syn::File = syn::parse_quote! {
            #[cfg(feature = "x")]
            impl A {}
        };
        let mut out = ScanOut::default();
        scan_items(&ast.items, &ctx(), &mut out, false).unwrap();

        // 覆盖：由 impl_cfg_guarded=true 触发的构造函数限制。
        let ast: syn::File = syn::parse_quote! {
            #[cfg(feature = "x")]
            impl A {
                #[constructor]
                pub fn new() -> Self { Self }
            }
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持在 cfg/cfg_attr 作用域里声明构造函数"));
    }

    #[test]
    fn scan_items_覆盖trait_impl_移除函数provider_非inline_mod跳过() {
        let ast: syn::File = syn::parse_quote! {
            mod external;

            #[bean]
            fn provider() {}

            pub trait T {}
            #[component]
            pub struct A;
            impl T for A {}
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("已移除函数 provider"));

        let ast: syn::File = syn::parse_quote! {
            #[component]
            fn provider() {}
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("已移除函数 provider"));

        let ast: syn::File = syn::parse_quote! {
            mod external;

            pub trait T {}
            #[component]
            pub struct A;
            impl T for A {}
        };
        let mut out = ScanOut::default();
        scan_items(&ast.items, &ctx(), &mut out, false).unwrap();
        assert_eq!(out.trait_impls.len(), 1);
    }

    #[test]
    fn scan_items_会跳过未标记component的结构体() {
        let ast: syn::File = syn::parse_quote! {
            pub struct NotComponent;

            #[component]
            pub struct A;
        };
        let mut out = ScanOut::default();
        scan_items(&ast.items, &ctx(), &mut out, false).unwrap();
        assert_eq!(out.components.len(), 1);
        assert_eq!(out.components[0].struct_name, "A");
    }

    #[test]
    fn scan_items_覆盖更多结构体与impl分支() {
        // type_key_from_qualified_ident 解析失败（模块路径含非法 ident）。
        let bad_ctx = ScanCtx {
            crate_ident: "crate".to_string(),
            module_path: vec!["bad-name".to_string()],
        };
        let ast: syn::File = syn::parse_quote! {
            #[component]
            pub struct A;
        };
        let mut out = ScanOut::default();
        assert!(scan_items(&ast.items, &bad_ctx, &mut out, false).is_err());

        // tuple struct + named 字段里混入一个 ident=None 的字段（覆盖 continue 分支）。
        let ast: syn::File = syn::parse_quote! {
            #[component]
            pub struct Tup(u8);

            #[component]
            pub struct Named {
                pub a: u8,
            }
        };
        let mut out = ScanOut::default();
        scan_items(&ast.items, &ctx(), &mut out, false).unwrap();
        assert!(
            out.components
                .iter()
                .any(|c| matches!(c.fields, ComponentFieldsRaw::Tuple))
        );

        // 人工插入一个无名字段到 named fields，同时覆盖 if let 的匹配与不匹配分支。
        let cases: Vec<(syn::File, bool)> = vec![
            (syn::parse_quote! { fn nope() {} }, false),
            (
                syn::parse_quote! {
                    #[component]
                    pub struct Weird(u8);
                },
                false,
            ),
            (
                syn::parse_quote! {
                    #[component]
                    pub struct Weird {
                        pub a: u8,
                    }
                },
                true,
            ),
        ];
        for (mut ast, expect_inserted) in cases {
            let mut inserted = false;
            if let Item::Struct(s) = &mut ast.items[0] {
                if let syn::Fields::Named(named) = &mut s.fields {
                    named.named.push(syn::parse_quote!(pub u8));
                    inserted = true;
                }
            }
            assert_eq!(inserted, expect_inserted);

            if inserted {
                let mut out = ScanOut::default();
                scan_items(&ast.items, &ctx(), &mut out, false).unwrap();
            }
        }

        // impl item 非 fn、以及普通方法（非 constructor）会被跳过。
        let ast: syn::File = syn::parse_quote! {
            impl A {
                type X = u8;
                pub fn helper() {}

                #[constructor]
                pub fn new() -> Self { Self }
            }

            fn normal() {}
        };
        let mut out = ScanOut::default();
        scan_items(&ast.items, &ctx(), &mut out, false).unwrap();
        assert_eq!(out.injects.len(), 1);

        // 覆盖：cfg_guarded=true 传播到 inline mod。
        let ast: syn::File = syn::parse_quote! {
            mod inner {
                #[component]
                pub struct A;
            }
        };
        let mut out = ScanOut::default();
        let err = scan_items(&ast.items, &ctx(), &mut out, true)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不支持在 cfg/cfg_attr 作用域里声明组件"));
    }
}
