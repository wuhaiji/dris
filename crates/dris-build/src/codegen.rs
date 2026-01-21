use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, anyhow};
use quote::{ToTokens, quote};

use crate::{
    model::{Component, ComponentScope, InjectParam, SharedKind},
    type_util::{snake_ident_from_type, stable_hash_u64},
};

fn shared_path(kind: SharedKind) -> &'static str {
    match kind {
        SharedKind::Arc => "std::sync::Arc",
        SharedKind::Rc => "std::rc::Rc",
    }
}

fn shared_type(kind: SharedKind, inner: &str) -> String {
    format!("{}<{inner}>", shared_path(kind))
}

fn shared_new(kind: SharedKind, expr: &str) -> String {
    format!("{}::new({expr})", shared_path(kind))
}

fn holder_kind_of(
    ty: &str,
    holder_kind_by_type: &BTreeMap<String, Option<SharedKind>>,
) -> Option<SharedKind> {
    holder_kind_by_type.get(ty).cloned().unwrap_or(None)
}

fn holder_type(holder: Option<SharedKind>, inner: &str) -> String {
    match holder {
        None => inner.to_string(),
        Some(kind) => shared_type(kind, inner),
    }
}

fn holder_new(holder: Option<SharedKind>, expr: &str) -> String {
    match holder {
        None => expr.to_string(),
        Some(kind) => shared_new(kind, expr),
    }
}

#[derive(Debug, Clone, Copy)]
enum AccessMode {
    Init,
    Runtime,
}

fn parse_ident(ident: &str) -> Result<syn::Ident> {
    syn::parse_str::<syn::Ident>(ident).with_context(|| format!("生成的标识符无法解析：{ident}"))
}

fn parse_type(ty: &str) -> Result<syn::Type> {
    syn::parse_str::<syn::Type>(ty).with_context(|| format!("无法解析类型：{ty}"))
}

fn parse_expr(expr: &str) -> Result<syn::Expr> {
    if expr.trim().is_empty() {
        return syn::parse_str::<syn::Expr>("()").context("无法解析空表达式为 ()");
    }
    syn::parse_str::<syn::Expr>(expr).with_context(|| format!("无法解析表达式：{expr}"))
}

fn format_generated(tokens: impl ToTokens) -> Result<String> {
    let file: syn::File = syn::parse2(tokens.to_token_stream()).context("生成的代码无法解析")?;
    Ok(prettyplease::unparse(&file))
}

pub(crate) fn minimal_generated_code() -> String {
    let tokens = quote! {
        #[allow(dead_code)]
        pub mod dris_gen {
            pub struct Container;

            impl Container {
                pub fn build() -> Self {
                    Self
                }
            }
        }
    };
    format_generated(tokens).expect("最小生成代码应当可格式化")
}

pub(crate) fn generate_container_code(
    components: &BTreeMap<String, Component>,
    trait_impls: &BTreeMap<String, Vec<String>>,
    order: &[String],
    roots: &[String],
    holder_kind_by_type: &BTreeMap<String, Option<SharedKind>>,
) -> Result<String> {
    let mut scope_by_type = BTreeMap::<String, ComponentScope>::new();
    for (ty, c) in components {
        scope_by_type.insert(ty.clone(), c.scope());
    }

    let mut name_by_type = BTreeMap::<String, String>::new();
    let mut used = BTreeSet::<String>::new();
    for ty in order {
        let mut name = snake_ident_from_type(ty)?;
        if name.is_empty() {
            name = format!("c{}", stable_hash_u64(ty));
        }
        if used.contains(&name) {
            let mut i = 2;
            while used.contains(&format!("{name}_{i}")) {
                i += 1;
            }
            name = format!("{name}_{i}");
        }
        used.insert(name.clone());
        name_by_type.insert(ty.clone(), name);
    }

    struct SingletonField {
        name: String,
        ty: String,
        init_expr: String,
        holder: Option<SharedKind>,
    }

    let mut singleton_fields = Vec::<SingletonField>::new();

    for ty in order {
        if scope_by_type
            .get(ty)
            .copied()
            .unwrap_or(ComponentScope::Prototype)
            != ComponentScope::Singleton
        {
            continue;
        }

        let field_name = name_by_type
            .get(ty)
            .ok_or_else(|| anyhow!("缺少字段名: {ty}"))?
            .clone();

        let init_expr = build_component_expr(
            ty,
            AccessMode::Init,
            components,
            trait_impls,
            &scope_by_type,
            &name_by_type,
            holder_kind_by_type,
        )?;

        let holder = holder_kind_of(ty, holder_kind_by_type);
        singleton_fields.push(SingletonField {
            name: field_name,
            ty: ty.to_string(),
            init_expr,
            holder,
        });
    }

    struct RootMethod {
        ty: String,
        method_name: String,
        scope: ComponentScope,
        expr: Option<String>,
        holder: Option<SharedKind>,
    }

    let root_set: BTreeSet<&str> = roots.iter().map(String::as_str).collect();
    let mut root_methods = Vec::<RootMethod>::new();
    for ty in order {
        if !root_set.contains(ty.as_str()) {
            continue;
        }

        let method_name = name_by_type
            .get(ty)
            .ok_or_else(|| anyhow!("缺少方法名: {ty}"))?
            .clone();
        let scope = scope_by_type
            .get(ty)
            .copied()
            .unwrap_or(ComponentScope::Prototype);

        let holder = holder_kind_of(ty, holder_kind_by_type);
        let expr = match scope {
            ComponentScope::Singleton => None,
            ComponentScope::Prototype => Some(build_component_expr(
                ty,
                AccessMode::Runtime,
                components,
                trait_impls,
                &scope_by_type,
                &name_by_type,
                holder_kind_by_type,
            )?),
        };

        root_methods.push(RootMethod {
            ty: ty.clone(),
            method_name,
            scope,
            expr,
            holder,
        });
    }

    let singleton_struct_fields = singleton_fields
        .iter()
        .map(|f| -> Result<_> {
            let name = parse_ident(&f.name)?;
            let ty = parse_type(&holder_type(f.holder, &f.ty))?;
            Ok(quote! { #name: #ty, })
        })
        .collect::<Result<Vec<_>>>()?;

    let singleton_let_stmts = singleton_fields
        .iter()
        .map(|f| -> Result<_> {
            let name = parse_ident(&f.name)?;
            let ty = parse_type(&holder_type(f.holder, &f.ty))?;
            let expr = parse_expr(&f.init_expr)
                .with_context(|| format!("解析单例组件 {} 的初始化表达式失败", f.ty))?;
            Ok(quote! { let #name: #ty = #expr; })
        })
        .collect::<Result<Vec<_>>>()?;

    let singleton_self_inits = singleton_fields
        .iter()
        .map(|f| -> Result<_> {
            let name = parse_ident(&f.name)?;
            Ok(quote! { #name, })
        })
        .collect::<Result<Vec<_>>>()?;

    let root_methods_tokens = root_methods
        .iter()
        .map(|m| -> Result<_> {
            let method_name = parse_ident(&m.method_name)?;
            match m.scope {
                ComponentScope::Singleton => match m.holder {
                    Some(kind) => {
                        let ret_ty = parse_type(&shared_type(kind, &m.ty))?;
                        Ok(quote! {
                            pub fn #method_name(&self) -> #ret_ty {
                                self.#method_name.clone()
                            }
                        })
                    }
                    None => {
                        let ret_ty = parse_type(&format!("&{}", m.ty))?;
                        Ok(quote! {
                            pub fn #method_name(&self) -> #ret_ty {
                                &self.#method_name
                            }
                        })
                    }
                },
                ComponentScope::Prototype => {
                    let ret_ty = parse_type(&m.ty)?;
                    let expr = parse_expr(m.expr.as_deref().unwrap_or_default())
                        .with_context(|| format!("解析原型组件 {} 的构造表达式失败", m.ty))?;
                    Ok(quote! {
                        pub fn #method_name(&self) -> #ret_ty {
                            #expr
                        }
                    })
                }
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let tokens = quote! {
        #[allow(dead_code)]
        pub mod dris_gen {
            pub struct Container {
                #(#singleton_struct_fields)*
            }

            impl Container {
                pub fn build() -> Self {
                    #(#singleton_let_stmts)*
                    Self {
                        #(#singleton_self_inits)*
                    }
                }

                #(#root_methods_tokens)*
            }
        }
    };

    format_generated(tokens)
}

fn build_component_expr(
    ty: &str,
    mode: AccessMode,
    components: &BTreeMap<String, Component>,
    trait_impls: &BTreeMap<String, Vec<String>>,
    scope_by_type: &BTreeMap<String, ComponentScope>,
    name_by_type: &BTreeMap<String, String>,
    holder_kind_by_type: &BTreeMap<String, Option<SharedKind>>,
) -> Result<String> {
    let component = components
        .get(ty)
        .ok_or_else(|| anyhow!("缺少组件: {ty}"))?;
    let scope = scope_by_type
        .get(ty)
        .copied()
        .unwrap_or(ComponentScope::Prototype);
    let holder = holder_kind_of(ty, holder_kind_by_type);
    if let Some(inject) = component.inject.as_ref() {
        let call_path = inject.call_path.clone();

        let mut lines = Vec::<String>::new();
        let mut args = Vec::<String>::new();
        let mut idx = 0usize;

        for p in &inject.params {
            match p {
                InjectParam::SingleRef { kind, dep_type } => {
                    let dep_holder = holder_kind_of(dep_type, holder_kind_by_type);
                    if dep_holder != Some(*kind) {
                        return Err(anyhow!(
                            "组件 {} 需要 {}<{}> 注入，但 {} 的持有方式不匹配（可能不是单例或发生了 Rc/Arc 冲突）",
                            ty,
                            shared_path(*kind),
                            dep_type,
                            dep_type
                        ));
                    }
                    let dep_name = name_by_type
                        .get(dep_type)
                        .ok_or_else(|| anyhow!("缺少依赖字段名: {dep_type}"))?;
                    let base = match mode {
                        AccessMode::Init => dep_name.to_string(),
                        AccessMode::Runtime => format!("self.{dep_name}"),
                    };
                    args.push(format!("{base}.clone()"));
                }
                InjectParam::SingleTraitRef {
                    kind,
                    trait_primary,
                    ..
                } => {
                    let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                    if impls.is_empty() {
                        return Err(anyhow!(
                            "Arc/Rc<dyn Trait> 注入需要 {} 的实现，但未找到任何实现",
                            trait_primary
                        ));
                    }
                    if impls.len() != 1 {
                        return Err(anyhow!(
                            "Arc/Rc<dyn Trait> 注入需要 {} 的实现，但找到多个实现（{}）：请改用 dris_rt::All<Arc/Rc<dyn Trait>> 或提供筛选机制（暂不支持）",
                            trait_primary,
                            impls.join(", ")
                        ));
                    }
                    let dep_type = &impls[0];
                    let dep_holder = holder_kind_of(dep_type, holder_kind_by_type);
                    if dep_holder != Some(*kind) {
                        return Err(anyhow!(
                            "组件 {} 需要 {}<dyn Trait> 注入 {}，但实现 {} 的持有方式不匹配（可能不是单例或发生了 Rc/Arc 冲突）",
                            ty,
                            shared_path(*kind),
                            trait_primary,
                            dep_type
                        ));
                    }
                    let dep_name = name_by_type
                        .get(dep_type)
                        .ok_or_else(|| anyhow!("缺少依赖字段名: {dep_type}"))?;
                    let base = match mode {
                        AccessMode::Init => dep_name.to_string(),
                        AccessMode::Runtime => format!("self.{dep_name}"),
                    };
                    args.push(format!("{base}.clone()"));
                }
                InjectParam::SingleBorrow { dep_type } => {
                    let dep_scope = scope_by_type
                        .get(dep_type)
                        .copied()
                        .unwrap_or(ComponentScope::Prototype);
                    if dep_scope == ComponentScope::Singleton {
                        let dep_holder = holder_kind_of(dep_type, holder_kind_by_type);
                        let dep_name = name_by_type
                            .get(dep_type)
                            .ok_or_else(|| anyhow!("缺少依赖字段名: {dep_type}"))?;
                        let base = match mode {
                            AccessMode::Init => dep_name.to_string(),
                            AccessMode::Runtime => format!("self.{dep_name}"),
                        };
                        match dep_holder {
                            None => args.push(format!("&{base}")),
                            Some(_) => args.push(format!("{base}.as_ref()")),
                        }
                    } else {
                        let dep_var = format!("dep{idx}");
                        idx += 1;
                        let dep_expr = build_component_expr(
                            dep_type,
                            mode,
                            components,
                            trait_impls,
                            scope_by_type,
                            name_by_type,
                            holder_kind_by_type,
                        )?;
                        lines.push(format!("let {dep_var} = {dep_expr};"));
                        let dep_ref_var = format!("{dep_var}_ref");
                        lines.push(format!("let {dep_ref_var} = &{dep_var};"));
                        args.push(dep_ref_var);
                    }
                }
                InjectParam::SingleOwned { dep_type } => {
                    let dep_var = format!("dep{idx}");
                    idx += 1;
                    let dep_expr = build_component_expr(
                        dep_type,
                        mode,
                        components,
                        trait_impls,
                        scope_by_type,
                        name_by_type,
                        holder_kind_by_type,
                    )?;
                    lines.push(format!("let {dep_var} = {dep_expr};"));
                    args.push(dep_var);
                }
                InjectParam::AllList {
                    kind,
                    trait_primary,
                    trait_object,
                    ..
                } => {
                    let dep_var = format!("dep{idx}");
                    idx += 1;
                    let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                    let expr =
                        build_list_all_expr(&impls, trait_object, name_by_type, mode, *kind)?;
                    lines.push(format!("let {dep_var} = {expr};"));
                    args.push(dep_var);
                }
                InjectParam::AllMap {
                    kind,
                    trait_primary,
                    trait_object,
                    ..
                } => {
                    let dep_var = format!("dep{idx}");
                    idx += 1;
                    let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                    let expr = build_map_all_expr(&impls, trait_object, name_by_type, mode, *kind)?;
                    lines.push(format!("let {dep_var} = {expr};"));
                    args.push(dep_var);
                }
            }
        }

        let call_expr = format!("{call_path}({})", args.join(", "));

        let expr = if lines.is_empty() {
            call_expr
        } else {
            let mut out = String::new();
            out.push_str("{\n");
            for line in lines {
                out.push_str("    ");
                out.push_str(&line);
                out.push('\n');
            }
            out.push_str("    ");
            out.push_str(&call_expr);
            out.push_str("\n}");
            out
        };

        return Ok(match scope {
            ComponentScope::Singleton => holder_new(holder, &expr),
            ComponentScope::Prototype => expr,
        });
    }

    let Some(fields) = component.auto_fields.as_ref() else {
        return Err(anyhow!("组件缺少 #[constructor] 且无法自动构造：{ty}"));
    };

    match &component.fields {
        crate::model::ComponentFieldsRaw::Unit => {
            let expr = ty.to_string();
            return Ok(match scope {
                ComponentScope::Singleton => holder_new(holder, &expr),
                ComponentScope::Prototype => expr,
            });
        }
        crate::model::ComponentFieldsRaw::Tuple => {
            return Err(anyhow!("组件是 tuple struct，无法自动构造：{ty}"));
        }
        crate::model::ComponentFieldsRaw::Named(_) => {}
    }

    let mut field_inits = Vec::<String>::new();
    for field in fields {
        let expr = match &field.param {
            InjectParam::SingleRef { kind, dep_type } => {
                let dep_holder = holder_kind_of(dep_type, holder_kind_by_type);
                if dep_holder != Some(*kind) {
                    return Err(anyhow!(
                        "自动构造字段 {} 需要 {}<{}> 注入，但 {} 的持有方式不匹配（可能不是单例或发生了 Rc/Arc 冲突）",
                        field.name,
                        shared_path(*kind),
                        dep_type,
                        dep_type
                    ));
                }
                let dep_name = name_by_type
                    .get(dep_type)
                    .ok_or_else(|| anyhow!("缺少依赖字段名: {dep_type}"))?;
                let base = match mode {
                    AccessMode::Init => dep_name.to_string(),
                    AccessMode::Runtime => format!("self.{dep_name}"),
                };
                format!("{base}.clone()")
            }
            InjectParam::SingleTraitRef {
                kind,
                trait_primary,
                ..
            } => {
                let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                if impls.is_empty() {
                    return Err(anyhow!(
                        "Arc/Rc<dyn Trait> 注入需要 {} 的实现，但未找到任何实现",
                        trait_primary
                    ));
                }
                if impls.len() != 1 {
                    return Err(anyhow!(
                        "Arc/Rc<dyn Trait> 注入需要 {} 的实现，但找到多个实现（{}）：请改用 dris_rt::All<Arc/Rc<dyn Trait>> 或提供筛选机制（暂不支持）",
                        trait_primary,
                        impls.join(", ")
                    ));
                }
                let dep_type = &impls[0];
                let dep_holder = holder_kind_of(dep_type, holder_kind_by_type);
                if dep_holder != Some(*kind) {
                    return Err(anyhow!(
                        "自动构造字段 {} 需要 {}<dyn Trait> 注入 {}，但实现 {} 的持有方式不匹配（可能不是单例或发生了 Rc/Arc 冲突）",
                        field.name,
                        shared_path(*kind),
                        trait_primary,
                        dep_type
                    ));
                }
                let dep_name = name_by_type
                    .get(dep_type)
                    .ok_or_else(|| anyhow!("缺少依赖字段名: {dep_type}"))?;
                let base = match mode {
                    AccessMode::Init => dep_name.to_string(),
                    AccessMode::Runtime => format!("self.{dep_name}"),
                };
                format!("{base}.clone()")
            }
            InjectParam::SingleOwned { dep_type } => build_component_expr(
                dep_type,
                mode,
                components,
                trait_impls,
                scope_by_type,
                name_by_type,
                holder_kind_by_type,
            )?,
            InjectParam::AllList {
                kind,
                trait_primary,
                trait_object,
                ..
            } => {
                let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                build_list_all_expr(&impls, trait_object, name_by_type, mode, *kind)?
            }
            InjectParam::AllMap {
                kind,
                trait_primary,
                trait_object,
                ..
            } => {
                let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                build_map_all_expr(&impls, trait_object, name_by_type, mode, *kind)?
            }
            other => {
                return Err(anyhow!("自动构造不支持的字段注入类型：{:?}", other));
            }
        };
        field_inits.push(format!("{}: {}", field.name, expr));
    }

    let mut out = String::new();
    out.push_str("{\n");
    out.push_str("    ");
    out.push_str(ty);
    out.push_str(" {\n");
    for init in field_inits {
        out.push_str("        ");
        out.push_str(&init);
        out.push_str(",\n");
    }
    out.push_str("    }\n");
    out.push_str("}");
    Ok(match scope {
        ComponentScope::Singleton => holder_new(holder, &out),
        ComponentScope::Prototype => out,
    })
}

fn build_list_all_expr(
    impls: &[String],
    trait_object: &str,
    name_by_type: &BTreeMap<String, String>,
    mode: AccessMode,
    kind: SharedKind,
) -> Result<String> {
    let mut s = String::new();
    s.push_str("{ let list: Vec<");
    s.push_str(shared_path(kind));
    s.push_str("<");
    s.push_str(trait_object);
    s.push_str(">> = vec![");
    let mut first = true;
    for ty in impls {
        let Some(f) = name_by_type.get(ty) else {
            continue;
        };
        if !first {
            s.push_str(", ");
        }
        first = false;
        match mode {
            AccessMode::Init => {
                s.push_str(f);
                s.push_str(".clone()");
            }
            AccessMode::Runtime => {
                s.push_str("self.");
                s.push_str(f);
                s.push_str(".clone()");
            }
        }
    }
    s.push_str("]; dris_rt::All::new(list) }");
    Ok(s)
}

fn build_map_all_expr(
    impls: &[String],
    trait_object: &str,
    name_by_type: &BTreeMap<String, String>,
    mode: AccessMode,
    kind: SharedKind,
) -> Result<String> {
    let mut s = String::new();
    s.push_str("{ let list: Vec<(dris_rt::Type, ");
    s.push_str(shared_path(kind));
    s.push_str("<");
    s.push_str(trait_object);
    s.push_str(">)> = vec![");
    let mut first = true;
    for ty in impls {
        let Some(f) = name_by_type.get(ty) else {
            continue;
        };
        if !first {
            s.push_str(", ");
        }
        first = false;
        s.push_str("(dris_rt::Type::of::<");
        s.push_str(ty);
        s.push_str(">(), ");
        match mode {
            AccessMode::Init => {
                s.push_str(f);
                s.push_str(".clone()");
            }
            AccessMode::Runtime => {
                s.push_str("self.");
                s.push_str(f);
                s.push_str(".clone()");
            }
        }
        s.push_str(")");
    }
    s.push_str("]; dris_rt::All::new(list) }");
    Ok(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{AutoField, ComponentFieldsRaw, InjectCtor};

    fn singleton_inject(ty: &str, params: Vec<InjectParam>) -> Component {
        Component {
            crate_ident: "crate".to_string(),
            type_key: ty.to_string(),
            struct_name: "X".to_string(),
            inject: Some(InjectCtor {
                call_path: format!("{ty}::new"),
                params,
            }),
            fields: ComponentFieldsRaw::Unit,
            auto_fields: None,
            scope_override: Some(ComponentScope::Singleton),
        }
    }

    fn prototype_inject(ty: &str, params: Vec<InjectParam>) -> Component {
        Component {
            crate_ident: "crate".to_string(),
            type_key: ty.to_string(),
            struct_name: "X".to_string(),
            inject: Some(InjectCtor {
                call_path: format!("{ty}::new"),
                params,
            }),
            fields: ComponentFieldsRaw::Unit,
            auto_fields: None,
            scope_override: None,
        }
    }

    fn prototype_unit_auto(ty: &str) -> Component {
        Component {
            crate_ident: "crate".to_string(),
            type_key: ty.to_string(),
            struct_name: "X".to_string(),
            inject: None,
            fields: ComponentFieldsRaw::Unit,
            auto_fields: Some(Vec::<AutoField>::new()),
            scope_override: None,
        }
    }

    fn named_auto(
        ty: &str,
        scope_override: Option<ComponentScope>,
        fields: Vec<AutoField>,
    ) -> Component {
        Component {
            crate_ident: "crate".to_string(),
            type_key: ty.to_string(),
            struct_name: "X".to_string(),
            inject: None,
            fields: ComponentFieldsRaw::Named(Vec::new()),
            auto_fields: Some(fields),
            scope_override,
        }
    }

    #[test]
    fn minimal_generated_code_是可解析的rust() {
        let code = minimal_generated_code();
        syn::parse_file(&code).unwrap();
        assert!(code.contains("pub struct Container"));
    }

    #[test]
    fn build_all_expr_覆盖init与runtime并跳过未知实现() {
        let impls = vec!["crate::A".to_string(), "crate::Missing".to_string()];
        let mut name_by_type = BTreeMap::<String, String>::new();
        name_by_type.insert("crate::A".to_string(), "a".to_string());

        let list = build_list_all_expr(
            &impls,
            "dyn crate::T",
            &name_by_type,
            AccessMode::Init,
            SharedKind::Arc,
        )
        .unwrap();
        assert!(list.contains("a.clone()"));
        assert!(!list.contains("Missing"));

        let list = build_list_all_expr(
            &impls,
            "dyn crate::T",
            &name_by_type,
            AccessMode::Runtime,
            SharedKind::Arc,
        )
        .unwrap();
        assert!(list.contains("self.a.clone()"));

        let map = build_map_all_expr(
            &impls,
            "dyn crate::T",
            &name_by_type,
            AccessMode::Init,
            SharedKind::Rc,
        )
        .unwrap();
        assert!(map.contains("dris_rt::Type::of::<crate::A>()"));
        assert!(map.contains("a.clone()"));
    }

    #[test]
    fn generate_container_code_覆盖单例字段_根getter_命名冲突() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            singleton_inject("crate::A", Vec::new()),
        );
        components.insert(
            "crate::B".to_string(),
            prototype_inject(
                "crate::B",
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Arc,
                    dep_type: "crate::A".to_string(),
                }],
            ),
        );

        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::A".to_string(), Some(SharedKind::Arc));
        holder.insert("crate::B".to_string(), None);

        let code = generate_container_code(
            &components,
            &BTreeMap::new(),
            &vec!["crate::A".to_string(), "crate::B".to_string()],
            &vec!["crate::B".to_string()],
            &holder,
        )
        .unwrap();
        syn::parse_file(&code).unwrap();
        assert!(code.contains("a: std::sync::Arc<crate::A>"));
        assert!(code.contains("pub fn b(&self) -> crate::B"));

        // 单例根组件：Arc 形式 getter。
        let mut components2 = BTreeMap::<String, Component>::new();
        components2.insert(
            "crate::A".to_string(),
            singleton_inject("crate::A", Vec::new()),
        );
        let mut holder2 = BTreeMap::<String, Option<SharedKind>>::new();
        holder2.insert("crate::A".to_string(), Some(SharedKind::Arc));
        let code = generate_container_code(
            &components2,
            &BTreeMap::new(),
            &vec!["crate::A".to_string()],
            &vec!["crate::A".to_string()],
            &holder2,
        )
        .unwrap();
        assert!(code.contains("pub fn a(&self) -> std::sync::Arc<crate::A>"));
        assert!(code.contains("self.a.clone()"));

        // 单例根组件：按值存储 -> &T getter。
        let mut holder3 = BTreeMap::<String, Option<SharedKind>>::new();
        holder3.insert("crate::A".to_string(), None);
        let code = generate_container_code(
            &components2,
            &BTreeMap::new(),
            &vec!["crate::A".to_string()],
            &vec!["crate::A".to_string()],
            &holder3,
        )
        .unwrap();
        assert!(code.contains("pub fn a(&self) -> &crate::A"));
        assert!(code.contains("&self.a"));

        // snake 命名冲突：FooBar vs Foo_Bar
        let mut components3 = BTreeMap::<String, Component>::new();
        components3.insert(
            "crate::FooBar".to_string(),
            prototype_inject("crate::FooBar", Vec::new()),
        );
        components3.insert(
            "crate::Foo_Bar".to_string(),
            prototype_inject("crate::Foo_Bar", Vec::new()),
        );
        let holder = BTreeMap::<String, Option<SharedKind>>::new();
        let code = generate_container_code(
            &components3,
            &BTreeMap::new(),
            &vec!["crate::FooBar".to_string(), "crate::Foo_Bar".to_string()],
            &vec!["crate::FooBar".to_string(), "crate::Foo_Bar".to_string()],
            &holder,
        )
        .unwrap();
        assert!(code.contains("pub fn foo_bar(&self) -> crate::FooBar"));
        assert!(code.contains("pub fn foo_bar_2(&self) -> crate::Foo_Bar"));
    }

    #[test]
    fn generate_container_code_三连命名冲突会递增到_3() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::FooBar".to_string(),
            prototype_inject("crate::FooBar", Vec::new()),
        );
        components.insert(
            "crate::Foo_Bar".to_string(),
            prototype_inject("crate::Foo_Bar", Vec::new()),
        );
        components.insert(
            "crate::FooBAR".to_string(),
            prototype_inject("crate::FooBAR", Vec::new()),
        );

        let code = generate_container_code(
            &components,
            &BTreeMap::new(),
            &vec![
                "crate::FooBar".to_string(),
                "crate::Foo_Bar".to_string(),
                "crate::FooBAR".to_string(),
            ],
            &vec![
                "crate::FooBar".to_string(),
                "crate::Foo_Bar".to_string(),
                "crate::FooBAR".to_string(),
            ],
            &BTreeMap::new(),
        )
        .unwrap();
        syn::parse_file(&code).unwrap();
        assert!(code.contains("pub fn foo_bar_3(&self) -> crate::FooBAR"));
    }

    #[test]
    fn generate_container_code_snake为空时使用hash字段名() {
        let ty = "&crate::A";
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(ty.to_string(), prototype_unit_auto(ty));
        let expected = format!("c{}", stable_hash_u64(ty));

        let code = generate_container_code(
            &components,
            &BTreeMap::new(),
            &vec![ty.to_string()],
            &vec![ty.to_string()],
            &BTreeMap::new(),
        )
        .unwrap();
        syn::parse_file(&code).unwrap();
        assert!(code.contains(&format!("pub fn {expected}(&self) -> {ty}")));
    }

    #[test]
    fn generate_container_code_字段名生成失败会返回错误() {
        let err = generate_container_code(
            &BTreeMap::new(),
            &BTreeMap::new(),
            &vec!["crate::".to_string()],
            &vec!["crate::".to_string()],
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("无法解析类型用于生成字段名"));
    }

    #[test]
    fn generate_container_code_会传播单例init构造错误() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::A".to_string(),
                struct_name: "A".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: None,
                scope_override: Some(ComponentScope::Singleton),
            },
        );
        let err = generate_container_code(
            &components,
            &BTreeMap::new(),
            &vec!["crate::A".to_string()],
            &Vec::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少 #[constructor]"));
    }

    #[test]
    fn generate_container_code_会传播原型根构造错误() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::A".to_string(),
                struct_name: "A".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: None,
                scope_override: None,
            },
        );
        let err = generate_container_code(
            &components,
            &BTreeMap::new(),
            &vec!["crate::A".to_string()],
            &vec!["crate::A".to_string()],
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少 #[constructor]"));
    }

    #[test]
    fn build_component_expr_覆盖部分错误与多行表达式() {
        // holder 不匹配：要求 Arc<Dep>，但 holder map 里是 Rc。
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            singleton_inject("crate::Dep", Vec::new()),
        );
        components.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Arc,
                    dep_type: "crate::Dep".to_string(),
                }],
            ),
        );
        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::Dep".to_string(), ComponentScope::Singleton);
        scope.insert("crate::A".to_string(), ComponentScope::Prototype);
        let mut name = BTreeMap::<String, String>::new();
        name.insert("crate::Dep".to_string(), "dep".to_string());
        name.insert("crate::A".to_string(), "a".to_string());
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Dep".to_string(), Some(SharedKind::Rc));
        holder.insert("crate::A".to_string(), None);
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &name,
            &holder,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("持有方式不匹配"));

        // trait 注入：无实现时直接报错。
        let mut components2 = BTreeMap::<String, Component>::new();
        components2.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![InjectParam::SingleTraitRef {
                    kind: SharedKind::Arc,
                    trait_primary: "crate::T".to_string(),
                    trait_object: "dyn crate::T".to_string(),
                }],
            ),
        );
        let mut scope2 = BTreeMap::<String, ComponentScope>::new();
        scope2.insert("crate::A".to_string(), ComponentScope::Prototype);
        let mut name2 = BTreeMap::<String, String>::new();
        name2.insert("crate::A".to_string(), "a".to_string());
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components2,
            &BTreeMap::new(),
            &scope2,
            &name2,
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("未找到任何实现"));

        // 单例依赖 prototype 的 &T：会生成多行 block 表达式。
        let mut components3 = BTreeMap::<String, Component>::new();
        components3.insert("crate::B".to_string(), prototype_unit_auto("crate::B"));
        components3.insert(
            "crate::A".to_string(),
            singleton_inject(
                "crate::A",
                vec![InjectParam::SingleBorrow {
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        let trait_impls = BTreeMap::<String, Vec<String>>::new();
        let order = vec!["crate::B".to_string(), "crate::A".to_string()];
        let roots = vec!["crate::A".to_string()];
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::A".to_string(), None);
        holder.insert("crate::B".to_string(), None);
        let code =
            generate_container_code(&components3, &trait_impls, &order, &roots, &holder).unwrap();
        assert!(code.contains("let dep0 = crate::B;"));
        assert!(code.contains("let dep0_ref = &dep0;"));
    }

    #[test]
    fn build_component_expr_缺少组件与缺少构造信息会报错() {
        let err = build_component_expr(
            "crate::Missing",
            AccessMode::Runtime,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少组件"));

        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::A".to_string(),
                struct_name: "A".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: None,
                scope_override: None,
            },
        );
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少 #[constructor]"));
    }

    #[test]
    fn build_component_expr_inject_single_ref_覆盖init与缺失字段名错误() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            singleton_inject("crate::Dep", Vec::new()),
        );
        components.insert(
            "crate::A".to_string(),
            singleton_inject(
                "crate::A",
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Arc,
                    dep_type: "crate::Dep".to_string(),
                }],
            ),
        );

        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::Dep".to_string(), ComponentScope::Singleton);
        scope.insert("crate::A".to_string(), ComponentScope::Singleton);

        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Dep".to_string(), Some(SharedKind::Arc));
        holder.insert("crate::A".to_string(), None);

        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Dep".to_string(), "dep".to_string());
        names.insert("crate::A".to_string(), "a".to_string());

        let expr = build_component_expr(
            "crate::A",
            AccessMode::Init,
            &components,
            &BTreeMap::new(),
            &scope,
            &names,
            &holder,
        )
        .unwrap();
        assert!(expr.contains("crate::A::new(dep.clone())"));

        let mut names_missing = BTreeMap::<String, String>::new();
        names_missing.insert("crate::A".to_string(), "a".to_string());
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &names_missing,
            &holder,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少依赖字段名"));
    }

    #[test]
    fn build_component_expr_inject_single_trait_ref_覆盖成功_多实现_持有不匹配_缺失字段名() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![InjectParam::SingleTraitRef {
                    kind: SharedKind::Arc,
                    trait_primary: "crate::T".to_string(),
                    trait_object: "dyn crate::T".to_string(),
                }],
            ),
        );

        // 多实现。
        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert(
            "crate::T".to_string(),
            vec!["crate::I1".to_string(), "crate::I2".to_string()],
        );
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("找到多个实现"));

        // 单实现但持有方式不匹配。
        trait_impls.insert("crate::T".to_string(), vec!["crate::Impl".to_string()]);
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Impl".to_string(), Some(SharedKind::Rc));
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Impl".to_string(), "imp".to_string());
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls,
            &BTreeMap::new(),
            &names,
            &holder,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("持有方式不匹配"));

        // 单实现且持有匹配，但缺少字段名。
        holder.insert("crate::Impl".to_string(), Some(SharedKind::Arc));
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &holder,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少依赖字段名"));

        // 成功：runtime 使用 self.xxx，init 使用 xxx。
        let expr = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls,
            &BTreeMap::new(),
            &names,
            &holder,
        )
        .unwrap();
        assert!(expr.contains("crate::A::new(self.imp.clone())"));

        let expr = build_component_expr(
            "crate::A",
            AccessMode::Init,
            &components,
            &trait_impls,
            &BTreeMap::new(),
            &names,
            &holder,
        )
        .unwrap();
        assert!(expr.contains("crate::A::new(imp.clone())"));
    }

    #[test]
    fn build_component_expr_inject_single_borrow_覆盖单例按值与共享两种() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            singleton_inject("crate::Dep", Vec::new()),
        );
        components.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![InjectParam::SingleBorrow {
                    dep_type: "crate::Dep".to_string(),
                }],
            ),
        );
        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::Dep".to_string(), ComponentScope::Singleton);
        scope.insert("crate::A".to_string(), ComponentScope::Prototype);
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Dep".to_string(), "dep".to_string());

        // 按值单例：&self.dep
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Dep".to_string(), None);
        let expr = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &names,
            &holder,
        )
        .unwrap();
        assert!(expr.contains("crate::A::new(&self.dep)"));

        // 共享单例：self.dep.as_ref()
        holder.insert("crate::Dep".to_string(), Some(SharedKind::Arc));
        let expr = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &names,
            &holder,
        )
        .unwrap();
        assert!(expr.contains("crate::A::new(self.dep.as_ref())"));
    }

    #[test]
    fn build_component_expr_inject_single_borrow_覆盖init分支与递归失败传播() {
        // 单例依赖，init 分支会走 dep 变量，而不是 self.dep。
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            singleton_inject("crate::Dep", Vec::new()),
        );
        components.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![InjectParam::SingleBorrow {
                    dep_type: "crate::Dep".to_string(),
                }],
            ),
        );
        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::Dep".to_string(), ComponentScope::Singleton);
        scope.insert("crate::A".to_string(), ComponentScope::Prototype);
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Dep".to_string(), "dep".to_string());
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Dep".to_string(), None);
        let expr = build_component_expr(
            "crate::A",
            AccessMode::Init,
            &components,
            &BTreeMap::new(),
            &scope,
            &names,
            &holder,
        )
        .unwrap();
        assert!(expr.contains("crate::A::new(&dep)"));

        // prototype 依赖：递归构造失败应向上返回。
        components.insert(
            "crate::Bad".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::Bad".to_string(),
                struct_name: "Bad".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: None,
                scope_override: None,
            },
        );
        components.insert(
            "crate::B".to_string(),
            prototype_inject(
                "crate::B",
                vec![InjectParam::SingleBorrow {
                    dep_type: "crate::Bad".to_string(),
                }],
            ),
        );
        scope.insert("crate::Bad".to_string(), ComponentScope::Prototype);
        scope.insert("crate::B".to_string(), ComponentScope::Prototype);
        names.insert("crate::Bad".to_string(), "bad".to_string());
        let err = build_component_expr(
            "crate::B",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &names,
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少 #[constructor]"));
    }

    #[test]
    fn build_component_expr_inject_single_owned_会生成block并传值() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert("crate::Dep".to_string(), prototype_unit_auto("crate::Dep"));
        components.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::Dep".to_string(),
                }],
            ),
        );
        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::Dep".to_string(), ComponentScope::Prototype);
        scope.insert("crate::A".to_string(), ComponentScope::Prototype);
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Dep".to_string(), "dep".to_string());
        let expr = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &names,
            &BTreeMap::new(),
        )
        .unwrap();
        assert!(expr.contains("let dep0 = crate::Dep;"));
        assert!(!expr.contains("dep0_ref"));
        assert!(expr.contains("crate::A::new(dep0)"));
    }

    #[test]
    fn build_component_expr_inject_single_owned_递归失败会向上返回() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Bad".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::Bad".to_string(),
                struct_name: "Bad".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: None,
                scope_override: None,
            },
        );
        components.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::Bad".to_string(),
                }],
            ),
        );
        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::Bad".to_string(), ComponentScope::Prototype);
        scope.insert("crate::A".to_string(), ComponentScope::Prototype);
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Bad".to_string(), "bad".to_string());
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &names,
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少 #[constructor]"));
    }

    #[test]
    fn build_component_expr_inject_all_list_all_map_覆盖逗号与map_runtime分支() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            prototype_inject(
                "crate::A",
                vec![
                    InjectParam::AllList {
                        kind: SharedKind::Arc,
                        trait_primary: "crate::T".to_string(),
                        trait_object: "dyn crate::T".to_string(),
                    },
                    InjectParam::AllMap {
                        kind: SharedKind::Arc,
                        trait_primary: "crate::T".to_string(),
                        trait_object: "dyn crate::T".to_string(),
                    },
                ],
            ),
        );

        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert(
            "crate::T".to_string(),
            vec!["crate::I1".to_string(), "crate::I2".to_string()],
        );
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::I1".to_string(), "i1".to_string());
        names.insert("crate::I2".to_string(), "i2".to_string());
        let expr = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls,
            &BTreeMap::new(),
            &names,
            &BTreeMap::new(),
        )
        .unwrap();
        assert!(expr.contains("self.i1.clone()"));
        assert!(expr.contains("self.i2.clone()"));
        assert!(expr.contains(", self.i2.clone()"));
        assert!(expr.contains("Type::of::<crate::I1>()"));
        assert!(expr.contains("Type::of::<crate::I2>()"));
        assert!(expr.contains("crate::A::new(dep0, dep1)"));
    }

    #[test]
    fn build_component_expr_auto_fields_覆盖unit_singleton_tuple_named与不支持字段注入() {
        // unit + singleton（覆盖 ComponentScope::Singleton 分支）。
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::U".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::U".to_string(),
                struct_name: "U".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: Some(Vec::<AutoField>::new()),
                scope_override: Some(ComponentScope::Singleton),
            },
        );
        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::U".to_string(), ComponentScope::Singleton);
        let expr = build_component_expr(
            "crate::U",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap();
        assert!(expr.contains("crate::U"));

        // tuple struct 不能自动构造。
        components.insert(
            "crate::Tup".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::Tup".to_string(),
                struct_name: "Tup".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Tuple,
                auto_fields: Some(Vec::<AutoField>::new()),
                scope_override: None,
            },
        );
        let err = build_component_expr(
            "crate::Tup",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("tuple struct"));

        // named struct + 不支持字段注入类型：SingleBorrow
        components.insert(
            "crate::A".to_string(),
            named_auto(
                "crate::A",
                None,
                vec![AutoField {
                    name: "x".to_string(),
                    param: InjectParam::SingleBorrow {
                        dep_type: "crate::Dep".to_string(),
                    },
                }],
            ),
        );
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("自动构造不支持"));
    }

    #[test]
    fn build_component_expr_auto_fields_named_覆盖多种字段与错误分支() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            singleton_inject("crate::Dep", Vec::new()),
        );
        components.insert(
            "crate::Owned".to_string(),
            prototype_unit_auto("crate::Owned"),
        );
        components.insert(
            "crate::A".to_string(),
            named_auto(
                "crate::A",
                None,
                vec![
                    AutoField {
                        name: "f0".to_string(),
                        param: InjectParam::SingleRef {
                            kind: SharedKind::Arc,
                            dep_type: "crate::Dep".to_string(),
                        },
                    },
                    AutoField {
                        name: "f1".to_string(),
                        param: InjectParam::SingleTraitRef {
                            kind: SharedKind::Arc,
                            trait_primary: "crate::T".to_string(),
                            trait_object: "dyn crate::T".to_string(),
                        },
                    },
                    AutoField {
                        name: "f2".to_string(),
                        param: InjectParam::SingleOwned {
                            dep_type: "crate::Owned".to_string(),
                        },
                    },
                    AutoField {
                        name: "f3".to_string(),
                        param: InjectParam::AllList {
                            kind: SharedKind::Arc,
                            trait_primary: "crate::T".to_string(),
                            trait_object: "dyn crate::T".to_string(),
                        },
                    },
                    AutoField {
                        name: "f4".to_string(),
                        param: InjectParam::AllMap {
                            kind: SharedKind::Arc,
                            trait_primary: "crate::T".to_string(),
                            trait_object: "dyn crate::T".to_string(),
                        },
                    },
                ],
            ),
        );

        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Dep".to_string(), "dep".to_string());
        names.insert("crate::Impl".to_string(), "imp".to_string());
        names.insert("crate::I1".to_string(), "i1".to_string());
        names.insert("crate::I2".to_string(), "i2".to_string());
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Dep".to_string(), Some(SharedKind::Arc));
        holder.insert("crate::Impl".to_string(), Some(SharedKind::Arc));
        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert(
            "crate::T".to_string(),
            vec![
                "crate::Impl".to_string(),
                "crate::I1".to_string(),
                "crate::I2".to_string(),
            ],
        );

        // 先覆盖：trait ref 多实现报错（len != 1）。
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls,
            &BTreeMap::new(),
            &names,
            &holder,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("找到多个实现"));

        // 成功路径：将 SingleTraitRef 的实现列表收窄到一个。
        let trait_impls_for_single =
            BTreeMap::from([("crate::T".to_string(), vec!["crate::Impl".to_string()])]);
        let expr = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls_for_single,
            &BTreeMap::new(),
            &names,
            &holder,
        )
        .unwrap();
        assert!(expr.contains("crate::A"));
        assert!(expr.contains("f0: self.dep.clone()"));
        assert!(expr.contains("f1: self.imp.clone()"));
        assert!(expr.contains("f2: crate::Owned"));
    }

    #[test]
    fn build_component_expr_auto_fields_single_ref_覆盖不匹配与init分支() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            singleton_inject("crate::Dep", Vec::new()),
        );
        components.insert(
            "crate::A".to_string(),
            named_auto(
                "crate::A",
                None,
                vec![AutoField {
                    name: "f0".to_string(),
                    param: InjectParam::SingleRef {
                        kind: SharedKind::Arc,
                        dep_type: "crate::Dep".to_string(),
                    },
                }],
            ),
        );

        // 持有方式不匹配。
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Dep".to_string(), Some(SharedKind::Rc));
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &holder,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("自动构造字段 f0"));
        assert!(err.contains("持有方式不匹配"));

        // init 分支：使用 dep.clone()。
        holder.insert("crate::Dep".to_string(), Some(SharedKind::Arc));
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Dep".to_string(), "dep".to_string());
        let out = build_component_expr(
            "crate::A",
            AccessMode::Init,
            &components,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &names,
            &holder,
        )
        .unwrap();
        assert!(out.contains("f0: dep.clone()"));
    }

    #[test]
    fn build_component_expr_auto_fields_single_trait_ref_覆盖无实现_不匹配_init分支() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            named_auto(
                "crate::A",
                None,
                vec![AutoField {
                    name: "f0".to_string(),
                    param: InjectParam::SingleTraitRef {
                        kind: SharedKind::Arc,
                        trait_primary: "crate::T".to_string(),
                        trait_object: "dyn crate::T".to_string(),
                    },
                }],
            ),
        );

        // 无实现。
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("未找到任何实现"));

        // 持有方式不匹配。
        let trait_impls_one =
            BTreeMap::from([("crate::T".to_string(), vec!["crate::Impl".to_string()])]);
        let mut holder = BTreeMap::<String, Option<SharedKind>>::new();
        holder.insert("crate::Impl".to_string(), Some(SharedKind::Rc));
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &trait_impls_one,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &holder,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("持有方式不匹配"));

        // init 分支：使用 imp.clone()。
        holder.insert("crate::Impl".to_string(), Some(SharedKind::Arc));
        let mut names = BTreeMap::<String, String>::new();
        names.insert("crate::Impl".to_string(), "imp".to_string());
        let out = build_component_expr(
            "crate::A",
            AccessMode::Init,
            &components,
            &trait_impls_one,
            &BTreeMap::new(),
            &names,
            &holder,
        )
        .unwrap();
        assert!(out.contains("f0: imp.clone()"));
    }

    #[test]
    fn build_component_expr_auto_fields_single_owned_递归失败与named_singleton分支() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Bad".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::Bad".to_string(),
                struct_name: "Bad".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: None,
                scope_override: None,
            },
        );
        components.insert(
            "crate::A".to_string(),
            named_auto(
                "crate::A",
                None,
                vec![AutoField {
                    name: "f0".to_string(),
                    param: InjectParam::SingleOwned {
                        dep_type: "crate::Bad".to_string(),
                    },
                }],
            ),
        );
        let err = build_component_expr(
            "crate::A",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("缺少 #[constructor]"));

        // named struct 的 singleton 分支。
        components.insert(
            "crate::S".to_string(),
            named_auto("crate::S", Some(ComponentScope::Singleton), Vec::new()),
        );
        let mut scope = BTreeMap::<String, ComponentScope>::new();
        scope.insert("crate::S".to_string(), ComponentScope::Singleton);
        let out = build_component_expr(
            "crate::S",
            AccessMode::Runtime,
            &components,
            &BTreeMap::new(),
            &scope,
            &BTreeMap::new(),
            &BTreeMap::new(),
        )
        .unwrap();
        assert!(out.contains("crate::S"));
    }
}
