use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Result, anyhow};

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

struct RustWriter {
    buf: String,
    indent: usize,
}

impl RustWriter {
    fn new() -> Self {
        Self {
            buf: String::new(),
            indent: 0,
        }
    }

    fn finish(self) -> String {
        self.buf
    }

    fn blank_line(&mut self) {
        self.buf.push('\n');
    }

    fn line(&mut self, s: impl AsRef<str>) {
        for _ in 0..self.indent {
            self.buf.push_str("    ");
        }
        self.buf.push_str(s.as_ref());
        self.buf.push('\n');
    }

    fn lines(&mut self, s: &str) {
        for line in s.lines() {
            self.line(line);
        }
    }

    fn block<F>(&mut self, header: impl AsRef<str>, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.line(format!("{} {{", header.as_ref()));
        self.indent += 1;
        f(self);
        self.indent -= 1;
        self.line("}");
    }

    fn write_let_assign(&mut self, lhs: &str, expr: &str) {
        let mut it = expr.lines();
        let Some(first) = it.next() else {
            self.line(format!("{lhs} = ();"));
            return;
        };
        let rest: Vec<&str> = it.collect();
        if rest.is_empty() {
            self.line(format!("{lhs} = {first};"));
            return;
        }

        self.line(format!("{lhs} = {first}"));
        for mid in &rest[..rest.len() - 1] {
            self.line(*mid);
        }
        self.line(format!("{};", rest[rest.len() - 1]));
    }
}

#[derive(Debug, Clone, Copy)]
enum AccessMode {
    Init,
    Runtime,
}

pub(crate) fn minimal_generated_code() -> String {
    let mut w = RustWriter::new();
    w.line("#[allow(dead_code)]");
    w.block("pub mod dris_gen", |w| {
        w.line("pub struct Container;");
        w.block("impl Container", |w| {
            w.block("pub fn build() -> Self", |w| {
                w.line("Self");
            });
        });
    });
    w.finish()
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

    let mut w = RustWriter::new();
    w.line("#[allow(dead_code)]");
    w.block("pub mod dris_gen", |w| {
        w.block("pub struct Container", |w| {
            for f in &singleton_fields {
                w.line(format!("{}: {},", f.name, holder_type(f.holder, &f.ty)));
            }
        });
        w.blank_line();

        w.block("impl Container", |w| {
            w.block("pub fn build() -> Self", |w| {
                for f in &singleton_fields {
                    w.write_let_assign(
                        &format!("let {}: {}", f.name, holder_type(f.holder, &f.ty)),
                        &f.init_expr,
                    );
                }
                w.block("Self", |w| {
                    for f in &singleton_fields {
                        w.line(format!("{},", f.name));
                    }
                });
            });
            w.blank_line();

            for m in &root_methods {
                match m.scope {
                    ComponentScope::Singleton => match m.holder {
                        Some(kind) => {
                            w.block(
                                format!(
                                    "pub fn {}(&self) -> {}",
                                    m.method_name,
                                    shared_type(kind, &m.ty)
                                ),
                                |w| {
                                    w.line(format!("self.{}.clone()", m.method_name));
                                },
                            );
                        }
                        None => {
                            w.block(
                                format!("pub fn {}(&self) -> &{}", m.method_name, m.ty),
                                |w| {
                                    w.line(format!("&self.{}", m.method_name));
                                },
                            );
                        }
                    },
                    ComponentScope::Prototype => {
                        let expr = m.expr.as_deref().unwrap_or_default();
                        w.block(
                            format!("pub fn {}(&self) -> {}", m.method_name, m.ty),
                            |w| {
                                w.lines(expr);
                            },
                        );
                    }
                }
                w.blank_line();
            }
        });
    });
    Ok(w.finish())
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
