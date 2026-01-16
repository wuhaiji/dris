use std::collections::BTreeMap;

use anyhow::{Result, anyhow};

use crate::model::{Component, ComponentScope, InjectParam, SharedKind};

fn shared_kind_name(kind: SharedKind) -> &'static str {
    match kind {
        SharedKind::Arc => "Arc",
        SharedKind::Rc => "Rc",
    }
}

pub(crate) fn validate_component_scopes(
    components: &BTreeMap<String, Component>,
    trait_impls: &BTreeMap<String, Vec<String>>,
) -> Result<()> {
    let mut scope_by_type = BTreeMap::<String, ComponentScope>::new();
    for (ty, c) in components {
        scope_by_type.insert(ty.clone(), c.scope());
    }

    for (ty, c) in components {
        let scope = scope_by_type
            .get(ty)
            .copied()
            .unwrap_or(ComponentScope::Prototype);

        if scope == ComponentScope::Prototype {
            if let Some(inject) = c.inject.as_ref() {
                if inject.return_is_ref {
                    return Err(anyhow!(
                        "组件 {} 声明为 Prototype，但 #[constructor] 返回了 Arc<_>/Rc<_>：请将返回类型改为 Self，或把组件声明为 Singleton（例如 #[component(singleton)]）",
                        ty
                    ));
                }
            }
        }

        let mut params: Vec<&InjectParam> = Vec::new();
        if let Some(inject) = c.inject.as_ref() {
            params.extend(inject.params.iter());
        } else {
            let Some(fields) = c.auto_fields.as_ref() else {
                return Err(anyhow!("组件缺少 #[constructor] 且无法自动构造：{ty}"));
            };
            params.extend(fields.iter().map(|f| &f.param));
        }

        for p in params {
            match p {
                InjectParam::SingleRef { kind, dep_type } => {
                    let dep_scope = scope_by_type.get(dep_type).copied().ok_or_else(|| {
                        anyhow!("组件 {} 依赖 {}，但未找到对应组件", ty, dep_type)
                    })?;
                    if dep_scope != ComponentScope::Singleton {
                        return Err(anyhow!(
                            "组件 {} 以 {} 注入 {}，但 {} 不是单例：请把 {} 声明为 Singleton（例如 #[component(singleton)]），或让其 #[constructor] 返回 {}<_>",
                            ty,
                            shared_kind_name(*kind),
                            dep_type,
                            dep_type,
                            dep_type,
                            shared_kind_name(*kind),
                        ));
                    }
                }
                InjectParam::SingleTraitRef {
                    kind,
                    trait_primary,
                    ..
                } => {
                    let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                    if impls.is_empty() {
                        return Err(anyhow!(
                            "组件 {} 以 {}<dyn Trait> 注入 {}，但未找到任何实现",
                            ty,
                            shared_kind_name(*kind),
                            trait_primary
                        ));
                    }
                    if impls.len() != 1 {
                        return Err(anyhow!(
                            "组件 {} 以 {}<dyn Trait> 注入 {}，但找到多个实现（{}）：请改用 dris_rt::All<{}<dyn Trait>> 或提供筛选机制（暂不支持）",
                            ty,
                            shared_kind_name(*kind),
                            trait_primary,
                            impls.join(", "),
                            shared_kind_name(*kind),
                        ));
                    }
                    let impl_ty = &impls[0];
                    let dep_scope = scope_by_type
                        .get(impl_ty)
                        .copied()
                        .ok_or_else(|| anyhow!("组件 {} 依赖 {}，但未找到对应组件", ty, impl_ty))?;
                    if dep_scope != ComponentScope::Singleton {
                        return Err(anyhow!(
                            "组件 {} 以 {}<dyn Trait> 注入 {} 的实现 {}，但 {} 不是单例：请把 {} 声明为 Singleton（例如 #[component(singleton)]），或让其 #[constructor] 返回 {}<_>",
                            ty,
                            shared_kind_name(*kind),
                            trait_primary,
                            impl_ty,
                            impl_ty,
                            impl_ty,
                            shared_kind_name(*kind),
                        ));
                    }
                }
                InjectParam::SingleOwned { dep_type } => {
                    let dep_scope = scope_by_type.get(dep_type).copied().ok_or_else(|| {
                        anyhow!("组件 {} 依赖 {}，但未找到对应组件", ty, dep_type)
                    })?;
                    if dep_scope != ComponentScope::Prototype {
                        return Err(anyhow!(
                            "组件 {} 按值注入 {}，但 {} 是单例：请改为 Arc<{}>/Rc<{}> 或 &{}",
                            ty,
                            dep_type,
                            dep_type,
                            dep_type,
                            dep_type,
                            dep_type
                        ));
                    }
                }
                InjectParam::SingleBorrow { .. } => {}
                InjectParam::AllList {
                    kind,
                    trait_primary,
                    ..
                }
                | InjectParam::AllMap {
                    kind,
                    trait_primary,
                    ..
                } => {
                    let impls = trait_impls.get(trait_primary).cloned().unwrap_or_default();
                    for impl_ty in impls {
                        let Some(dep_scope) = scope_by_type.get(&impl_ty).copied() else {
                            continue;
                        };
                        if dep_scope != ComponentScope::Singleton {
                            return Err(anyhow!(
                                "组件 {} 需要 {} 的实现以 {}<dyn Trait> 注入，但实现 {} 不是单例：请把 {} 声明为 Singleton（例如 #[component(singleton)]），或让其 #[constructor] 返回 {}<_>",
                                ty,
                                trait_primary,
                                shared_kind_name(*kind),
                                impl_ty,
                                impl_ty,
                                shared_kind_name(*kind),
                            ));
                        }
                    }
                }
            }
        }
    }

    Ok(())
}
