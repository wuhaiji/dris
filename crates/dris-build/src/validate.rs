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
                            "组件 {} 以 {} 注入 {}，但 {} 不是单例：{}<T> 仅支持 Singleton 注入；若该依赖应为 Prototype，请改为按值注入 {}；否则请把 {} 声明为 Singleton（例如 #[component(singleton)]）",
                            ty,
                            shared_kind_name(*kind),
                            dep_type,
                            dep_type,
                            shared_kind_name(*kind),
                            dep_type,
                            dep_type,
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
                            "组件 {} 以 {}<dyn Trait> 注入 {} 的实现 {}，但 {} 不是单例：{}<dyn Trait> 仅支持 Singleton 实现；若该实现应为 Prototype，请改为按值注入具体类型；否则请把 {} 声明为 Singleton（例如 #[component(singleton)]）",
                            ty,
                            shared_kind_name(*kind),
                            trait_primary,
                            impl_ty,
                            impl_ty,
                            shared_kind_name(*kind),
                            impl_ty,
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
                InjectParam::SingleBorrow { dep_type } => {
                    let dep_scope = scope_by_type.get(dep_type).copied().ok_or_else(|| {
                        anyhow!("组件 {} 依赖 {}，但未找到对应组件", ty, dep_type)
                    })?;
                    if dep_scope != ComponentScope::Singleton {
                        return Err(anyhow!(
                            "组件 {} 以 &T 注入 {}，但 {} 是 Prototype：Prototype 组件不支持 &T/Rc<T>/Arc<T> 注入，请改为按值注入 {}",
                            ty,
                            dep_type,
                            dep_type,
                            dep_type
                        ));
                    }
                }
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
                                "组件 {} 需要 {} 的实现以 {}<dyn Trait> 注入，但实现 {} 不是单例：{}<dyn Trait> 仅支持 Singleton 实现；若该实现应为 Prototype，请改为按值注入具体类型；否则请把 {} 声明为 Singleton（例如 #[component(singleton)]）",
                                ty,
                                trait_primary,
                                shared_kind_name(*kind),
                                impl_ty,
                                shared_kind_name(*kind),
                                impl_ty,
                            ));
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{AutoField, ComponentFieldsRaw, InjectCtor};

    fn component_with_inject(
        ty: &str,
        scope_override: Option<ComponentScope>,
        params: Vec<InjectParam>,
    ) -> Component {
        Component {
            crate_ident: "crate".to_string(),
            type_key: ty.to_string(),
            struct_name: "X".to_string(),
            inject: Some(InjectCtor {
                call_path: "X::new".to_string(),
                params,
            }),
            fields: ComponentFieldsRaw::Unit,
            auto_fields: None,
            scope_override,
        }
    }

    fn component_with_auto_fields(
        ty: &str,
        scope_override: Option<ComponentScope>,
        params: Vec<InjectParam>,
    ) -> Component {
        Component {
            crate_ident: "crate".to_string(),
            type_key: ty.to_string(),
            struct_name: "X".to_string(),
            inject: None,
            fields: ComponentFieldsRaw::Unit,
            auto_fields: Some(
                params
                    .into_iter()
                    .enumerate()
                    .map(|(i, p)| AutoField {
                        name: format!("f{i}"),
                        param: p,
                    })
                    .collect(),
            ),
            scope_override,
        }
    }

    #[test]
    fn validate_component_scopes_组件缺少构造信息会报错() {
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
        let err = validate_component_scopes(&components, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("组件缺少 #[constructor] 且无法自动构造"));
    }

    #[test]
    fn validate_component_scopes_single_ref_覆盖缺失与非单例() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Arc,
                    dep_type: "crate::Missing".to_string(),
                }],
            ),
        );
        let err = validate_component_scopes(&components, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("未找到对应组件"));

        components.insert(
            "crate::B".to_string(),
            component_with_inject("crate::B", None, Vec::new()),
        );
        let mut components2 = components.clone();
        components2.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Arc,
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        let err = validate_component_scopes(&components2, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("不是单例"));

        let mut components3 = BTreeMap::<String, Component>::new();
        components3.insert(
            "crate::B".to_string(),
            component_with_inject("crate::B", Some(ComponentScope::Singleton), Vec::new()),
        );
        components3.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Arc,
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        validate_component_scopes(&components3, &BTreeMap::new()).unwrap();
    }

    #[test]
    fn validate_component_scopes_single_trait_ref_覆盖无实现_多实现_缺失组件_非单例() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleTraitRef {
                    kind: SharedKind::Arc,
                    trait_primary: "crate::MyTrait".to_string(),
                    trait_object: "dyn crate::MyTrait".to_string(),
                }],
            ),
        );

        let err = validate_component_scopes(&components, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("未找到任何实现"));

        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert(
            "crate::MyTrait".to_string(),
            vec!["crate::Impl1".to_string(), "crate::Impl2".to_string()],
        );
        let err = validate_component_scopes(&components, &trait_impls)
            .unwrap_err()
            .to_string();
        assert!(err.contains("找到多个实现"));

        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert(
            "crate::MyTrait".to_string(),
            vec!["crate::Impl1".to_string()],
        );
        let err = validate_component_scopes(&components, &trait_impls)
            .unwrap_err()
            .to_string();
        assert!(err.contains("未找到对应组件"));

        let mut components2 = components.clone();
        components2.insert(
            "crate::Impl1".to_string(),
            component_with_inject("crate::Impl1", None, Vec::new()),
        );
        let err = validate_component_scopes(&components2, &trait_impls)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不是单例"));

        let mut components3 = components.clone();
        components3.insert(
            "crate::Impl1".to_string(),
            component_with_inject("crate::Impl1", Some(ComponentScope::Singleton), Vec::new()),
        );
        validate_component_scopes(&components3, &trait_impls).unwrap();
    }

    #[test]
    fn validate_component_scopes_single_owned_覆盖缺失与禁止注入单例() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::Missing".to_string(),
                }],
            ),
        );
        let err = validate_component_scopes(&components, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("未找到对应组件"));

        components.insert(
            "crate::B".to_string(),
            component_with_inject("crate::B", Some(ComponentScope::Singleton), Vec::new()),
        );
        let mut components2 = components.clone();
        components2.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        let err = validate_component_scopes(&components2, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("是单例"));

        let mut components3 = BTreeMap::<String, Component>::new();
        components3.insert(
            "crate::B".to_string(),
            component_with_inject("crate::B", None, Vec::new()),
        );
        components3.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        validate_component_scopes(&components3, &BTreeMap::new()).unwrap();
    }

    #[test]
    fn validate_component_scopes_single_borrow_禁止借用prototype_允许借用单例() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::B".to_string(),
            component_with_inject("crate::B", None, Vec::new()),
        );
        components.insert(
            "crate::A".to_string(),
            component_with_inject(
                "crate::A",
                None,
                vec![InjectParam::SingleBorrow {
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        let err = validate_component_scopes(&components, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("Prototype"));
        assert!(err.contains("不支持"));

        components.insert(
            "crate::B".to_string(),
            component_with_inject("crate::B", Some(ComponentScope::Singleton), Vec::new()),
        );
        validate_component_scopes(&components, &BTreeMap::new()).unwrap();
    }

    #[test]
    fn validate_component_scopes_all_list_all_map_覆盖忽略缺失实现与非单例实现() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            component_with_auto_fields(
                "crate::A",
                None,
                vec![
                    InjectParam::AllList {
                        kind: SharedKind::Rc,
                        trait_primary: "crate::MyTrait".to_string(),
                        trait_object: "dyn crate::MyTrait".to_string(),
                    },
                    InjectParam::AllMap {
                        kind: SharedKind::Rc,
                        trait_primary: "crate::MyTrait".to_string(),
                        trait_object: "dyn crate::MyTrait".to_string(),
                    },
                    InjectParam::SingleBorrow {
                        dep_type: "crate::B".to_string(),
                    },
                ],
            ),
        );
        components.insert(
            "crate::B".to_string(),
            component_with_inject("crate::B", Some(ComponentScope::Singleton), Vec::new()),
        );

        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert(
            "crate::MyTrait".to_string(),
            vec!["crate::MissingImpl".to_string(), "crate::Impl1".to_string()],
        );
        components.insert(
            "crate::Impl1".to_string(),
            component_with_inject("crate::Impl1", None, Vec::new()),
        );

        let err = validate_component_scopes(&components, &trait_impls)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不是单例"));

        components.insert(
            "crate::Impl1".to_string(),
            component_with_inject("crate::Impl1", Some(ComponentScope::Singleton), Vec::new()),
        );
        validate_component_scopes(&components, &trait_impls).unwrap();
    }
}
