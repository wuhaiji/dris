use std::collections::BTreeMap;

use anyhow::{Context, Result, anyhow};

use crate::model::{
    AutoField, Component, ComponentFieldsRaw, InjectParam, InjectParamRaw, TypeRef,
};

pub(crate) fn build_component_name_index(
    components: &BTreeMap<String, Component>,
) -> BTreeMap<(String, String), Vec<String>> {
    let mut index = BTreeMap::<(String, String), Vec<String>>::new();
    for c in components.values() {
        index
            .entry((c.crate_ident.clone(), c.struct_name.clone()))
            .or_default()
            .push(c.type_key.clone());
    }
    for v in index.values_mut() {
        v.sort();
    }
    index
}

pub(crate) fn resolve_type_ref(
    r: &TypeRef,
    components: &BTreeMap<String, Component>,
    index: &BTreeMap<(String, String), Vec<String>>,
) -> Result<String> {
    if components.contains_key(&r.key) {
        return Ok(r.key.clone());
    }
    let Some(name) = r.simple_name.as_ref() else {
        return Err(anyhow!("无法解析类型：{}", r.key));
    };
    let Some(cands) = index.get(&(r.crate_ident.clone(), name.clone())) else {
        return Err(anyhow!("未找到组件类型：{}", r.key));
    };
    if cands.len() == 1 {
        return Ok(cands[0].clone());
    }
    Err(anyhow!(
        "类型 {} 存在多个候选实现（{}），请使用完整路径避免歧义",
        name,
        cands.join(", ")
    ))
}

pub(crate) fn resolve_inject_param_raw(
    raw: &InjectParamRaw,
    components: &BTreeMap<String, Component>,
    index: &BTreeMap<(String, String), Vec<String>>,
) -> Result<InjectParam> {
    match raw {
        InjectParamRaw::SingleRef { kind, dep } => {
            let dep_key = resolve_type_ref(dep, components, index)?;
            Ok(InjectParam::SingleRef {
                kind: *kind,
                dep_type: dep_key,
            })
        }
        InjectParamRaw::SingleTraitRef { kind, info } => Ok(InjectParam::SingleTraitRef {
            kind: *kind,
            trait_primary: info.primary_trait_key.clone(),
            trait_object: info.trait_object_key.clone(),
        }),
        InjectParamRaw::SingleBorrow(dep_ref) => {
            let dep_key = resolve_type_ref(dep_ref, components, index)?;
            Ok(InjectParam::SingleBorrow { dep_type: dep_key })
        }
        InjectParamRaw::SingleOwned(dep_ref) => {
            let dep_key = resolve_type_ref(dep_ref, components, index)?;
            Ok(InjectParam::SingleOwned { dep_type: dep_key })
        }
        InjectParamRaw::AllList { kind, info } => Ok(InjectParam::AllList {
            kind: *kind,
            trait_primary: info.primary_trait_key.clone(),
            trait_object: info.trait_object_key.clone(),
        }),
        InjectParamRaw::AllMap { kind, info } => Ok(InjectParam::AllMap {
            kind: *kind,
            trait_primary: info.primary_trait_key.clone(),
            trait_object: info.trait_object_key.clone(),
        }),
    }
}

pub(crate) fn build_auto_fields(
    _ty: &str,
    component: &Component,
    components: &BTreeMap<String, Component>,
    index: &BTreeMap<(String, String), Vec<String>>,
) -> Result<Vec<AutoField>> {
    match &component.fields {
        ComponentFieldsRaw::Unit => Ok(Vec::new()),
        ComponentFieldsRaw::Tuple => Err(anyhow!(
            "组件是 tuple struct，无法自动构造（请提供 #[constructor] 构造函数）"
        )),
        ComponentFieldsRaw::Named(fields) => {
            let mut out = Vec::new();
            for field in fields {
                if !field.is_pub {
                    return Err(anyhow!(
                        "字段 {} 不是 pub，无法自动构造（请提供 #[constructor] 构造函数）",
                        field.name
                    ));
                }
                let Some(raw) = field.inject.as_ref() else {
                    return Err(anyhow!(
                        "字段 {}: {} 暂不支持自动注入：{}（请提供 #[constructor] 构造函数）",
                        field.name,
                        field.ty,
                        field.inject_error.as_deref().unwrap_or("无法解析注入类型")
                    ));
                };
                let param = resolve_inject_param_raw(raw, components, index)
                    .with_context(|| format!("字段 {}: {}", field.name, field.ty))?;
                match param {
                    InjectParam::SingleBorrow { .. } => {
                        return Err(anyhow!(
                            "字段 {}: {} 暂不支持该注入方式的自动构造（请提供 #[constructor] 构造函数）",
                            field.name,
                            field.ty
                        ));
                    }
                    _ => {}
                }
                out.push(AutoField {
                    name: field.name.clone(),
                    param,
                });
            }
            Ok(out)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{
        ComponentFieldRaw, ComponentFieldsRaw, ComponentScope, InjectCtor, SharedKind,
    };

    fn component(ty: &str, crate_ident: &str, struct_name: &str) -> Component {
        Component {
            crate_ident: crate_ident.to_string(),
            type_key: ty.to_string(),
            struct_name: struct_name.to_string(),
            inject: Some(InjectCtor {
                call_path: "X::new".to_string(),
                params: Vec::new(),
            }),
            fields: ComponentFieldsRaw::Unit,
            auto_fields: None,
            scope_override: Some(ComponentScope::Prototype),
        }
    }

    #[test]
    fn build_component_name_index_会按type_key排序() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::B".to_string(),
            component("crate::B", "crate", "Same"),
        );
        components.insert(
            "crate::A".to_string(),
            component("crate::A", "crate", "Same"),
        );
        let idx = build_component_name_index(&components);
        let cands = idx
            .get(&(String::from("crate"), String::from("Same")))
            .unwrap();
        assert_eq!(cands, &vec!["crate::A".to_string(), "crate::B".to_string()]);
    }

    #[test]
    fn resolve_type_ref_覆盖直达_缺失simple_name_缺失候选_歧义_单候选() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert("crate::A".to_string(), component("crate::A", "crate", "A"));
        let idx = build_component_name_index(&components);

        let r = TypeRef {
            crate_ident: "crate".to_string(),
            key: "crate::A".to_string(),
            simple_name: Some("A".to_string()),
        };
        assert_eq!(resolve_type_ref(&r, &components, &idx).unwrap(), "crate::A");

        let r = TypeRef {
            crate_ident: "crate".to_string(),
            key: "weird::<T>".to_string(),
            simple_name: None,
        };
        let err = resolve_type_ref(&r, &components, &idx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("无法解析类型"));

        let r = TypeRef {
            crate_ident: "crate".to_string(),
            key: "crate::Missing".to_string(),
            simple_name: Some("Missing".to_string()),
        };
        let err = resolve_type_ref(&r, &components, &idx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("未找到组件类型"));

        let mut single = BTreeMap::<String, Component>::new();
        single.insert(
            "crate::ImplA".to_string(),
            component("crate::ImplA", "crate", "A"),
        );
        let idx_single = build_component_name_index(&single);
        let r = TypeRef {
            crate_ident: "crate".to_string(),
            key: "crate::A".to_string(),
            simple_name: Some("A".to_string()),
        };
        assert_eq!(
            resolve_type_ref(&r, &single, &idx_single).unwrap(),
            "crate::ImplA"
        );

        let mut components2 = BTreeMap::<String, Component>::new();
        components2.insert(
            "crate::ImplA1".to_string(),
            component("crate::ImplA1", "crate", "A"),
        );
        components2.insert(
            "crate::ImplA2".to_string(),
            component("crate::ImplA2", "crate", "A"),
        );
        let idx2 = build_component_name_index(&components2);
        let r = TypeRef {
            crate_ident: "crate".to_string(),
            key: "crate::A".to_string(),
            simple_name: Some("A".to_string()),
        };
        let err = resolve_type_ref(&r, &components2, &idx2)
            .unwrap_err()
            .to_string();
        assert!(err.contains("多个候选实现"));
    }

    #[test]
    fn resolve_inject_param_raw_覆盖全部形态() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            component("crate::Dep", "crate", "Dep"),
        );
        let idx = build_component_name_index(&components);

        let dep_ref = TypeRef {
            crate_ident: "crate".to_string(),
            key: "crate::Dep".to_string(),
            simple_name: Some("Dep".to_string()),
        };
        let p = resolve_inject_param_raw(
            &InjectParamRaw::SingleRef {
                kind: SharedKind::Arc,
                dep: dep_ref.clone(),
            },
            &components,
            &idx,
        )
        .unwrap();
        assert!(matches!(p, InjectParam::SingleRef { .. }));

        let p = resolve_inject_param_raw(
            &InjectParamRaw::SingleBorrow(dep_ref.clone()),
            &components,
            &idx,
        )
        .unwrap();
        assert!(matches!(p, InjectParam::SingleBorrow { .. }));

        let p = resolve_inject_param_raw(&InjectParamRaw::SingleOwned(dep_ref), &components, &idx)
            .unwrap();
        assert!(matches!(p, InjectParam::SingleOwned { .. }));

        let info = crate::model::TraitObjectInfo {
            primary_trait_key: "crate::MyTrait".to_string(),
            trait_object_key: "dyn crate::MyTrait".to_string(),
        };
        let p = resolve_inject_param_raw(
            &InjectParamRaw::SingleTraitRef {
                kind: SharedKind::Rc,
                info: info.clone(),
            },
            &components,
            &idx,
        )
        .unwrap();
        assert!(matches!(p, InjectParam::SingleTraitRef { .. }));

        let p = resolve_inject_param_raw(
            &InjectParamRaw::AllList {
                kind: SharedKind::Rc,
                info: info.clone(),
            },
            &components,
            &idx,
        )
        .unwrap();
        assert!(matches!(p, InjectParam::AllList { .. }));

        let p = resolve_inject_param_raw(
            &InjectParamRaw::AllMap {
                kind: SharedKind::Rc,
                info,
            },
            &components,
            &idx,
        )
        .unwrap();
        assert!(matches!(p, InjectParam::AllMap { .. }));
    }

    #[test]
    fn build_auto_fields_覆盖unit_tuple_named与错误分支() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            component("crate::Dep", "crate", "Dep"),
        );
        let idx = build_component_name_index(&components);

        let unit = Component {
            crate_ident: "crate".to_string(),
            type_key: "crate::Unit".to_string(),
            struct_name: "Unit".to_string(),
            inject: None,
            fields: ComponentFieldsRaw::Unit,
            auto_fields: None,
            scope_override: None,
        };
        assert!(
            build_auto_fields("crate::Unit", &unit, &components, &idx)
                .unwrap()
                .is_empty()
        );

        let tup = Component {
            fields: ComponentFieldsRaw::Tuple,
            ..unit.clone()
        };
        let err = build_auto_fields("crate::Tup", &tup, &components, &idx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("tuple struct"));

        let named_non_pub = Component {
            fields: ComponentFieldsRaw::Named(vec![ComponentFieldRaw {
                name: "a".to_string(),
                is_pub: false,
                ty: "crate::Dep".to_string(),
                inject: Some(InjectParamRaw::SingleOwned(TypeRef {
                    crate_ident: "crate".to_string(),
                    key: "crate::Dep".to_string(),
                    simple_name: Some("Dep".to_string()),
                })),
                inject_error: None,
            }]),
            ..unit.clone()
        };
        let err = build_auto_fields("crate::Named", &named_non_pub, &components, &idx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("不是 pub"));

        let named_inject_none = Component {
            fields: ComponentFieldsRaw::Named(vec![ComponentFieldRaw {
                name: "a".to_string(),
                is_pub: true,
                ty: "weird".to_string(),
                inject: None,
                inject_error: Some("bad".to_string()),
            }]),
            ..unit.clone()
        };
        let err = build_auto_fields("crate::Named", &named_inject_none, &components, &idx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("暂不支持自动注入"));

        let named_resolve_fail = Component {
            fields: ComponentFieldsRaw::Named(vec![ComponentFieldRaw {
                name: "a".to_string(),
                is_pub: true,
                ty: "Missing".to_string(),
                inject: Some(InjectParamRaw::SingleOwned(TypeRef {
                    crate_ident: "crate".to_string(),
                    key: "crate::Missing".to_string(),
                    simple_name: Some("Missing".to_string()),
                })),
                inject_error: None,
            }]),
            ..unit.clone()
        };
        let err = build_auto_fields("crate::Named", &named_resolve_fail, &components, &idx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("字段 a: Missing"));

        let named_borrow = Component {
            fields: ComponentFieldsRaw::Named(vec![ComponentFieldRaw {
                name: "a".to_string(),
                is_pub: true,
                ty: "&crate::Dep".to_string(),
                inject: Some(InjectParamRaw::SingleBorrow(TypeRef {
                    crate_ident: "crate".to_string(),
                    key: "crate::Dep".to_string(),
                    simple_name: Some("Dep".to_string()),
                })),
                inject_error: None,
            }]),
            ..unit.clone()
        };
        let err = build_auto_fields("crate::Named", &named_borrow, &components, &idx)
            .unwrap_err()
            .to_string();
        assert!(err.contains("暂不支持该注入方式"));

        let ok = Component {
            fields: ComponentFieldsRaw::Named(vec![ComponentFieldRaw {
                name: "dep".to_string(),
                is_pub: true,
                ty: "crate::Dep".to_string(),
                inject: Some(InjectParamRaw::SingleOwned(TypeRef {
                    crate_ident: "crate".to_string(),
                    key: "crate::Dep".to_string(),
                    simple_name: Some("Dep".to_string()),
                })),
                inject_error: None,
            }]),
            ..unit
        };
        let fields = build_auto_fields("crate::Ok", &ok, &components, &idx).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, "dep");
    }
}
