use std::collections::{BTreeMap, BTreeSet, VecDeque};

use anyhow::{Result, anyhow};

use crate::model::{Component, InjectParam};

pub(crate) fn topo_sort(
    components: &BTreeMap<String, Component>,
    trait_impls: &BTreeMap<String, Vec<String>>,
) -> Result<Vec<String>> {
    let mut indeg = BTreeMap::<String, usize>::new();
    let mut edges = BTreeMap::<String, Vec<String>>::new();

    for (ty, c) in components {
        indeg.entry(ty.clone()).or_insert(0);
        for dep in collect_component_deps(ty, c, trait_impls)? {
            if !components.contains_key(&dep) {
                return Err(anyhow!("组件 {} 依赖 {}，但未找到对应组件", ty, dep));
            }
            edges.entry(dep).or_default().push(ty.clone());
            *indeg.entry(ty.clone()).or_insert(0) += 1;
        }
    }

    let mut queue = VecDeque::new();
    for (k, v) in &indeg {
        if *v == 0 {
            queue.push_back(k.clone());
        }
    }

    let mut out = Vec::with_capacity(indeg.len());
    while let Some(n) = queue.pop_front() {
        out.push(n.clone());
        for m in edges.get(&n).into_iter().flatten() {
            let e = indeg.get_mut(m).ok_or_else(|| anyhow!("缺少 indeg: {m}"))?;
            *e -= 1;
            if *e == 0 {
                queue.push_back(m.clone());
            }
        }
    }

    if out.len() != indeg.len() {
        return Err(anyhow!("检测到循环依赖，无法生成对象图"));
    }
    Ok(out)
}

pub(crate) fn collect_root_components(
    components: &BTreeMap<String, Component>,
    trait_impls: &BTreeMap<String, Vec<String>>,
) -> Result<Vec<String>> {
    let mut used_as_dep = BTreeSet::<String>::new();
    for (ty, c) in components {
        for dep in collect_component_deps(ty, c, trait_impls)? {
            used_as_dep.insert(dep);
        }
    }

    let mut roots: Vec<String> = components
        .keys()
        .filter(|ty| !used_as_dep.contains(*ty))
        .cloned()
        .collect();
    roots.sort();
    Ok(roots)
}

pub(crate) fn collect_component_deps(
    ty: &str,
    component: &Component,
    trait_impls: &BTreeMap<String, Vec<String>>,
) -> Result<Vec<String>> {
    if let Some(inject) = component.inject.as_ref() {
        return Ok(collect_component_deps_from_params(
            inject.params.iter(),
            trait_impls,
        ));
    }
    let Some(fields) = component.auto_fields.as_ref() else {
        return Err(anyhow!("组件缺少 #[constructor] 且无法自动构造：{ty}"));
    };
    Ok(collect_component_deps_from_params(
        fields.iter().map(|f| &f.param),
        trait_impls,
    ))
}

fn collect_component_deps_from_params<'a, I>(
    params: I,
    trait_impls: &BTreeMap<String, Vec<String>>,
) -> Vec<String>
where
    I: IntoIterator<Item = &'a InjectParam>,
{
    let mut deps = Vec::new();
    for p in params {
        match p {
            InjectParam::SingleRef { dep_type, .. }
            | InjectParam::SingleBorrow { dep_type }
            | InjectParam::SingleOwned { dep_type } => deps.push(dep_type.clone()),
            InjectParam::SingleTraitRef { trait_primary, .. } => {
                if let Some(impls) = trait_impls.get(trait_primary) {
                    deps.extend(impls.iter().cloned());
                }
            }
            InjectParam::AllList { trait_primary, .. }
            | InjectParam::AllMap { trait_primary, .. } => {
                if let Some(impls) = trait_impls.get(trait_primary) {
                    deps.extend(impls.iter().cloned());
                }
            }
        }
    }
    deps
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{AutoField, ComponentFieldsRaw, InjectCtor, SharedKind};

    fn component_inject(ty: &str, params: Vec<InjectParam>) -> Component {
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
            scope_override: None,
        }
    }

    fn component_auto(ty: &str, params: Vec<InjectParam>) -> Component {
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
            scope_override: None,
        }
    }

    #[test]
    fn collect_component_deps_覆盖inject与auto_fields与缺失报错() {
        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert(
            "crate::MyTrait".to_string(),
            vec!["crate::B".to_string(), "crate::C".to_string()],
        );

        let c = component_inject(
            "crate::A",
            vec![
                InjectParam::SingleOwned {
                    dep_type: "crate::D".to_string(),
                },
                InjectParam::SingleTraitRef {
                    kind: SharedKind::Arc,
                    trait_primary: "crate::MyTrait".to_string(),
                    trait_object: "dyn crate::MyTrait".to_string(),
                },
                InjectParam::AllList {
                    kind: SharedKind::Arc,
                    trait_primary: "crate::MyTrait".to_string(),
                    trait_object: "dyn crate::MyTrait".to_string(),
                },
                InjectParam::AllMap {
                    kind: SharedKind::Arc,
                    trait_primary: "crate::MyTrait".to_string(),
                    trait_object: "dyn crate::MyTrait".to_string(),
                },
            ],
        );
        let deps = collect_component_deps("crate::A", &c, &trait_impls).unwrap();
        assert_eq!(
            deps,
            vec![
                "crate::D".to_string(),
                "crate::B".to_string(),
                "crate::C".to_string(),
                "crate::B".to_string(),
                "crate::C".to_string(),
                "crate::B".to_string(),
                "crate::C".to_string()
            ]
        );

        let c = component_auto(
            "crate::A",
            vec![InjectParam::SingleBorrow {
                dep_type: "crate::B".to_string(),
            }],
        );
        let deps = collect_component_deps("crate::A", &c, &BTreeMap::new()).unwrap();
        assert_eq!(deps, vec!["crate::B".to_string()]);

        let c = component_inject(
            "crate::A",
            vec![InjectParam::SingleTraitRef {
                kind: SharedKind::Arc,
                trait_primary: "crate::MissingTrait".to_string(),
                trait_object: "dyn crate::MissingTrait".to_string(),
            }],
        );
        let deps = collect_component_deps("crate::A", &c, &BTreeMap::new()).unwrap();
        assert!(deps.is_empty());

        let c = component_inject(
            "crate::A",
            vec![InjectParam::AllList {
                kind: SharedKind::Arc,
                trait_primary: "crate::MissingTrait".to_string(),
                trait_object: "dyn crate::MissingTrait".to_string(),
            }],
        );
        let deps = collect_component_deps("crate::A", &c, &BTreeMap::new()).unwrap();
        assert!(deps.is_empty());

        let c = Component {
            crate_ident: "crate".to_string(),
            type_key: "crate::A".to_string(),
            struct_name: "A".to_string(),
            inject: None,
            fields: ComponentFieldsRaw::Unit,
            auto_fields: None,
            scope_override: None,
        };
        let err = collect_component_deps("crate::A", &c, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("组件缺少 #[constructor] 且无法自动构造"));
    }

    #[test]
    fn collect_root_components_找出未被依赖的根() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            component_inject(
                "crate::A",
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        components.insert(
            "crate::B".to_string(),
            component_inject("crate::B", Vec::new()),
        );

        let roots = collect_root_components(&components, &BTreeMap::new()).unwrap();
        assert_eq!(roots, vec!["crate::A".to_string()]);
    }

    #[test]
    fn topo_sort_覆盖正常排序_缺失依赖_循环依赖() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::A".to_string(),
            component_inject(
                "crate::A",
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        components.insert(
            "crate::B".to_string(),
            component_inject("crate::B", Vec::new()),
        );
        let order = topo_sort(&components, &BTreeMap::new()).unwrap();
        assert_eq!(order, vec!["crate::B".to_string(), "crate::A".to_string()]);

        let mut components2 = BTreeMap::<String, Component>::new();
        components2.insert(
            "crate::A".to_string(),
            component_inject(
                "crate::A",
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::Missing".to_string(),
                }],
            ),
        );
        let err = topo_sort(&components2, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("未找到对应组件"));

        let mut components3 = BTreeMap::<String, Component>::new();
        components3.insert(
            "crate::A".to_string(),
            component_inject(
                "crate::A",
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::B".to_string(),
                }],
            ),
        );
        components3.insert(
            "crate::B".to_string(),
            component_inject(
                "crate::B",
                vec![InjectParam::SingleOwned {
                    dep_type: "crate::A".to_string(),
                }],
            ),
        );
        let err = topo_sort(&components3, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("循环依赖"));

        // 覆盖 indeg > 1 的分支（同一节点依赖多个上游）。
        let mut components4 = BTreeMap::<String, Component>::new();
        components4.insert(
            "crate::A".to_string(),
            component_inject("crate::A", Vec::new()),
        );
        components4.insert(
            "crate::B".to_string(),
            component_inject("crate::B", Vec::new()),
        );
        components4.insert(
            "crate::C".to_string(),
            component_inject(
                "crate::C",
                vec![
                    InjectParam::SingleOwned {
                        dep_type: "crate::A".to_string(),
                    },
                    InjectParam::SingleOwned {
                        dep_type: "crate::B".to_string(),
                    },
                ],
            ),
        );
        let order = topo_sort(&components4, &BTreeMap::new()).unwrap();
        let pos = |ty: &str| order.iter().position(|x| x == ty).unwrap();
        assert!(pos("crate::A") < pos("crate::C"));
        assert!(pos("crate::B") < pos("crate::C"));
    }
}
