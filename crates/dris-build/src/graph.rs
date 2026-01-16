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
