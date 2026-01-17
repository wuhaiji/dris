use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::PathBuf,
};

use anyhow::{Result, anyhow};

use crate::{
    cargo::{
        direct_dependencies, find_lockfile, find_registry_src_dir, lock_pkg_depends_on,
        lock_reachable_packages, manifest_quick_mentions_dris, parse_lockfile,
    },
    codegen::{generate_container_code, minimal_generated_code},
    model::{Component, InjectCtor, InjectParam, SharedKind},
    resolve::{
        build_auto_fields, build_component_name_index, resolve_inject_param_raw, resolve_type_ref,
    },
    scan::scan_crate,
};

fn shared_kind_name(kind: SharedKind) -> &'static str {
    match kind {
        SharedKind::Arc => "Arc",
        SharedKind::Rc => "Rc",
    }
}

fn require_shared_kind(
    by_type: &mut BTreeMap<String, Option<SharedKind>>,
    ty: &str,
    kind: SharedKind,
) -> Result<()> {
    let Some(slot) = by_type.get_mut(ty) else {
        return Ok(());
    };
    match slot {
        None => {
            *slot = Some(kind);
            Ok(())
        }
        Some(existing) if *existing == kind => Ok(()),
        Some(existing) => Err(anyhow!(
            "组件 {} 同时被要求使用 {}<T> 与 {}<T>：无法保证同一实例，请统一",
            ty,
            shared_kind_name(*existing),
            shared_kind_name(kind)
        )),
    }
}

fn infer_holder_kind_by_type(
    components: &BTreeMap<String, Component>,
    trait_impls: &BTreeMap<String, Vec<String>>,
) -> Result<BTreeMap<String, Option<SharedKind>>> {
    let mut by_type = BTreeMap::<String, Option<SharedKind>>::new();
    for ty in components.keys() {
        by_type.insert(ty.clone(), None);
    }

    for c in components.values() {
        let params: Vec<&InjectParam> = if let Some(inject) = c.inject.as_ref() {
            inject.params.iter().collect()
        } else if let Some(fields) = c.auto_fields.as_ref() {
            fields.iter().map(|f| &f.param).collect()
        } else {
            Vec::new()
        };

        for p in params {
            match p {
                InjectParam::SingleRef { kind, dep_type } => {
                    require_shared_kind(&mut by_type, dep_type, *kind)?;
                }
                InjectParam::SingleTraitRef {
                    kind,
                    trait_primary,
                    ..
                }
                | InjectParam::AllList {
                    kind,
                    trait_primary,
                    ..
                }
                | InjectParam::AllMap {
                    kind,
                    trait_primary,
                    ..
                } => {
                    for impl_ty in trait_impls.get(trait_primary).into_iter().flatten() {
                        require_shared_kind(&mut by_type, impl_ty, *kind)?;
                    }
                }
                InjectParam::SingleBorrow { .. } | InjectParam::SingleOwned { .. } => {}
            }
        }
    }

    Ok(by_type)
}

pub(crate) fn generate() -> Result<()> {
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?);
    let out_dir = PathBuf::from(std::env::var("OUT_DIR")?);

    let mut rerun_if_changed = BTreeSet::<PathBuf>::new();
    let mut scanned_dirs = BTreeSet::<PathBuf>::new();
    let mut all_components = Vec::new();
    let mut all_injects = Vec::new();
    let mut all_trait_impls = Vec::new();
    let (scan, mut files) = scan_crate(&manifest_dir, "crate")?;
    all_components.extend(scan.components);
    all_injects.extend(scan.injects);
    all_trait_impls.extend(scan.trait_impls);
    rerun_if_changed.append(&mut files);
    scanned_dirs.insert(manifest_dir.clone());

    let lock_path = find_lockfile(&manifest_dir);
    let lock_packages = match lock_path {
        Some(ref p) => Some(parse_lockfile(p)?),
        None => {
            println!(
                "cargo:warning=dris-build: 未找到 Cargo.lock，无法递归扫描 registry 依赖中的组件"
            );
            None
        }
    };
    let root_pkg_name = std::env::var("CARGO_PKG_NAME").unwrap_or_default();
    let root_pkg_version = std::env::var("CARGO_PKG_VERSION").unwrap_or_default();

    let direct_deps = direct_dependencies(&manifest_dir.join("Cargo.toml"))?;
    let mut direct_dep_pkg_to_ident = BTreeMap::<String, String>::new();
    for dep in &direct_deps {
        if dep.key == "dris" {
            continue;
        }
        let package_name = dep.package.as_deref().unwrap_or(&dep.key);
        direct_dep_pkg_to_ident.insert(package_name.to_string(), dep.key.replace('-', "_"));
    }

    for dep in direct_deps {
        if dep.key == "dris" {
            continue;
        }

        let Some(dep_dir) = dep.path else {
            continue;
        };
        if !scanned_dirs.insert(dep_dir.clone()) {
            continue;
        }

        let crate_ident = dep.key.replace('-', "_");
        let (scan, mut files) = scan_crate(&dep_dir, &crate_ident)?;
        all_components.extend(scan.components);
        all_injects.extend(scan.injects);
        all_trait_impls.extend(scan.trait_impls);
        rerun_if_changed.append(&mut files);
    }

    if let Some(lock_packages) = lock_packages.as_ref() {
        if !root_pkg_name.is_empty() && !root_pkg_version.is_empty() {
            let reachable =
                lock_reachable_packages(lock_packages, &root_pkg_name, &root_pkg_version)?;
            for idx in reachable {
                let pkg = &lock_packages[idx];
                if pkg.name == root_pkg_name && pkg.version == root_pkg_version {
                    continue;
                }

                if !lock_pkg_depends_on(pkg, "dris-rt") && !lock_pkg_depends_on(pkg, "dris-macros")
                {
                    continue;
                }

                let is_registry = pkg
                    .source
                    .as_deref()
                    .map(|s| s.starts_with("registry+"))
                    .unwrap_or(false);
                if !is_registry {
                    continue;
                }

                let Some(src_dir) = find_registry_src_dir(&pkg.name, &pkg.version)? else {
                    println!(
                        "cargo:warning=dris-build: 未找到依赖源码目录：{}-{}（可能需要先下载依赖源码，例如 cargo fetch）",
                        pkg.name, pkg.version
                    );
                    continue;
                };
                if !scanned_dirs.insert(src_dir.clone()) {
                    continue;
                }

                if !manifest_quick_mentions_dris(&src_dir.join("Cargo.toml"))? {
                    continue;
                }

                let crate_ident = direct_dep_pkg_to_ident
                    .get(&pkg.name)
                    .cloned()
                    .unwrap_or_else(|| pkg.name.replace('-', "_"));

                let (scan, mut files) = scan_crate(&src_dir, &crate_ident)?;
                all_components.extend(scan.components);
                all_injects.extend(scan.injects);
                all_trait_impls.extend(scan.trait_impls);
                rerun_if_changed.append(&mut files);
            }
        } else {
            println!(
                "cargo:warning=dris-build: 未获取到 root 包信息（CARGO_PKG_NAME/CARGO_PKG_VERSION），跳过递归扫描"
            );
        }
    }

    let mut components = BTreeMap::<String, Component>::new();
    for def in all_components {
        if components.contains_key(&def.type_key) {
            return Err(anyhow!("组件类型重复：{}", def.type_key));
        }
        components.insert(
            def.type_key.clone(),
            Component {
                crate_ident: def.crate_ident,
                type_key: def.type_key,
                struct_name: def.struct_name,
                inject: None,
                fields: def.fields,
                auto_fields: None,
                scope_override: def.scope_override,
            },
        );
    }

    if components.is_empty() {
        fs::write(out_dir.join("dris_gen.rs"), minimal_generated_code())?;
        return Ok(());
    }

    let component_name_index = build_component_name_index(&components);

    let mut trait_impls = BTreeMap::<String, BTreeSet<String>>::new();
    for raw in all_trait_impls {
        let Ok(self_ty_key) = resolve_type_ref(&raw.self_ty, &components, &component_name_index)
        else {
            continue;
        };
        trait_impls
            .entry(raw.trait_key)
            .or_default()
            .insert(self_ty_key);
    }

    for raw in all_injects {
        let self_ty_key = resolve_type_ref(&raw.self_ty, &components, &component_name_index)?;

        let mut params = Vec::new();
        for p in raw.params {
            params.push(resolve_inject_param_raw(
                &p,
                &components,
                &component_name_index,
            )?);
        }

        let component = components.get_mut(&self_ty_key).unwrap();
        if component.inject.is_some() {
            return Err(anyhow!(
                "组件 {} 存在多个 #[constructor] 构造函数",
                self_ty_key
            ));
        }

        component.inject = Some(InjectCtor {
            call_path: raw.call_path,
            params,
        });
    }

    let mut auto_fields_by_type = BTreeMap::<String, Vec<crate::model::AutoField>>::new();
    let mut auto_errors = Vec::<String>::new();
    for (ty, c) in &components {
        if c.inject.is_some() {
            continue;
        }
        match build_auto_fields(ty, c, &components, &component_name_index) {
            Ok(fields) => {
                auto_fields_by_type.insert(ty.clone(), fields);
            }
            Err(e) => {
                auto_errors.push(format!("{ty}: {e}"));
            }
        }
    }
    for (ty, fields) in auto_fields_by_type {
        let component = components
            .get_mut(&ty)
            .ok_or_else(|| anyhow!("缺少组件: {ty}"))?;
        component.auto_fields = Some(fields);
    }
    if !auto_errors.is_empty() {
        return Err(anyhow!(
            "以下组件标记了 #[component] 但没有找到 #[constructor] 构造函数，且无法自动构造：\n{}",
            auto_errors.join("\n")
        ));
    }

    let trait_impls: BTreeMap<String, Vec<String>> = trait_impls
        .into_iter()
        .map(|(k, set)| (k, set.into_iter().collect()))
        .collect();

    let order = crate::graph::topo_sort(&components, &trait_impls)?;
    crate::validate::validate_component_scopes(&components, &trait_impls)?;
    let roots = crate::graph::collect_root_components(&components, &trait_impls)?;
    let holder_kinds = infer_holder_kind_by_type(&components, &trait_impls)?;
    let generated =
        generate_container_code(&components, &trait_impls, &order, &roots, &holder_kinds)?;
    fs::write(out_dir.join("dris_gen.rs"), generated)?;

    for path in rerun_if_changed {
        println!("cargo:rerun-if-changed={}", path.display());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{AutoField, ComponentFieldsRaw, InjectCtor};

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
    fn require_shared_kind_覆盖缺失_首次写入_一致_冲突() {
        let mut by_type = BTreeMap::<String, Option<SharedKind>>::new();
        require_shared_kind(&mut by_type, "crate::Missing", SharedKind::Arc).unwrap();

        by_type.insert("crate::A".to_string(), None);
        require_shared_kind(&mut by_type, "crate::A", SharedKind::Arc).unwrap();
        assert_eq!(by_type["crate::A"], Some(SharedKind::Arc));

        require_shared_kind(&mut by_type, "crate::A", SharedKind::Arc).unwrap();
        let err = require_shared_kind(&mut by_type, "crate::A", SharedKind::Rc)
            .unwrap_err()
            .to_string();
        assert!(err.contains("同时被要求使用"));
    }

    #[test]
    fn infer_holder_kind_by_type_覆盖注入与trait实现推断() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Dep".to_string(),
            component_inject("crate::Dep", Vec::new()),
        );
        components.insert(
            "crate::A".to_string(),
            component_inject(
                "crate::A",
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Arc,
                    dep_type: "crate::Dep".to_string(),
                }],
            ),
        );
        let holder = infer_holder_kind_by_type(&components, &BTreeMap::new()).unwrap();
        assert_eq!(holder["crate::Dep"], Some(SharedKind::Arc));
        assert_eq!(holder["crate::A"], None);

        let mut components2 = components.clone();
        components2.insert(
            "crate::B".to_string(),
            component_inject(
                "crate::B",
                vec![InjectParam::SingleRef {
                    kind: SharedKind::Rc,
                    dep_type: "crate::Dep".to_string(),
                }],
            ),
        );
        let err = infer_holder_kind_by_type(&components2, &BTreeMap::new())
            .unwrap_err()
            .to_string();
        assert!(err.contains("同时被要求使用"));
        assert!(err.contains("Arc"));
        assert!(err.contains("Rc"));

        let mut components3 = BTreeMap::<String, Component>::new();
        components3.insert(
            "crate::Impl".to_string(),
            component_inject("crate::Impl", Vec::new()),
        );
        components3.insert(
            "crate::A".to_string(),
            component_auto(
                "crate::A",
                vec![InjectParam::AllList {
                    kind: SharedKind::Rc,
                    trait_primary: "crate::T".to_string(),
                    trait_object: "dyn crate::T".to_string(),
                }],
            ),
        );
        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert("crate::T".to_string(), vec!["crate::Impl".to_string()]);

        let holder = infer_holder_kind_by_type(&components3, &trait_impls).unwrap();
        assert_eq!(holder["crate::Impl"], Some(SharedKind::Rc));
    }

    #[test]
    fn infer_holder_kind_by_type_覆盖_single_trait_ref_all_map_以及空组件参数() {
        let mut components = BTreeMap::<String, Component>::new();
        components.insert(
            "crate::Impl1".to_string(),
            component_inject("crate::Impl1", Vec::new()),
        );
        components.insert(
            "crate::Impl2".to_string(),
            component_inject("crate::Impl2", Vec::new()),
        );
        components.insert(
            "crate::UseTraitRef".to_string(),
            component_inject(
                "crate::UseTraitRef",
                vec![InjectParam::SingleTraitRef {
                    kind: SharedKind::Arc,
                    trait_primary: "crate::T".to_string(),
                    trait_object: "dyn crate::T".to_string(),
                }],
            ),
        );
        components.insert(
            "crate::UseAllMap".to_string(),
            component_inject(
                "crate::UseAllMap",
                vec![InjectParam::AllMap {
                    kind: SharedKind::Rc,
                    trait_primary: "crate::U".to_string(),
                    trait_object: "dyn crate::U".to_string(),
                }],
            ),
        );

        // 单纯为了覆盖 match 的 no-op 分支。
        components.insert(
            "crate::Noop".to_string(),
            component_inject(
                "crate::Noop",
                vec![
                    InjectParam::SingleBorrow {
                        dep_type: "crate::Impl1".to_string(),
                    },
                    InjectParam::SingleOwned {
                        dep_type: "crate::Impl2".to_string(),
                    },
                ],
            ),
        );

        // 覆盖 params = Vec::new() 分支：既无 inject 也无 auto_fields。
        components.insert(
            "crate::Empty".to_string(),
            Component {
                crate_ident: "crate".to_string(),
                type_key: "crate::Empty".to_string(),
                struct_name: "Empty".to_string(),
                inject: None,
                fields: ComponentFieldsRaw::Unit,
                auto_fields: None,
                scope_override: None,
            },
        );

        let mut trait_impls = BTreeMap::<String, Vec<String>>::new();
        trait_impls.insert("crate::T".to_string(), vec!["crate::Impl1".to_string()]);
        trait_impls.insert("crate::U".to_string(), vec!["crate::Impl2".to_string()]);

        let holder = infer_holder_kind_by_type(&components, &trait_impls).unwrap();
        assert_eq!(holder["crate::Impl1"], Some(SharedKind::Arc));
        assert_eq!(holder["crate::Impl2"], Some(SharedKind::Rc));
        assert_eq!(holder["crate::Empty"], None);
    }
}
