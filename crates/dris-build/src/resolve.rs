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
