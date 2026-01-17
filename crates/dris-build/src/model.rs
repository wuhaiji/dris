#[derive(Debug, Clone)]
pub(crate) struct ScanCtx {
    pub(crate) crate_ident: String,
    pub(crate) module_path: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypeRef {
    pub(crate) crate_ident: String,
    pub(crate) key: String,
    pub(crate) simple_name: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct TraitObjectInfo {
    pub(crate) primary_trait_key: String,
    pub(crate) trait_object_key: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum SharedKind {
    Arc,
    Rc,
}

#[derive(Debug, Clone)]
pub(crate) enum InjectParamRaw {
    SingleRef {
        kind: SharedKind,
        dep: TypeRef,
    },
    SingleTraitRef {
        kind: SharedKind,
        info: TraitObjectInfo,
    },
    SingleBorrow(TypeRef),
    SingleOwned(TypeRef),
    AllList {
        kind: SharedKind,
        info: TraitObjectInfo,
    },
    AllMap {
        kind: SharedKind,
        info: TraitObjectInfo,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct InjectCtorRaw {
    pub(crate) self_ty: TypeRef,
    pub(crate) call_path: String,
    pub(crate) params: Vec<InjectParamRaw>,
}

#[derive(Debug, Clone)]
pub(crate) struct TraitImplRaw {
    pub(crate) trait_key: String,
    pub(crate) self_ty: TypeRef,
}

#[derive(Debug, Clone)]
pub(crate) enum ComponentFieldsRaw {
    Unit,
    Named(Vec<ComponentFieldRaw>),
    Tuple,
}

#[derive(Debug, Clone)]
pub(crate) struct ComponentFieldRaw {
    pub(crate) name: String,
    pub(crate) is_pub: bool,
    pub(crate) ty: String,
    pub(crate) inject: Option<InjectParamRaw>,
    pub(crate) inject_error: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct ComponentDef {
    pub(crate) crate_ident: String,
    pub(crate) type_key: String,
    pub(crate) struct_name: String,
    pub(crate) fields: ComponentFieldsRaw,
    pub(crate) scope_override: Option<ComponentScope>,
}

#[derive(Default)]
pub(crate) struct ScanOut {
    pub(crate) components: Vec<ComponentDef>,
    pub(crate) injects: Vec<InjectCtorRaw>,
    pub(crate) trait_impls: Vec<TraitImplRaw>,
}

#[derive(Debug, Clone)]
pub(crate) struct InjectCtor {
    pub(crate) call_path: String,
    pub(crate) params: Vec<InjectParam>,
}

#[derive(Debug, Clone)]
pub(crate) enum InjectParam {
    SingleRef {
        kind: SharedKind,
        dep_type: String,
    },
    SingleTraitRef {
        kind: SharedKind,
        trait_primary: String,
        #[allow(dead_code)]
        trait_object: String,
    },
    SingleBorrow {
        dep_type: String,
    },
    SingleOwned {
        dep_type: String,
    },
    AllList {
        kind: SharedKind,
        trait_primary: String,
        trait_object: String,
    },
    AllMap {
        kind: SharedKind,
        trait_primary: String,
        trait_object: String,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct AutoField {
    pub(crate) name: String,
    pub(crate) param: InjectParam,
}

#[derive(Debug, Clone)]
pub(crate) struct Component {
    pub(crate) crate_ident: String,
    pub(crate) type_key: String,
    pub(crate) struct_name: String,
    pub(crate) inject: Option<InjectCtor>,
    pub(crate) fields: ComponentFieldsRaw,
    pub(crate) auto_fields: Option<Vec<AutoField>>,
    pub(crate) scope_override: Option<ComponentScope>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ComponentScope {
    Prototype,
    Singleton,
}

impl Component {
    pub(crate) fn scope(&self) -> ComponentScope {
        if let Some(scope) = self.scope_override {
            return scope;
        }
        ComponentScope::Prototype
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn component_scope_覆盖默认与显式覆盖() {
        let base = Component {
            crate_ident: "c".to_string(),
            type_key: "crate::Foo".to_string(),
            struct_name: "Foo".to_string(),
            inject: None,
            fields: ComponentFieldsRaw::Unit,
            auto_fields: None,
            scope_override: Some(ComponentScope::Singleton),
        };
        assert!(base.scope() == ComponentScope::Singleton);

        let mut c2 = base.clone();
        c2.scope_override = None;
        assert!(c2.scope() == ComponentScope::Prototype);
    }
}
