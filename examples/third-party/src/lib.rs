use dris_rt::{component, constructor};

pub trait Plugin {
    fn id(&self) -> &'static str;
}

#[component(singleton)]
pub struct ThirdPartyConfig {
    pub base: u32,
}

impl ThirdPartyConfig {
    #[constructor]
    pub fn new() -> Self {
        Self { base: 7 }
    }
}

#[component(singleton)]
pub struct ThirdPartyClient {
    pub base: u32,
}

impl ThirdPartyClient {
    #[constructor]
    pub fn new(cfg: &crate::ThirdPartyConfig) -> Self {
        Self { base: cfg.base }
    }
}

#[component(singleton)]
pub struct BuiltinPlugin;

impl Plugin for BuiltinPlugin {
    fn id(&self) -> &'static str {
        "builtin"
    }
}

impl BuiltinPlugin {
    #[constructor]
    pub fn new() -> Self {
        Self
    }
}
