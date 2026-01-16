use dris_rt::{All, Type, component, constructor};
use std::sync::Arc;

#[component(singleton)]
pub struct AppConfig {
    pub port: u16,
}

impl AppConfig {
    #[constructor]
    pub fn new() -> Self {
        Self { port: 8080 }
    }
}

#[component(singleton)]
pub struct AppPluginA;

impl third_party::Plugin for AppPluginA {
    fn id(&self) -> &'static str {
        "app-a"
    }
}

impl AppPluginA {
    #[constructor]
    pub fn new() -> Self {
        Self
    }
}

#[component(singleton)]
pub struct AppPluginB {
    pub base: u32,
}

impl third_party::Plugin for AppPluginB {
    fn id(&self) -> &'static str {
        "app-b"
    }
}

impl AppPluginB {
    #[constructor]
    pub fn new(cfg: &third_party::ThirdPartyConfig) -> Self {
        Self { base: cfg.base }
    }
}

#[component]
pub struct AppGreeting {
    pub text: &'static str,
}

impl AppGreeting {
    #[constructor]
    pub fn new() -> Self {
        Self { text: "hello" }
    }
}

#[component(singleton)]
pub struct AppService {
    pub cfg: Arc<AppConfig>,
    pub client: Arc<third_party::ThirdPartyClient>,
    pub greeting: AppGreeting,
    pub plugins: All<Arc<dyn third_party::Plugin>>,
    pub plugin_map: All<(Type, Arc<dyn third_party::Plugin>)>,
}

include!(concat!(env!("OUT_DIR"), "/dris_gen.rs"));

fn main() {
    let container = dris_gen::Container::build();
    let service = container.app_service();
    println!(
        "port={} base={} plugins={} map={}",
        service.cfg.port,
        service.client.base,
        service.plugins.len(),
        service.plugin_map.len()
    );
    println!("greeting={}", service.greeting.text);
}
