use std::sync::Mutex;

use tempfile::TempDir;

static ENV_LOCK: Mutex<()> = Mutex::new(());

struct EnvGuard {
    saved: Vec<(String, Option<String>)>,
}

impl EnvGuard {
    fn new() -> Self {
        Self { saved: Vec::new() }
    }

    fn set(&mut self, key: &str, value: &str) {
        let old = std::env::var(key).ok();
        self.saved.push((key.to_string(), old));
        unsafe {
            std::env::set_var(key, value);
        }
    }
}

impl Drop for EnvGuard {
    fn drop(&mut self) {
        for (k, old) in self.saved.drain(..).rev() {
            match old {
                Some(v) => unsafe { std::env::set_var(&k, v) },
                None => unsafe { std::env::remove_var(&k) },
            }
        }
    }
}

fn write_lib_crate(dir: &std::path::Path, cargo_toml: &str, lib_rs: &str) {
    std::fs::create_dir_all(dir.join("src")).unwrap();
    std::fs::write(dir.join("Cargo.toml"), cargo_toml).unwrap();
    std::fs::write(dir.join("src/lib.rs"), lib_rs).unwrap();
}

fn write_lockfile(dir: &std::path::Path, content: &str) {
    std::fs::write(dir.join("Cargo.lock"), content).unwrap();
}

#[test]
fn generate_无组件时输出最小容器() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(manifest_dir.join("src")).unwrap();
    std::fs::create_dir_all(&out_dir).unwrap();
    std::fs::write(
        manifest_dir.join("Cargo.toml"),
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"
"#,
    )
    .unwrap();
    std::fs::write(manifest_dir.join("src/lib.rs"), "pub struct X;").unwrap();

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");

    dris_build::generate().unwrap();

    let gen_path = out_dir.join("dris_gen.rs");
    let content = std::fs::read_to_string(&gen_path).unwrap();
    assert!(content.contains("pub struct Container"));
    syn::parse_file(&content).unwrap();
}

#[test]
fn generate_有组件时会生成getter与单例初始化() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(manifest_dir.join("src")).unwrap();
    std::fs::create_dir_all(&out_dir).unwrap();
    std::fs::write(
        manifest_dir.join("Cargo.toml"),
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"
"#,
    )
    .unwrap();
    std::fs::write(
        manifest_dir.join("src/lib.rs"),
        r#"
use dris_rt::{component, constructor};
use std::sync::Arc;

#[component(singleton)]
pub struct Config;
impl Config {
    #[constructor]
    pub fn new() -> Self { Self }
}

#[component]
pub struct App;
impl App {
    #[constructor]
    pub fn new(_cfg: Arc<Config>) -> Self { Self }
}
"#,
    )
    .unwrap();

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");

    dris_build::generate().unwrap();

    let gen_path = out_dir.join("dris_gen.rs");
    let content = std::fs::read_to_string(&gen_path).unwrap();
    syn::parse_file(&content).unwrap();
    assert!(content.contains("pub struct Container"));
    assert!(content.contains("fn app"));
    assert!(content.contains("Config"));
}

#[test]
fn generate_会扫描路径依赖与registry依赖并覆盖关键分支() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    let cargo_home = tmp.path().join("cargo_home");
    std::fs::create_dir_all(&out_dir).unwrap();

    let reg_root = cargo_home.join("registry").join("src").join("reg1");
    std::fs::create_dir_all(&reg_root).unwrap();

    let foo_dir = reg_root.join("foo-pkg-1.0.0");
    let bar_dir = reg_root.join("bar-pkg-1.0.0");
    let baz_dir = reg_root.join("baz-1.0.0");
    let no_mention_dir = reg_root.join("no-mention-1.0.0");

    write_lib_crate(
        &foo_dir,
        r#"[package]
name = "foo-pkg"
version = "1.0.0"
edition = "2024"

[dependencies]
dris-rt = "0.1"
"#,
        r#"
use dris_rt::{component, constructor};

#[component]
pub struct Foo;
impl Foo {
    #[constructor]
    pub fn new() -> Self { Self }
}
"#,
    );
    write_lib_crate(
        &bar_dir,
        r#"[package]
name = "bar-pkg"
version = "1.0.0"
edition = "2024"

[dependencies]
dris-macros = "0.1"
"#,
        r#"
use dris_rt::{component, constructor};

#[component]
pub struct Bar;
impl Bar {
    #[constructor]
    pub fn new() -> Self { Self }
}
"#,
    );
    write_lib_crate(
        &baz_dir,
        r#"[package]
name = "baz"
version = "1.0.0"
edition = "2024"

[dependencies]
dris-rt = "0.1"
"#,
        r#"
use dris_rt::{component, constructor};

#[component]
pub struct Baz;
impl Baz {
    #[constructor]
    pub fn new() -> Self { Self }
}
"#,
    );
    write_lib_crate(
        &no_mention_dir,
        r#"[package]
name = "no-mention"
version = "1.0.0"
edition = "2024"
"#,
        "pub struct X;",
    );

    let foo_abs = foo_dir.to_str().unwrap();
    let cargo_toml = format!(
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"

[dependencies]
dris = "0.1"
serde = "1"
foo = {{ package = "foo-pkg", version = "1" }}
bar = {{ package = "bar-pkg", version = "1" }}
foo_path = {{ path = "{foo_abs}" }}
foo_path_dup = {{ path = "{foo_abs}" }}
"#
    );

    let lib_rs = r#"
use dris_rt::{component, constructor};
use std::sync::Arc;

pub trait Marker {}

#[component(singleton)]
pub struct Config;
impl Config {
    #[constructor]
    pub fn new() -> Self { Self }
}

#[component]
pub struct App;
impl App {
    #[constructor]
    pub fn new(_cfg: Arc<Config>) -> Self { Self }
}

impl Marker for App {}

pub struct Unknown;
impl Marker for Unknown {}
"#;

    write_lib_crate(&manifest_dir, &cargo_toml, lib_rs);
    write_lockfile(
        &manifest_dir,
        r#"
[[package]]
name = "app"
version = "0.1.0"
dependencies = [
 "app 0.2.0 (registry+https://example)",
 "foo-pkg 1.0.0 (registry+https://example)",
 "bar-pkg 1.0.0 (registry+https://example)",
 "baz 1.0.0 (registry+https://example)",
 "no-dris 1.0.0 (registry+https://example)",
 "git-pkg 1.0.0 (git+https://example)",
 "missing-src 1.0.0 (registry+https://example)",
 "no-mention 1.0.0 (registry+https://example)",
]

[[package]]
name = "app"
version = "0.2.0"
source = "registry+https://example"

[[package]]
name = "foo-pkg"
version = "1.0.0"
source = "registry+https://example"
dependencies = ["dris-rt 0.1.0 (registry+https://example)"]

[[package]]
name = "bar-pkg"
version = "1.0.0"
source = "registry+https://example"
dependencies = ["dris-macros 0.1.0 (registry+https://example)"]

[[package]]
name = "baz"
version = "1.0.0"
source = "registry+https://example"
dependencies = ["dris-rt 0.1.0 (registry+https://example)"]

[[package]]
name = "no-mention"
version = "1.0.0"
source = "registry+https://example"
dependencies = ["dris-rt 0.1.0 (registry+https://example)"]

[[package]]
name = "no-dris"
version = "1.0.0"
source = "registry+https://example"

[[package]]
name = "git-pkg"
version = "1.0.0"
source = "git+https://example"
dependencies = ["dris-rt 0.1.0 (registry+https://example)"]

[[package]]
name = "missing-src"
version = "1.0.0"
source = "registry+https://example"
dependencies = ["dris-rt 0.1.0 (registry+https://example)"]
"#,
    );

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");
    env.set("CARGO_HOME", cargo_home.to_str().unwrap());

    dris_build::generate().unwrap();

    let gen_path = out_dir.join("dris_gen.rs");
    let content = std::fs::read_to_string(&gen_path).unwrap();
    syn::parse_file(&content).unwrap();
    assert!(content.contains("pub struct Container"));
}

#[test]
fn generate_锁文件存在但root信息缺失时跳过递归扫描() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(&out_dir).unwrap();

    write_lib_crate(
        &manifest_dir,
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"
"#,
        "pub struct X;",
    );
    write_lockfile(
        &manifest_dir,
        r#"
[[package]]
name = "app"
version = "0.1.0"
"#,
    );

    // case: name/version 都为空（short-circuit 只覆盖第一个判断）
    {
        let mut env = EnvGuard::new();
        env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
        env.set("OUT_DIR", out_dir.to_str().unwrap());
        env.set("CARGO_PKG_NAME", "");
        env.set("CARGO_PKG_VERSION", "");

        dris_build::generate().unwrap();

        let gen_path = out_dir.join("dris_gen.rs");
        let content = std::fs::read_to_string(&gen_path).unwrap();
        syn::parse_file(&content).unwrap();
    }

    // case: 仅 version 为空（覆盖第二个判断为 false 的分支）
    {
        let mut env = EnvGuard::new();
        env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
        env.set("OUT_DIR", out_dir.to_str().unwrap());
        env.set("CARGO_PKG_NAME", "app");
        env.set("CARGO_PKG_VERSION", "");

        dris_build::generate().unwrap();

        let gen_path = out_dir.join("dris_gen.rs");
        let content = std::fs::read_to_string(&gen_path).unwrap();
        syn::parse_file(&content).unwrap();
    }
}

#[test]
fn generate_同一组件多个constructor会报错() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(&out_dir).unwrap();

    write_lib_crate(
        &manifest_dir,
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"
"#,
        r#"
use dris_rt::{component, constructor};

#[component]
pub struct A;
impl A {
    #[constructor]
    pub fn new() -> Self { Self }

    #[constructor]
    pub fn new2() -> Self { Self }
}
"#,
    );

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");

    let err = dris_build::generate().unwrap_err().to_string();
    assert!(err.contains("多个 #[constructor]"));
}

#[test]
fn generate_constructor参数无法解析会报错() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(&out_dir).unwrap();

    write_lib_crate(
        &manifest_dir,
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"
"#,
        r#"
use dris_rt::{component, constructor};
use std::sync::Arc;

#[component]
pub struct A;
impl A {
    #[constructor]
    pub fn new(_x: Arc<Missing>) -> Self { Self }
}
"#,
    );

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");

    let err = dris_build::generate().unwrap_err().to_string();
    assert!(err.contains("未找到组件类型"));
}

#[test]
fn generate_自动构造成功时会继续生成代码() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(&out_dir).unwrap();

    write_lib_crate(
        &manifest_dir,
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"
"#,
        r#"
use dris_rt::component;

#[component]
pub struct AutoOk;
"#,
    );

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");

    dris_build::generate().unwrap();

    let gen_path = out_dir.join("dris_gen.rs");
    let content = std::fs::read_to_string(&gen_path).unwrap();
    syn::parse_file(&content).unwrap();
    assert!(content.contains("Container"));
}

#[test]
fn generate_自动构造失败会聚合错误并返回() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(&out_dir).unwrap();

    write_lib_crate(
        &manifest_dir,
        r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"
"#,
        r#"
use dris_rt::component;

#[component]
pub struct AutoOk;

#[component]
pub struct AutoBad(u32);
"#,
    );

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");

    let err = dris_build::generate().unwrap_err().to_string();
    assert!(err.contains("无法自动构造"));
    assert!(err.contains("AutoBad"));
}

#[test]
fn generate_依赖crate_ident冲突会导致组件类型重复() {
    let _lock = ENV_LOCK.lock().unwrap();

    let tmp = TempDir::new().unwrap();
    let manifest_dir = tmp.path().join("app");
    let out_dir = tmp.path().join("out");
    std::fs::create_dir_all(&out_dir).unwrap();

    let dep1 = tmp.path().join("dep1");
    let dep2 = tmp.path().join("dep2");
    write_lib_crate(
        &dep1,
        r#"[package]
name = "dep1"
version = "0.1.0"
edition = "2024"
"#,
        r#"
use dris_rt::{component, constructor};

#[component]
pub struct Dup;
impl Dup {
    #[constructor]
    pub fn new() -> Self { Self }
}
"#,
    );
    write_lib_crate(
        &dep2,
        r#"[package]
name = "dep2"
version = "0.1.0"
edition = "2024"
"#,
        r#"
use dris_rt::{component, constructor};

#[component]
pub struct Dup;
impl Dup {
    #[constructor]
    pub fn new() -> Self { Self }
}
"#,
    );

    let dep1_abs = dep1.to_str().unwrap();
    let dep2_abs = dep2.to_str().unwrap();
    write_lib_crate(
        &manifest_dir,
        &format!(
            r#"[package]
name = "app"
version = "0.1.0"
edition = "2024"

[dependencies]
foo-bar = {{ path = "{dep1_abs}" }}
foo_bar = {{ path = "{dep2_abs}" }}
"#
        ),
        "pub struct X;",
    );

    let mut env = EnvGuard::new();
    env.set("CARGO_MANIFEST_DIR", manifest_dir.to_str().unwrap());
    env.set("OUT_DIR", out_dir.to_str().unwrap());
    env.set("CARGO_PKG_NAME", "app");
    env.set("CARGO_PKG_VERSION", "0.1.0");

    let err = dris_build::generate().unwrap_err().to_string();
    assert!(err.contains("组件类型重复"));
}
