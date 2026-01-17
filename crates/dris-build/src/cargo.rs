use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, anyhow};

#[derive(Debug, Clone)]
pub(crate) struct DepSpec {
    pub(crate) key: String,
    pub(crate) package: Option<String>,
    pub(crate) path: Option<PathBuf>,
}

pub(crate) fn direct_dependencies(manifest_path: &Path) -> Result<Vec<DepSpec>> {
    let manifest_dir = manifest_path
        .parent()
        .ok_or_else(|| anyhow!("无效 manifest_path: {}", manifest_path.display()))?;
    let content = fs::read_to_string(manifest_path)
        .with_context(|| format!("读取 Cargo.toml 失败: {}", manifest_path.display()))?;

    let mut deps = BTreeMap::<String, DepSpec>::new();
    let mut mode = TomlMode::None;
    for raw_line in content.lines() {
        let line = raw_line.split('#').next().unwrap_or("").trim();
        if line.is_empty() {
            continue;
        }

        if let Some(section) = parse_toml_section(line) {
            mode = match section.as_str() {
                "dependencies" => TomlMode::DepsInline,
                _ => {
                    if let Some(dep) = section.strip_prefix("dependencies.") {
                        TomlMode::DepTable(dep.to_string())
                    } else {
                        TomlMode::None
                    }
                }
            };
            continue;
        }

        match &mode {
            TomlMode::DepsInline => {
                let Some((key, value)) = parse_toml_kv(line) else {
                    continue;
                };
                let spec = deps.entry(key.to_string()).or_insert_with(|| DepSpec {
                    key: key.to_string(),
                    package: None,
                    path: None,
                });

                if let Some(path_value) = parse_inline_table_string(value, "path") {
                    spec.path = Some(manifest_dir.join(path_value));
                }
                if let Some(package_value) = parse_inline_table_string(value, "package") {
                    spec.package = Some(package_value.to_string());
                }
            }
            TomlMode::DepTable(dep_key) => {
                let Some((key, value)) = parse_toml_kv(line) else {
                    continue;
                };
                let spec = deps.entry(dep_key.clone()).or_insert_with(|| DepSpec {
                    key: dep_key.clone(),
                    package: None,
                    path: None,
                });
                if key == "path" {
                    let Some(path_value) = parse_toml_string(value) else {
                        continue;
                    };
                    spec.path = Some(manifest_dir.join(path_value));
                    continue;
                }
                if key == "package" {
                    let Some(package_value) = parse_toml_string(value) else {
                        continue;
                    };
                    spec.package = Some(package_value.to_string());
                    continue;
                }
            }
            TomlMode::None => {}
        }
    }

    Ok(deps.into_values().collect())
}

#[derive(Debug, Clone)]
enum TomlMode {
    None,
    DepsInline,
    DepTable(String),
}

fn parse_toml_section(line: &str) -> Option<String> {
    let line = line.trim();
    if !(line.starts_with('[') && line.ends_with(']')) {
        return None;
    }
    let inner = &line[1..line.len() - 1];
    Some(inner.trim().to_string())
}

fn parse_toml_kv(line: &str) -> Option<(&str, &str)> {
    let (k, v) = line.split_once('=')?;
    Some((k.trim(), v.trim()))
}

fn parse_toml_string(value: &str) -> Option<&str> {
    let value = value.trim();
    if let Some(inner) = value.strip_prefix('"').and_then(|v| v.strip_suffix('"')) {
        return Some(inner);
    }
    if let Some(inner) = value.strip_prefix('\'').and_then(|v| v.strip_suffix('\'')) {
        return Some(inner);
    }
    None
}

fn parse_inline_table_string<'a>(value: &'a str, key: &str) -> Option<&'a str> {
    let value = value.trim();
    if !(value.starts_with('{') && value.ends_with('}')) {
        return None;
    }
    let inner = value[1..value.len() - 1].trim();
    for part in inner.split(',') {
        let part = part.trim();
        let Some((k, v)) = parse_toml_kv(part) else {
            continue;
        };
        if k != key {
            continue;
        }
        if let Some(s) = parse_toml_string(v) {
            return Some(s);
        }
    }
    None
}

pub(crate) fn find_lockfile(start_dir: &Path) -> Option<PathBuf> {
    let mut dir = start_dir;
    loop {
        let cand = dir.join("Cargo.lock");
        if cand.exists() {
            return Some(cand);
        }
        let Some(parent) = dir.parent() else {
            return None;
        };
        dir = parent;
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LockPackage {
    pub(crate) name: String,
    pub(crate) version: String,
    pub(crate) source: Option<String>,
    pub(crate) dependencies: Vec<String>,
}

pub(crate) fn parse_lockfile(lock_path: &Path) -> Result<Vec<LockPackage>> {
    let content = fs::read_to_string(lock_path)
        .with_context(|| format!("读取 Cargo.lock 失败: {}", lock_path.display()))?;

    let mut out = Vec::new();
    let mut current: Option<LockPackage> = None;
    let mut in_deps = false;

    for raw_line in content.lines() {
        let line = raw_line.trim();
        if line == "[[package]]" {
            if let Some(p) = current.take() {
                out.push(p);
            }
            current = Some(LockPackage {
                name: String::new(),
                version: String::new(),
                source: None,
                dependencies: Vec::new(),
            });
            in_deps = false;
            continue;
        }

        let Some(pkg) = current.as_mut() else {
            continue;
        };

        if in_deps {
            for s in extract_quoted_strings(line) {
                pkg.dependencies.push(s);
            }
            if line.contains(']') {
                in_deps = false;
            }
            continue;
        }

        if line.starts_with("name") {
            let Some((k, v)) = parse_toml_kv(line) else {
                continue;
            };
            if k == "name" {
                if let Some(s) = parse_toml_string(v) {
                    pkg.name = s.to_string();
                }
            }
            continue;
        }

        if line.starts_with("version") {
            let Some((k, v)) = parse_toml_kv(line) else {
                continue;
            };
            if k == "version" {
                if let Some(s) = parse_toml_string(v) {
                    pkg.version = s.to_string();
                }
            }
            continue;
        }

        if line.starts_with("source") {
            let Some((k, v)) = parse_toml_kv(line) else {
                continue;
            };
            if k == "source" {
                if let Some(s) = parse_toml_string(v) {
                    pkg.source = Some(s.to_string());
                }
            }
            continue;
        }

        if line.starts_with("dependencies") && line.contains('[') {
            in_deps = true;
            for s in extract_quoted_strings(line) {
                pkg.dependencies.push(s);
            }
            if line.contains(']') {
                in_deps = false;
            }
            continue;
        }
    }

    if let Some(p) = current.take() {
        out.push(p);
    }

    Ok(out)
}

pub(crate) fn lock_reachable_packages(
    lock_packages: &[LockPackage],
    root_name: &str,
    root_version: &str,
) -> Result<Vec<usize>> {
    let mut by_id = BTreeMap::<(String, String), usize>::new();
    let mut by_name = BTreeMap::<String, Vec<usize>>::new();
    for (i, p) in lock_packages.iter().enumerate() {
        by_id.insert((p.name.clone(), p.version.clone()), i);
        by_name.entry(p.name.clone()).or_default().push(i);
    }

    let root_idx = *by_id
        .get(&(root_name.to_string(), root_version.to_string()))
        .ok_or_else(|| anyhow!("Cargo.lock 未找到 root package：{root_name} {root_version}"))?;

    let mut queue = VecDeque::new();
    queue.push_back(root_idx);
    let mut visited = BTreeSet::<usize>::new();
    visited.insert(root_idx);

    while let Some(idx) = queue.pop_front() {
        for dep in &lock_packages[idx].dependencies {
            let (name, version) = parse_lock_dependency(dep);
            if name.is_empty() {
                continue;
            }

            let candidates: Vec<usize> = if let Some(version) = version {
                by_id
                    .get(&(name.clone(), version))
                    .copied()
                    .into_iter()
                    .collect()
            } else {
                by_name.get(&name).cloned().unwrap_or_default()
            };

            for next in candidates {
                if visited.insert(next) {
                    queue.push_back(next);
                }
            }
        }
    }

    Ok(visited.into_iter().collect())
}

fn parse_lock_dependency(s: &str) -> (String, Option<String>) {
    let mut it = s.split_whitespace();
    let name = it.next().unwrap_or("").to_string();
    let Some(v) = it.next() else {
        return (name, None);
    };
    if looks_like_semver(v) {
        return (name, Some(v.to_string()));
    }
    (name, None)
}

fn looks_like_semver(s: &str) -> bool {
    s.chars()
        .next()
        .map(|c| c.is_ascii_digit())
        .unwrap_or(false)
}

pub(crate) fn lock_pkg_depends_on(pkg: &LockPackage, dep_name: &str) -> bool {
    pkg.dependencies
        .iter()
        .any(|d| d.split_whitespace().next() == Some(dep_name))
}

fn extract_quoted_strings(line: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut chars = line.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '"' {
            continue;
        }
        let mut s = String::new();
        while let Some(ch2) = chars.next() {
            if ch2 == '"' {
                break;
            }
            if ch2 == '\\' {
                if let Some(escaped) = chars.next() {
                    s.push(escaped);
                }
                continue;
            }
            s.push(ch2);
        }
        out.push(s);
    }
    out
}

pub(crate) fn find_registry_src_dir(package_name: &str, version: &str) -> Result<Option<PathBuf>> {
    let cargo_home = if let Ok(v) = std::env::var("CARGO_HOME") {
        PathBuf::from(v)
    } else {
        let home = std::env::var("HOME")?;
        PathBuf::from(home).join(".cargo")
    };
    let src_root = cargo_home.join("registry").join("src");
    let Ok(registries) = fs::read_dir(&src_root) else {
        return Ok(None);
    };

    let folder = format!("{package_name}-{version}");
    for reg in registries {
        let reg = reg?;
        let reg_path = reg.path();
        let cand = reg_path.join(&folder);
        if cand.exists() {
            return Ok(Some(cand));
        }
    }
    Ok(None)
}

pub(crate) fn manifest_quick_mentions_dris(manifest_path: &Path) -> Result<bool> {
    let Ok(content) = fs::read_to_string(manifest_path) else {
        return Ok(false);
    };
    Ok(content.contains("dris"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_env_var(key: &str, value: &str) -> impl Drop {
        struct Guard {
            key: String,
            old: Option<String>,
        }
        impl Drop for Guard {
            fn drop(&mut self) {
                if let Some(v) = self.old.as_ref() {
                    unsafe {
                        std::env::set_var(&self.key, v);
                    }
                } else {
                    unsafe {
                        std::env::remove_var(&self.key);
                    }
                }
            }
        }

        let old = std::env::var(key).ok();
        unsafe {
            std::env::set_var(key, value);
        }
        Guard {
            key: key.to_string(),
            old,
        }
    }

    fn restore_env_var(key: &str, old: Option<String>) {
        match old {
            Some(v) => unsafe { std::env::set_var(key, v) },
            None => unsafe { std::env::remove_var(key) },
        }
    }

    #[test]
    fn toml_parse_helpers_覆盖常见分支() {
        assert_eq!(
            parse_toml_section("[dependencies]").as_deref(),
            Some("dependencies")
        );
        assert!(parse_toml_section("[dependencies").is_none());
        assert!(parse_toml_section("dependencies").is_none());

        assert_eq!(parse_toml_kv("a = 1").unwrap(), ("a", "1"));
        assert!(parse_toml_kv("nope").is_none());

        assert_eq!(parse_toml_string("\"a\""), Some("a"));
        assert_eq!(parse_toml_string("'a'"), Some("a"));
        assert!(parse_toml_string("a").is_none());

        assert_eq!(
            parse_inline_table_string("{ path = \"../x\" }", "path"),
            Some("../x")
        );
        assert_eq!(
            parse_inline_table_string("{ bad, path = \"../x\" }", "path"),
            Some("../x")
        );
        assert!(parse_inline_table_string("not_table", "path").is_none());
        assert!(parse_inline_table_string("{ path = \"../x\" ", "path").is_none());
        assert!(parse_inline_table_string("{ x = \"1\" }", "path").is_none());
        assert!(parse_inline_table_string("{ path = ../x }", "path").is_none());
    }

    #[test]
    fn direct_dependencies_覆盖inline与table两种写法() {
        let tmp = TempDir::new().unwrap();
        let manifest = tmp.path().join("Cargo.toml");
        std::fs::write(
            &manifest,
            r#"
[package]
name = "t"
version = "0.1.0"

[dependencies]
foo = "1"
broken
bar = { path = "../bar" }
baz = { package = "baz-real" }

[dependencies.qux]
broken
path = 123
package = 123
optional = true
path = "../qux"
package = "qux-real"
"#,
        )
        .unwrap();

        let deps = direct_dependencies(&manifest).unwrap();
        let mut by_key = BTreeMap::<String, DepSpec>::new();
        for d in deps {
            by_key.insert(d.key.clone(), d);
        }

        assert!(by_key.contains_key("foo"));
        assert_eq!(by_key["baz"].package.as_deref(), Some("baz-real"));
        assert!(by_key["bar"].path.as_ref().unwrap().ends_with("../bar"));
        assert_eq!(by_key["qux"].package.as_deref(), Some("qux-real"));
        assert!(by_key["qux"].path.as_ref().unwrap().ends_with("../qux"));
    }

    #[test]
    fn find_lockfile_会向上查找() {
        let tmp = TempDir::new().unwrap();
        let a = tmp.path().join("a");
        let b = a.join("b");
        std::fs::create_dir_all(&b).unwrap();
        std::fs::write(tmp.path().join("Cargo.lock"), "x").unwrap();
        assert_eq!(find_lockfile(&b).unwrap(), tmp.path().join("Cargo.lock"));
    }

    #[test]
    fn extract_quoted_strings_支持转义() {
        let out = extract_quoted_strings(r#"["a\\\"b", "c"]"#);
        assert_eq!(out, vec![r#"a\"b"#.to_string(), "c".to_string()]);

        let out = extract_quoted_strings("\"unterminated");
        assert_eq!(out, vec!["unterminated".to_string()]);

        let out = extract_quoted_strings("\"foo\\");
        assert_eq!(out, vec!["foo".to_string()]);
    }

    #[test]
    fn parse_lockfile_能解析多package与依赖列表() {
        let tmp = TempDir::new().unwrap();
        let lock = tmp.path().join("Cargo.lock");
        std::fs::write(
            &lock,
            r#"
[[package]]
name = "root"
version = "0.1.0"
dependencies = [
 "dep1 1.2.3 (registry+https://example)",
 "dep2",
]

[[package]]
name = "dep1"
version = "1.2.3"
source = "registry+https://example"
"#,
        )
        .unwrap();

        let pkgs = parse_lockfile(&lock).unwrap();
        assert_eq!(pkgs.len(), 2);
        assert_eq!(pkgs[0].name, "root");
        assert_eq!(pkgs[0].dependencies.len(), 2);
        assert_eq!(pkgs[1].source.as_deref(), Some("registry+https://example"));
    }

    #[test]
    fn parse_lockfile_覆盖inline_dependencies与异常key() {
        let tmp = TempDir::new().unwrap();
        let lock = tmp.path().join("Cargo.lock");
        std::fs::write(
            &lock,
            r#"
[[package]]
namex = "ignored"
name
name = "root"
versionx = "ignored"
version
version = "0.1.0"
sourcex = "ignored"
source
source = "registry+https://example"
dependencies = ["dep1 1.2.3 (registry+https://example)", "dep2"]
"#,
        )
        .unwrap();

        let pkgs = parse_lockfile(&lock).unwrap();
        assert_eq!(pkgs.len(), 1);
        assert_eq!(pkgs[0].name, "root");
        assert_eq!(pkgs[0].dependencies.len(), 2);
    }

    #[test]
    fn parse_lockfile_覆盖未加引号与dependencies缺少括号_以及空文件() {
        let tmp = TempDir::new().unwrap();
        let lock = tmp.path().join("Cargo.lock");
        std::fs::write(
            &lock,
            r#"
[[package]]
name = root
version = 0.1.0
source = registry+https://example
dependencies = dep1
"#,
        )
        .unwrap();

        let pkgs = parse_lockfile(&lock).unwrap();
        assert_eq!(pkgs.len(), 1);
        assert!(pkgs[0].name.is_empty());
        assert!(pkgs[0].version.is_empty());
        assert!(pkgs[0].source.is_none());
        assert!(pkgs[0].dependencies.is_empty());

        let empty_lock = tmp.path().join("Empty.lock");
        std::fs::write(&empty_lock, "").unwrap();
        let pkgs = parse_lockfile(&empty_lock).unwrap();
        assert!(pkgs.is_empty());
    }

    #[test]
    fn lock_reachable_packages_覆盖root缺失与按依赖遍历() {
        let pkgs = vec![
            LockPackage {
                name: "root".to_string(),
                version: "0.1.0".to_string(),
                source: None,
                dependencies: vec![
                    "".to_string(),
                    "dep1 1.0.0 (registry+xxx)".to_string(),
                    "dep2".to_string(),
                ],
            },
            LockPackage {
                name: "dep1".to_string(),
                version: "1.0.0".to_string(),
                source: None,
                dependencies: vec![],
            },
            LockPackage {
                name: "dep2".to_string(),
                version: "2.0.0".to_string(),
                source: None,
                dependencies: vec!["dep1".to_string()],
            },
            LockPackage {
                name: "dep2".to_string(),
                version: "3.0.0".to_string(),
                source: None,
                dependencies: vec![],
            },
        ];

        let err = lock_reachable_packages(&pkgs, "nope", "0.1.0")
            .unwrap_err()
            .to_string();
        assert!(err.contains("未找到 root package"));

        let mut reachable = lock_reachable_packages(&pkgs, "root", "0.1.0").unwrap();
        reachable.sort();
        assert_eq!(reachable, vec![0, 1, 2, 3]);
    }

    #[test]
    fn parse_lock_dependency_与_semver判断() {
        assert_eq!(
            parse_lock_dependency("dep 1.2.3 (registry+xxx)"),
            ("dep".to_string(), Some("1.2.3".to_string()))
        );
        assert_eq!(
            parse_lock_dependency("dep git+https://example"),
            ("dep".to_string(), None)
        );
        assert!(!looks_like_semver(""));
        assert!(looks_like_semver("1.0.0"));
    }

    #[test]
    fn lock_pkg_depends_on_按首段匹配() {
        let pkg = LockPackage {
            name: "a".to_string(),
            version: "0.1.0".to_string(),
            source: None,
            dependencies: vec!["dep1 1.0.0 (registry+xxx)".to_string(), "dep2".to_string()],
        };
        assert!(lock_pkg_depends_on(&pkg, "dep1"));
        assert!(lock_pkg_depends_on(&pkg, "dep2"));
        assert!(!lock_pkg_depends_on(&pkg, "dep3"));
    }

    #[test]
    fn find_registry_src_dir_与_manifest_quick_mentions_dris() {
        let _lock = ENV_LOCK.lock().unwrap();
        let tmp = TempDir::new().unwrap();
        let _g = with_env_var("CARGO_HOME", tmp.path().to_str().unwrap());

        // src_root 不存在时直接返回 None。
        let none = find_registry_src_dir("pkg", "1.0.0").unwrap();
        assert!(none.is_none());

        let src_root = tmp.path().join("registry").join("src").join("reg1");
        std::fs::create_dir_all(src_root.join("pkg-1.0.0")).unwrap();

        let found = find_registry_src_dir("pkg", "1.0.0").unwrap().unwrap();
        assert!(found.ends_with("pkg-1.0.0"));

        let missing = find_registry_src_dir("nope", "1.0.0").unwrap();
        assert!(missing.is_none());

        let manifest = tmp.path().join("Cargo.toml");
        std::fs::write(&manifest, "name = \"dris\"").unwrap();
        assert!(manifest_quick_mentions_dris(&manifest).unwrap());
        let other = tmp.path().join("Other.toml");
        std::fs::write(&other, "name = \"x\"").unwrap();
        assert!(!manifest_quick_mentions_dris(&other).unwrap());
        let missing_file = tmp.path().join("Missing.toml");
        assert!(!manifest_quick_mentions_dris(&missing_file).unwrap());

        // 覆盖 Guard::drop 的 remove_var 分支（原本不存在该变量）。
        let _g2 = with_env_var("DRIS_BUILD_TEST_TMP_ENV", "1");
    }

    #[test]
    fn find_registry_src_dir_覆盖_home分支() {
        let _lock = ENV_LOCK.lock().unwrap();
        let tmp = TempDir::new().unwrap();

        let old = std::env::var("CARGO_HOME").ok();
        unsafe {
            std::env::remove_var("CARGO_HOME");
        }
        let _home = with_env_var("HOME", tmp.path().to_str().unwrap());

        let src_root = tmp
            .path()
            .join(".cargo")
            .join("registry")
            .join("src")
            .join("reg1");
        std::fs::create_dir_all(src_root.join("pkg-1.0.0")).unwrap();
        let found = find_registry_src_dir("pkg", "1.0.0").unwrap().unwrap();
        assert!(found.ends_with("pkg-1.0.0"));

        // 覆盖 restore_env_var 的两个分支，并在最后恢复原始值。
        restore_env_var(
            "CARGO_HOME",
            Some(tmp.path().join("dummy").to_string_lossy().to_string()),
        );
        restore_env_var("CARGO_HOME", None);
        restore_env_var("CARGO_HOME", old);
    }
}
