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
