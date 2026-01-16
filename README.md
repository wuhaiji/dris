# dris

`dris` 是一个面向 Rust 的依赖注入（DI）代码生成器：在编译期扫描 `#[component]`/`#[constructor]`，生成容器代码，从而在运行时以最小开销获取对象图。

本仓库以 workspace 形式组织，核心分为三个 crate：

- `dris-rt`：运行时支持库（用户代码主要依赖它；在代码里通过 `dris_rt::...` 引用）。
- `dris-macros`：属性宏（`#[component]`/`#[constructor]`），一般由 `dris-rt` 重新导出。
- `dris-build`：build.rs 侧代码生成器（`dris_build::generate()`）。

## 格式化规范

- 统一工具链：`rust-toolchain.toml`（Rust `1.92.0`）
- 统一 rustfmt 行为：`rustfmt.toml`
- 统一编辑器行尾/缩进：`.editorconfig`（并用 `.gitattributes` 强制 `LF`）

常用命令：

- `cargo fmt --all`
- `cargo fmt --all --check`
