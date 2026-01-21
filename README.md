# dris

`dris` 是一个 Rust 的编译期依赖注入（DI）代码生成器：在 `build.rs` 阶段扫描源码里的 `#[component]` / `#[constructor]`，生成一个静态容器（`OUT_DIR/dris_gen.rs`），让你在运行时用接近手写的开销拿到对象图。

这个仓库是 workspace，核心分三块：

- `dris-rt`：运行时支持（`All` / `Type`，以及宏的 re-export）。
- `dris-macros`：标记用属性宏（本身不改代码，只是给扫描器“打点”）。
- `dris-build`：build.rs 侧扫描 + 生成器（入口 `dris_build::generate()`）。

## 你会得到什么

生成文件里会有一个模块 `dris_gen`，里面是 `Container`：

- `dris_gen::Container::build()`：构建容器（会初始化所有单例）。
- `container.xxx()`：对每个“根组件”生成一个 getter（方法名由类型名转成 snake_case）。

“根组件”的定义：**没有被其他组件当作依赖使用的组件**都会被当成根组件，因此会生成对应 getter。

## 快速开始（最小可用例子）

### 1）在 `Cargo.toml` 里加依赖

```toml
[dependencies]
dris-rt = "0.1.1"

[build-dependencies]
dris-build = "0.1.1"
```

### 2）写 `build.rs`

```rust
fn main() {
    dris_build::generate().unwrap();
}
```

### 3）写组件与构造函数，并引入生成文件

```rust
use dris_rt::{component, constructor};
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

#[component]
pub struct App {
    pub port: u16,
}

impl App {
    #[constructor]
    pub fn new(cfg: Arc<AppConfig>) -> Self {
        Self { port: cfg.port }
    }
}

include!(concat!(env!("OUT_DIR"), "/dris_gen.rs"));

fn main() {
    let container = dris_gen::Container::build();

    // App 没有被任何其他组件依赖，所以它是根组件，会生成 getter：container.app()
    let app = container.app();
    println!("port={}", app.port);
}
```

## 注入与作用域（规则很直白）

### 组件声明

- `#[component]` 标记 `struct` 为组件。
- `#[constructor]` 标记一个 `pub fn` 为构造函数（不允许 `self` 参数，不支持泛型）。

### 作用域

默认是 **Prototype**（每次获取都会新建）。

单例（Singleton）需要显式指定：`#[component(singleton)]` 或 `#[component(scope = "singleton")]`

### 支持的注入形态（四种基本形态）

- `T`：按值注入（通常用于 Prototype）。
- `&T`：借用注入（仅支持 Singleton；适合把依赖当“只读入参”使用）。
- `Rc<T>`：单线程共享所有权（单例）。
- `Arc<T>`：跨线程共享所有权（单例）。

### Trait 注入

- 单实现：可以直接注入 `Arc<dyn Trait>` / `Rc<dyn Trait>`（要求该 trait 只有一个实现）。
- 多实现：用 `dris_rt::All<Arc/Rc<dyn Trait>>` 或 `dris_rt::All<(dris_rt::Type, Arc/Rc<dyn Trait>)>`。

## 天然限制（当前版本明确不做的事）

这些不是 bug，而是为了保持实现简单、行为可预测：

- **跨 crate / 第三方组件的一个前置条件**：如果生成代码里需要直接引用某个依赖 crate 的类型名，那么这个 crate 必须对“生成代码所在的 crate”是可见的（通常意味着你要把它写成直接依赖，或由某个直接依赖 re-export 出来）。否则生成代码可能会因为找不到该 crate 名而无法编译。
- **不做宏展开**：扫描的是源码 AST（`syn`），不要在 `macro_rules!` / 过程宏里“生成”组件或构造函数声明（生成器看不到）。
- **不做类型检查**：只做“结构化扫描 + 生成”，很多错误会在生成阶段或编译阶段暴露。
- **会解析 `src/` 下的所有 `.rs` 文件**：不依赖 `mod` 树，也不走 `cfg` 条件裁剪；因此即使某个文件当前未被引用/未启用 feature，它也必须能被 `syn` 正常解析。
- **不支持在 `cfg`/`cfg_attr` 作用域里声明组件/构造函数**：包括给 `struct`/`impl`/`fn`/`mod` 打 `#[cfg]`，或用 `#[cfg_attr(..., component/constructor)]` 条件启用打标。建议把 `cfg` 放在组件/构造函数的内部实现细节里，不要切换组件类型/构造函数本身。
- **构造函数限制**：`#[constructor]` 必须是 `pub fn`，不支持泛型，不允许 `self` 参数。
- **不支持 `&mut` 注入**：请用内部可变性（`RefCell`/`Mutex`/`RwLock` 等）。
- **Prototype 不支持 `&T`/`Rc<T>`/`Arc<T>` 注入**：为了避免“临时值引用”的语义歧义与误用风险，Prototype 组件只允许按值注入 `T`。
- **多实现聚合只认 `All<...>`**：不支持 `Vec` / `HashMap` / `impl Iterator` 作为多实现注入形态。
- **Rc/Arc 冲突会直接报错**：同一组件如果同时被要求以 `Rc<T>` 与 `Arc<T>` 注入，生成阶段会拒绝（无法保证“同一实例”）。
- **递归扫描 registry 依赖需要 `Cargo.lock`**：没有 lockfile 时会跳过 registry 依赖扫描；并且依赖源码需要已下载（例如 `cargo fetch`）。

## 第三方组件（跨 crate）会不会有扫描问题？

一般不会；前提是你满足下面这几个条件：

- 第三方 crate 的组件是“普通 Rust 源码里直接写出来的”（不是宏展开生成的）。
- 你项目里有 `Cargo.lock`，并且依赖源码在本机可用（通常跑过 `cargo build`/`cargo fetch` 就有）。
- 如果你希望生成代码直接引用该第三方 crate 的类型：把它写成你的**直接依赖**（或由某个直接依赖 re-export），并确保它对外暴露你要用的组件类型（跨 crate 组件通常应为 `pub struct`，构造函数已强制要求 `pub fn`）。

## 如果你想基于 dris 写一个更抽象的框架库（lib），怎么保证扫描稳定？

**递归扫描 registry 依赖最终依赖的是组装端/终端用户项目的 `Cargo.lock`**：

- 框架库（lib）负责：定义 trait、提供组件实现（可选用 `#[component]` / `#[constructor]` 打标）、提供对外 API。
- 组装端（通常是应用的 bin crate）负责：写 `build.rs` 调 `dris_build::generate()`，并 `include!(.../dris_gen.rs)`。

要保证解析/扫描稳定，按下面清单做：

- **组件定义要“直接写在源码里”**：不要把组件/构造函数依赖“宏展开生成”，因为 `dris-build` 不做宏展开。
- **`src/` 里别放不可解析的文件**：由于会解析 `src/**/*.rs` 全量文件，避免在 `src/` 里放半成品/实验代码；需要试验请放在 `examples/` 或 `tests/`（`dris-build` 默认不扫这些目录）。
- **跨 crate 类型要保证可见**：如果生成代码里需要写出 `some_crate::Type`，那么 `some_crate` 最好是组装端的直接依赖（或由直接依赖 re-export）。
- **需要递归扫 registry 依赖时**：确保组装端（终端用户的应用 / workspace 根）有 `Cargo.lock`，并且依赖源码已下载（`cargo fetch`）。

## 格式化规范（避免不同编辑器打架）

- 统一工具链：`rust-toolchain.toml`（Rust `1.92.0`）
- 统一 rustfmt 行为：`rustfmt.toml`
- 统一编辑器行尾/缩进：`.editorconfig`（并用 `.gitattributes` 强制 `LF`）

常用命令：

- `cargo fmt --all`
- `cargo fmt --all --check`
