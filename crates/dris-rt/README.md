# dris-rt

`dris-rt` 是 `dris` 依赖注入（DI）代码生成器的运行时支持库，配合 `dris-macros` 与 `dris-build` 使用。

## 快速开始

```toml
[dependencies]
dris-rt = "0.1.1"

[build-dependencies]
dris-build = "0.1.1"
```

在 `build.rs` 中生成容器代码：

```rust,ignore
fn main() {
    dris_build::generate().unwrap();
}
```

声明组件与构造函数，并引入生成文件：

```rust,ignore
use dris_rt::{component, constructor};

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

include!(concat!(env!("OUT_DIR"), "/dris_gen.rs"));

fn main() {
    let container = dris_gen::Container::build();
    let cfg = container.app_config(); // root getter
    println!("port={}", cfg.port);
}
```

## 注入类型（四种基本形态）

支持：

- `T`：按值注入（通常用于 Prototype）。
- `&T`：借用注入（零开销；仅支持 Singleton）。
- `Rc<T>`：单线程共享所有权。
- `Arc<T>`：跨线程共享所有权。

### 单例持有方式的推断

生成器会按“实际被注入的类型”推断单例在 `Container` 内部的持有方式：

- 只用到 `&T` → `Container` 内存 `T`（读路径零开销）。
- 某处需要 `Rc<T>` → `Container` 内存 `Rc<T>`。
- 某处需要 `Arc<T>` → `Container` 内存 `Arc<T>`。
- 同一类型同时要求 `Rc<T>` 与 `Arc<T>` → **直接报错**（无法保证同一实例同时以两种指针形式存在）。

## 作用域（Prototype / Singleton）

- `Prototype`：每次获取都会构建新实例。
- `Singleton`：容器构建时只构建一次并复用。

常用方式：

- `#[component(singleton)]` 强制单例。

## 约束与建议

- `dris-build` 不做 `cfg` 条件裁剪（会解析 `src/**/*.rs`），因此当前版本**不支持**在 `cfg`/`cfg_attr` 作用域里声明组件/构造函数（包括 `#[cfg_attr(..., component/constructor)]`）。建议把 `cfg` 放在实现细节里，不要切换组件类型/构造函数本身。
- `Arc/Rc<dyn Trait>` 注入：当且仅当某个 trait 只有一个实现时可直接注入；多个实现请用 `dris_rt::All<Arc/Rc<dyn Trait>>`（或 `dris_rt::All<(dris_rt::Type, Arc/Rc<dyn Trait>)>`）。
