# dris-build

`dris-build` 是 `dris` 的 build.rs 侧代码生成器：在编译期扫描组件与构造函数，生成 `Container` 到 `OUT_DIR/dris_gen.rs`。

## 用法

生成代码会引用 `dris_rt::All`/`dris_rt::Type` 等路径（直接依赖 `dris-rt` 即可）：

```toml
[dependencies]
dris-rt = "0.1.0"

[build-dependencies]
dris-build = "0.1.0"
```

`build.rs`：

```rust,no_run
fn main() {
    dris_build::generate().unwrap();
}
```

在你的 crate 里引入生成文件：

```rust,ignore
include!(concat!(env!("OUT_DIR"), "/dris_gen.rs"));
```

然后就可以：

```rust,ignore
let container = dris_gen::Container::build();
```

## 扫描范围

- 扫描当前 crate 的 `#[component]`/`#[constructor]`。
- 会尝试递归扫描依赖（path 依赖与 registry 依赖），以支持跨 crate 的组件组合。
