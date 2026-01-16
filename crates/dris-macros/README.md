# dris-macros

`dris-macros` 提供 `dris` 的属性宏：

- `#[component]`：标记一个组件类型（struct）。
- `#[constructor]`：标记组件的构造函数（`pub fn`，不允许 `self` 参数）。

一般情况下你不需要直接依赖它：`dris-rt` 会重新导出这些宏，用户侧通常只需要依赖并 `use dris_rt::{component, constructor};`。
