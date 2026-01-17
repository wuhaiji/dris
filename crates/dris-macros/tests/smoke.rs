use dris_macros::{component, constructor};

#[component(singleton)]
pub struct A;

impl A {
    #[constructor]
    pub fn new() -> Self {
        Self
    }
}

#[component]
pub struct B;

impl B {
    #[constructor(anything)]
    pub fn new(_a: std::sync::Arc<A>) -> Self {
        Self
    }
}

#[test]
fn attribute_macros_可用且不破坏语法() {
    let _ = A::new();
    let _ = B::new(std::sync::Arc::new(A::new()));
}
