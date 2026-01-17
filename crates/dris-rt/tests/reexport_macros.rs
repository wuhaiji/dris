use dris_rt::{component, constructor};

#[component(singleton)]
pub struct A;

impl A {
    #[constructor]
    pub fn new() -> Self {
        Self
    }
}

#[test]
fn reexported_attribute_macros_可直接使用() {
    let _ = A::new();
}
