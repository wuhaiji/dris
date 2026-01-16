#![doc = include_str!("../README.md")]

pub use dris_macros::{component, constructor};

// 结构体字段无法写 `impl Iterator`；多组件注入用 Vec newtype 更稳妥。
pub struct All<T>(Vec<T>);

impl<T> All<T> {
    pub fn new(items: Vec<T>) -> Self {
        Self(items)
    }
}

impl<T> std::ops::Deref for All<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> IntoIterator for All<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a All<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type(std::any::TypeId);

impl Type {
    pub fn of<T: 'static>() -> Self {
        Self(std::any::TypeId::of::<T>())
    }
}
