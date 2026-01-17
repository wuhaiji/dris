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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn all_覆盖new_deref_引用迭代_值迭代() {
        let all = All::new(vec![1, 2, 3]);
        assert_eq!(all.len(), 3);
        assert_eq!(all[0], 1);

        let by_ref: Vec<i32> = (&all).into_iter().copied().collect();
        assert_eq!(by_ref, vec![1, 2, 3]);

        let by_value: Vec<i32> = all.into_iter().collect();
        assert_eq!(by_value, vec![1, 2, 3]);

        let empty: All<i32> = All::new(Vec::new());
        assert!((&empty).into_iter().next().is_none());
    }

    #[test]
    fn type_of_可比较可哈希() {
        let a = Type::of::<u8>();
        let b = Type::of::<u8>();
        let c = Type::of::<u16>();
        assert!(a == b);
        assert!(a != c);

        let mut set = HashSet::new();
        assert!(set.insert(a));
        assert!(!set.insert(b));
        assert!(set.insert(c));
    }
}
