use std::collections::HashMap;
use std::hash::Hash;
use std::fmt::Debug;

#[derive(Clone)]
pub struct Substitution<T: Eq + PartialEq + Hash + Copy + Debug> {
    map: HashMap<T, T>
}

impl<T: Eq + PartialEq + Hash + Copy + Debug> Substitution<T> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn add_subst(&mut self, a: T, b: T) {
        self.map.insert(a, b);
    }

    pub fn subst(&self, a: T) -> T {
        let mut cur = a;
        while let Some(entry) = self.map.get(&cur) {
            cur = *entry;
        }
        cur
    }
}