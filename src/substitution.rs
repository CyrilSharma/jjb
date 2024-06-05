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

    pub fn bake(&mut self) {
        let mut baked = HashMap::new();
        for (&key, _) in &self.map {
            if baked.contains_key(&key) { continue }
            Self::bake_entry(key, &mut baked, &self.map);
        }
        self.map = baked;
    }

    fn bake_entry(entry: T, baked: &mut HashMap<T, T>, mp: &HashMap<T, T>) -> T {
        let cur = entry;
        if let Some(entry) = mp.get(&cur) {
            let res = Self::bake_entry(*entry, baked, mp);
            baked.insert(cur, res);
            return res;
        }
        return cur;
    }
}