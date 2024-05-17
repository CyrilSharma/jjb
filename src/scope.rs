use std::collections::HashMap;
use crate::symbolmaker::Symbol;

pub struct Scope<'l> {
    table: Vec<HashMap<&'l str, Symbol>>
}

impl<'l> Scope<'l> {
    pub fn new() -> Self { Self { table: vec![HashMap::new()] } }
    pub fn find(&self, name: &'l str) -> Option<Symbol> {
        for cur in (0..self.table.len()).rev() {
            if let Some(res) = self.table[cur].get(name) {
                return Some(*res);
            }
        } 
        None
    }

    pub fn scope_in(&mut self) {
        self.table.push(HashMap::new());
    }

    pub fn scope_out(&mut self) {
        self.table.pop();
    }

    pub fn insert(&mut self, name: &'l str, sym: Symbol) {
        let len = self.table.len();
        if let Some(back) = self.table.get_mut(len - 1) {
            if back.insert(name, sym).is_some() {
                panic!("Overwriting ({}) with {:?}", name, sym)
            }
            return;
        }
        panic!("Empty Symbol Table received: {}\n", name)
    }
}