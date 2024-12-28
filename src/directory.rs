use crate::symbolmanager::Symbol;
use std::collections::HashMap;

pub struct Dirent<'l> {
    members: HashMap<&'l str, Symbol>,
    methods: HashMap<&'l str, Symbol>,
    parent: Option<Symbol>, // When I decide to support inner classes, we can add classes too.
}

pub struct Directory<'l> {
    table: HashMap<Symbol, Dirent<'l>>,
}
impl<'l> Directory<'l> {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }
    pub fn resolve_field(&self, class: Symbol, name: &'l str) -> Option<&Symbol> {
        self.table
            .get(&class)
            .map(|entry| entry.members.get(name))
            .flatten()
    }
    pub fn resolve_method(&self, class: Symbol, fun: &'l str) -> Option<&Symbol> {
        self.table
            .get(&class)
            .map(|entry| entry.methods.get(fun))
            .flatten()
    }
    pub fn resolve_parent(&self, class: Symbol) -> Option<Symbol> {
        self.table.get(&class).map(|entry| entry.parent).flatten()
    }
    pub fn add_class(
        &mut self,
        class: Symbol,
        methods: HashMap<&'l str, Symbol>,
        members: HashMap<&'l str, Symbol>,
    ) {
        self.table.insert(
            class,
            Dirent {
                methods,
                members,
                parent: None,
            },
        );
    }
    pub fn add_parent(&mut self, class: Symbol, parent: Symbol) {
        let entry = self
            .table
            .get_mut(&class)
            .expect("Trying to add parent to unknown!");
        entry.parent = Some(parent);
    }
    pub fn members(&self, class: Symbol) -> Option<Vec<Symbol>> {
        self.table
            .get(&class)
            .map(|entry| entry.members.values().cloned().collect())
    }
}
