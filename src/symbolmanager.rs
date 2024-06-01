use std::collections::HashMap;
use crate::ir::*;

#[derive(Clone, Debug)]
pub enum NameType {
    Reserved(String),
    Known(String)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol { pub id: usize }

#[derive(Default)]
pub struct SymbolManager {
    pub names: Vec<NameType>,
    arraytypes: HashMap<Symbol, ArrayTyp>,
    // Perhaps these should just have names in their types.
    // Then, we wouldn't need HashMaps...
    classtypes: HashMap<Symbol, ClassTyp>,
    enumtypes: HashMap<Symbol, EnumTyp>
}

impl SymbolManager {
    /* ------------------ Name Management ---------------- */
    pub fn new() -> Self { Self { ..Default::default() } }
    pub fn numsyms(&self) -> usize {
        self.names.len()
    }

    pub fn fresh(&mut self, name: &str) -> Symbol {
        self.names.push(NameType::Known(name.to_string()));
        Symbol { id: self.names.len() - 1 }
    }

    pub fn fresh_reserved(&mut self, name: &str) -> Symbol {
        self.names.push(NameType::Reserved(name.to_string()));
        Symbol { id: self.names.len() - 1 }
    }

    pub fn refresh(&mut self, sym: &Symbol) -> Symbol {
        self.names.push(self.names[sym.id].clone());
        Symbol { id: self.names.len() - 1 }
    }

    pub fn name(&self, sym: Symbol) -> &str {
        match &self.names[sym.id as usize] {
            NameType::Known(s) => s,
            NameType::Reserved(s) => s
        }
    }

    pub fn uname(&self, sym: Symbol) -> String {
        match self.names[sym.id].clone() {
            NameType::Known(s) => format!("{}_{}", s, sym.id),
            NameType::Reserved(s) => s
        }
    }

    /* ------------------ Type Management ---------------- */

    // Add in logic to deduplicate arraytypes.
    pub fn fresh_array(&mut self, typ: ArrayTyp) -> Symbol {
        let sym = self.fresh("a");
        self.arraytypes.insert(sym, typ).map(|_| panic!(
            "Symbol {} already had a type assigned!", self.uname(sym)
        ));
        sym
    }

    // Cannot use "fresh" semantics, because of mutual recursion.
    pub fn add_class(&mut self, sym: Symbol, typ: ClassTyp) {
        self.classtypes.insert(sym, typ).map(|_| panic!(
            "Symbol {} already had a type assigned!", self.uname(sym)
        ));
    }

    pub fn arraytyp(&self, sym: Symbol) -> Option<&ArrayTyp> {
        self.arraytypes.get(&sym)
    }

    pub fn classtyp(&self, sym: Symbol) -> Option<&ClassTyp> {
        self.classtypes.get(&sym)
    }
}