use std::collections::HashMap;
use crate::ir::ArrayTyp;
use crate::symbolmaker::Symbol;

pub struct TypeTracker {
    array: HashMap<Symbol, ArrayTyp>
}

impl TypeTracker {
    pub fn new() -> Self {
        Self { array: HashMap::new() }
    }
    pub fn type_of(&self, sym: &Symbol) -> Option<&ArrayTyp> {
        self.array.get(sym)
    }
}

