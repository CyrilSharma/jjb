#[derive(Clone)]
enum NameType {
    Reserved(String),
    Known(String)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol { id: usize }
pub struct SymbolMaker { names: Vec<NameType> }
impl SymbolMaker {
    pub fn new() -> Self {
        Self { names: Vec::new() }
    }

    pub fn fresh(&mut self, name: &str) -> Symbol {
        self.names.push(NameType::Known(name.to_string()));
        Symbol { id: self.names.len() - 1 }
    }

    pub fn fresh_reserved(&mut self, name: &str) -> Symbol {
        self.names.push(NameType::Reserved(name.to_string()));
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
}