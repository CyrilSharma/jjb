use crate::ir::Symbol;
pub struct LabelStack<'l> {
    stack: Vec<Option<(&'l str, Symbol)>>
}

impl<'l> LabelStack<'l> {
    pub fn new() -> Self { Self { table: Vec::new() } }
    pub fn find(&self, name: &'l str) -> Option<Symbol> {
        for cur in (0..self.stack.len()).rev() {
            if let Some((cname, csym)) = self.stack[cur] {
                if (cname == name) { return Some(csym) }
            }
        } 
        None
    }

    pub fn push(&mut self, item: Option<(&'l str, Symbol)>) {
        self.stack.push(item)
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }
}