use std::collections::HashMap;

use crate::ir::*;
use crate::symbolmanager::{Symbol, SymbolManager};

struct State<'l> {
    sm: &'l mut SymbolManager,
    cont_to_break: HashMap<Symbol, Symbol>
}

impl<'l> State<'l> {
    fn new(sm: &'l mut SymbolManager) -> Self {
        State { sm, cont_to_break: HashMap::new() }
    }
}

pub fn flatten(root: &Tree, sm: &mut SymbolManager) -> Box<Tree> {
    let mut state = State::new(sm);
    Box::new(Tree::Program(statement(root, &mut state)))
}

fn statement(root: &Tree, state: &mut State) -> TreeContainer {
    todo!()
}