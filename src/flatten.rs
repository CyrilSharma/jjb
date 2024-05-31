use std::collections::HashMap;
use crate::graph::Allocator;
use crate::graph::Id;
use crate::{graph, ir::*};
use crate::symbolmanager::{Symbol, SymbolManager};

struct State<'l> {
    sm: &'l mut SymbolManager,
    to_dsym: HashMap<Id, Symbol>,
    to_def: HashMap<Id, Operand>,
    count: usize
}

impl<'l> State<'l> {
    fn new(sm: &'l mut SymbolManager) -> Self {
        State {
            sm,
            to_dsym: HashMap::new(),
            to_def: HashMap::new(),
            count: 0
        }
    }
}

pub fn flatten(tree: Tree, sm: &mut SymbolManager) -> Tree {
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| flatten(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let mut state = State::new(sm);
            let (mut allocator, next_map) = graph::build(f.body);
            flatten_graph(0, &mut allocator, &mut state);
            // print_graph(&allocator.nodes, sm);
            FunDeclaration {
                body: graph::fold(allocator, &next_map),
                ..f
            }
        }),
        Tree::LetC(c) => Tree::LetC(ClassDeclaration {
            methods: c.methods.into_iter().map(|f| flatten(f, sm)).collect(),
            ..c
        }),
        Tree::LetE(_) => todo!(),
        Tree::EntryPoint(e) => Tree::EntryPoint(e),
        _ => panic!("Invalid tree in flatten")
    }
}

fn flatten_graph(id: Id, alloc: &mut Allocator, state: &mut State) -> TreeContainer {
    // This is a reaching definitions analysis.
    // If exactly one definition of a variable reaches a point,
    // We want to inline that definition.
    todo!()
}