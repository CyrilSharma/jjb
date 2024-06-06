use std::collections::HashMap;
use crate::substitution::Substitution;
use crate::symbolmanager::SymbolManager;
use crate::{cssa, ir::*};
use crate::symbolmanager::Symbol;
use fixedbitset::FixedBitSet;
use crate::graph::{self, print_graph, Allocator, CfgNode, Id};
use crate::ir::{ExprTree, Operand};
use std::collections::HashSet;

struct State<'l> {
    stack: HashMap<Symbol, Vec<Symbol>>,
    sm: &'l mut SymbolManager,
}

impl<'l> State<'l> {
    fn add_name(&mut self, sym: Symbol, nname: Symbol, push_count: &mut HashMap<Symbol, usize>) {
        // println!("==> added {} -> {}", self.sm.uname(sym), self.sm.uname(nname));
        let stk = self.stack.entry(sym).or_insert(Vec::new());
        stk.push(nname);
        let entry = push_count.entry(sym).or_insert(0);
        *entry += 1;
    }
    fn get_name(&mut self, sym: Symbol) -> Symbol {
        *self.stack.get(&sym).unwrap_or(&vec![sym])
            .last().expect(&format!(
                "Symbol {} was undefined",
                self.sm.uname(sym)
            )
        )
    }
    fn restore_stack(&mut self, mut push_count: HashMap<Symbol, usize>) {
        let mut dead = Vec::new();
        for (sym, stack) in self.stack.iter_mut() {
            if let Some(mut amt) = push_count.remove(&sym) {
                stack.truncate(stack.len() - amt);
                // while amt != 0 {
                //     let nname = stack.pop().expect("");
                //     println!("==> removed {} -> {}", self.sm.uname(*sym), self.sm.uname(nname));
                //     amt -= 1;
                // }
                if stack.len() == 0 { dead.push(*sym) }
            }
        }
        for sym in dead { self.stack.remove(&sym); }
    }
}

pub fn transform(tree: Tree, sm: &mut SymbolManager) -> Tree {
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| transform(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let (mut allocator, next_map) = graph::build(f.body);
            transform_graph(&mut allocator, sm);
            // print_graph(&allocator.nodes, sm);
            FunDeclaration {
                body: graph::fold(allocator, &next_map),
                ..f
            }
        }),
        Tree::LetC(c) => Tree::LetC(ClassDeclaration {
            methods: c.methods.into_iter().map(|f| transform(f, sm)).collect(),
            ..c
        }),
        Tree::LetE(_) => todo!(),
        Tree::EntryPoint(e) => Tree::EntryPoint(e),
        _ => panic!("Invalid tree in transform")
    }
}

// https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/5/
pub fn transform_graph(alloc: &mut Allocator, sm: &mut SymbolManager) {
    let doms = graph::dominance(alloc);
    let dominance_frontier = graph::dominance_frontier(&doms, alloc);
    let mut idom: Vec<Vec<usize>> = vec![Vec::new(); alloc.len()];
    for (i, d) in doms.into_iter().enumerate() { if d != i { idom[d].push(i) } }
    let (vars, typs) = stat(alloc);
    for (v, mut block_set) in vars {
        let mut blocks: Vec<Id> = block_set.clone().into_iter().collect();
        while let Some(vassigned) = blocks.pop() {
            for block in dominance_frontier[vassigned].ones() {
                insert_phi(v, typs[&v], block, alloc);
                if !block_set.contains(&block) {
                    block_set.insert(block);
                    blocks.push(block);
                }
            }
        }
    }
    
    let mut state = State { stack: HashMap::new(), sm };
    rename(0, &mut state, alloc, &typs, &idom);
}

pub fn stat(alloc: &Allocator) -> (
    HashMap<Symbol, HashSet<usize>>,
    HashMap<Symbol, Typ>,
) {
    let mut mp = HashMap::new();
    let mut typ = HashMap::new();
    for id in 0..alloc.len() {
        let content = &alloc.grab(id).content;
        for tree in content {
            match tree {
                Tree::LetP(p) => if let Some(pname) = p.name {
                    let id_set = mp.entry(pname).or_insert(HashSet::new());
                    id_set.insert(id);
                    if p.typ != Typ::Void { typ.insert(pname, p.typ); }
                },
                Tree::Block(_) | Tree::Switch(_) | Tree::LetI(_) | 
                Tree::Loop(_) | Tree::If(_) | Tree::Return(_) |
                Tree::Break(_) | Tree::Continue(_) | Tree::EntryPoint(_) => (),
                _ => panic!("Invalid Tree Type in SSA")
            }
        }
    }
    (mp, typ)
}

pub fn insert_phi(v: Symbol, typ: Typ, block: Id, alloc: &mut Allocator) {
    fn phi(v: Symbol, typ: Typ) -> Tree {
        Tree::LetP(PrimStatement {
            name: Some(v), typ,
            exp: Some(Operand::T(ExprTree {
                // The first argument is reserved for tracking the original variable.
                op: Operation::Phi, args: vec![Operand::V(v)],
            })),
        })
    }
    
    fn has_phi(head: &mut Tree, v: Symbol) -> bool {
        if let Tree::LetP(PrimStatement {
            exp: Some(Operand::T(ExprTree { op: Operation::Phi, args })),
            ..
        }) = head {
            match args[0] {
                Operand::V(sym) => if sym != v { return false },
                _ => panic!("Invalid Phi")
            }
            return true;
        }
        return false;
    }

    let node = alloc.grab_mut(block);
    if let Some(head) = node.content.iter_mut().next() {
        if has_phi(head, v) { return }
    }
    node.content.push_front(phi(v, typ));
}

// https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/5/
fn rename(id: Id, state: &mut State, alloc: &mut Allocator,
            typs: &HashMap<Symbol, Typ>, idom: &Vec<Vec<Id>>) {
    let mut push_count = HashMap::new();
    let content = alloc.grab_mut(id).content.iter_mut();
    for tree in content {
        match tree {
            Tree::LetP(p) => {
                match p.exp.as_mut() {
                    Some(op) => rename_operand(op, state),
                    _ => ()
                }
                if let Some(pname) = p.name {
                    let nname = state.sm.refresh(&pname);
                    state.add_name(pname, nname, &mut push_count);
                    p.name = Some(nname);
                    p.typ = typs[&pname];
                }
            },
            Tree::Switch(s) => rename_operand(&mut s.arg, state),
            Tree::If(i) => rename_operand(&mut i.cond, state),
            Tree::Return(r) => match r.val.as_mut() {
                Some(op) => rename_operand(op, state),
                _ => ()
            },
            Tree::Loop(l) => rename_operand(&mut l.cond, state),
            Tree::LetI(_) | Tree::Break(_) | Tree::Block(_) |
            Tree::Continue(_) | Tree::EntryPoint(_) => (),
            Tree::Try(_) => todo!(),
            _ => panic!("Invalid Tree Type in SSA")
        }
    }

    // https://users.rust-lang.org/t/need-help-with-mutable-and-immutable-borrow-of-self/68811/2
    let children = std::mem::take(&mut alloc.grab_mut(id).children);
    for &child in &children { // For Each Successor
        let mut iter = alloc.grab_mut(child).content.iter_mut();
        while let Some(Tree::LetP(PrimStatement {
            exp: Some(Operand::T(ExprTree { op: Operation::Phi, args })),
            name: Some(nm),
            ..
        })) = iter.next() { // For Each Phi Node containing a name we changed.
            if let Operand::V(sym) = args[0] {
                if !state.stack.contains_key(&sym) { continue }
                if *nm == state.get_name(sym) { continue }
                args.push(Operand::V(state.get_name(sym)));
            } else {
                panic!("Invalid Phi Operation!")
            }
        }
    }
    alloc.grab_mut(id).children = children;
    for &child in &idom[id] {
        rename(child, state, alloc, typs, idom)
    }
    state.restore_stack(push_count);
}

fn rename_operand(op: &mut Operand, state: &mut State) {
    match op {
        Operand::This(_) | Operand::Super(_) |
        Operand::C(_) | Operand::Tp(_) => (),
        Operand::V(sym) => {
            *sym = state.get_name(*sym);
        },
        Operand::T(ExprTree { op, args }) => match op {
            Operation::InstanceOf | Operation::Throw => todo!(),
            Operation::ArrayNew => rename_operand(&mut args[1], state),
            Operation::Phi => args[1..].iter_mut().for_each(|a| rename_operand(a, state)),
            Operation::InvokeVirtual => {
                rename_operand(&mut args[0], state);
                args[2..].iter_mut().for_each(|a| rename_operand(a, state));
            },
            Operation::InvokeStatic => todo!(),
            Operation::Access => rename_operand(&mut args[0], state),
            _ => args.iter_mut().for_each(|a| rename_operand(a, state))
        }
        Operand::A(aexp) => match aexp {
            ArrayExpression::Empty(v) => v.as_mut().iter_mut().for_each(|a| rename_operand(a, state)),
            ArrayExpression::Initializer(aexp) => rename_array_initializer(aexp, state)
        }
    }
}

fn rename_array_initializer(ops: &mut ArrayInitializer, state: &mut State) {
    ops.iter_mut().for_each(|item| match item.as_mut() {
        ElementInitializer::Expr(exp) => rename_operand(exp, state),
        ElementInitializer::ArrayInitializer(a) => rename_array_initializer(a, state)
    });
}

pub fn verify(alloc: &Allocator, sm: &SymbolManager) -> Result<(), String> {
    let mut defs = HashSet::new();
    for id in 0..alloc.len() {
        let content = &alloc.grab(id).content;
        for tree in content {
            match tree {
                Tree::LetP(p) => if let Some(pname) = p.name {
                    if defs.contains(&p.name) {
                        return Err(format!("Variable {:?} is defined twice!", sm.uname(pname)));
                    } else {
                        defs.insert(p.name);
                    }
                },
                _ => ()
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use tree_sitter::Parser;
    use crate::ir::*;
    use crate::converter::convert;
    use crate::hoist::hoist;
    use crate::optimizer::optimize;
    use crate::parameters::Parameters;
    use crate::printer::print;
    use crate::ssa::transform;
    use crate::symbolmanager::SymbolManager;
    use crate::typeinfer::typeinfer;
    use crate::graph::print_graph;

    #[test]
    pub fn f() {
        let text = r#"
        class Test {
            public static void main(String[] args_2) {
                int count = 0;
                do {
                    count++;
                } while (count < 100);
                System.out.println(count);
            }
          }
        "#;

        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
        let tree = parser.parse(text, None).unwrap();
        let mut sm = SymbolManager::new();
        let class_name = "Test".to_string();
        let params = Parameters { entry_class: class_name, entry_name: "main".to_string() };
        let mut ast = convert(tree.root_node(), text.as_bytes(), &params, &mut sm);
        ast = hoist(ast.as_ref(), &mut sm);
        typeinfer(ast.as_mut(), &mut sm);
        ast = Box::new(super::transform(*ast, &mut sm));
        ast = optimize(ast.as_ref(), &mut sm);
        // print(&ast, &sm);
        // ast = Box::new(super::revert(*ast, &mut sm));
        // print(&ast, &sm);
        assert!(true)
    }
}