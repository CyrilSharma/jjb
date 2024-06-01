use std::collections::HashMap;
use crate::substitution::Substitution;
use crate::symbolmanager::SymbolManager;
use crate::ir::*;
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
    let (doms, dominance_frontier) = dominance(alloc);
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

pub fn dominance(alloc: &Allocator) -> (Vec<Id>, Vec<FixedBitSet>) {
    fn dfs(node: Id, v: &mut Vec<Id>, alloc: &Allocator, visited: &mut Vec<bool>) {
        visited[node] = true;
        let cfg = alloc.grab(node);
        for &child in &cfg.children {
            if visited[child] { continue }
            dfs(child, v, alloc, visited)
        }
        v.push(node);
    }

    // https://web.archive.org/web/20210422111834/https://www.cs.rice.edu/~keith/EMBED/dom.pdf
    fn intersect(a: Id, b: Id, doms: &Vec<usize>, index: &Vec<usize>) -> Id {
        let mut fingera = a;
        let mut fingerb = b;
        while fingera != fingerb {
            while index[fingera] < index[fingerb] {
                fingera = doms[fingera];
            }
            while index[fingerb] < index[fingera] {
                fingerb = doms[fingerb];
            }
        }
        return fingera;
    }

    // N^2 but with incredible constant factor.
    let undefined: usize = alloc.len(); 
    let (mut postorder, mut index) = (Vec::new(), vec![undefined; alloc.len()]);
    postorder.reserve(alloc.len());
    dfs(0, &mut postorder, alloc, &mut vec![false; alloc.len()]);
    for (i, p) in postorder.iter().enumerate() { index[*p] = i; }
    let mut doms = vec![undefined; alloc.len()];
    doms[0] = 0;
    loop {
        let mut changed = false;
        for &node in postorder.iter().rev().skip(1) {
            let preds = &alloc.grab(node).preds;
            let mut new_idom = undefined;
            for &p in preds {
                if doms[p] == undefined { continue }
                if new_idom == undefined { new_idom = p; continue }
                new_idom = intersect(p, new_idom, &doms, &index);
            }
            if doms[node] != new_idom {
                doms[node] = new_idom;
                changed = true;
            }
        }
        if !changed { break }
    }

    let mut df: Vec<FixedBitSet> = vec![
        FixedBitSet::with_capacity(alloc.len());
        alloc.len()
    ];
    for b in 0..alloc.len() {
        let preds = &alloc.grab(b).preds;
        if preds.len() < 2 { continue; } 
        for &p in preds {
            let mut runner = p;
            while runner != doms[b] {
                df[runner].set(b, true);
                runner = doms[runner];
            }
        }
    }
    (doms, df)
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

pub fn revert(tree: Tree, sm: &mut SymbolManager) -> Tree {
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| revert(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let (mut allocator, next_map) = graph::build(f.body);
            print_graph(&allocator.nodes, sm);
            to_cssa(&mut allocator, sm);
            print_graph(&allocator.nodes, sm);
            out_of_cssa(&mut allocator, sm);
            print_graph(&allocator.nodes, sm);
            fix_declarations(&mut allocator);
            FunDeclaration {
                body: graph::fold(allocator, &next_map),
                ..f
            }
        }),
        Tree::LetC(c) => Tree::LetC(ClassDeclaration {
            methods: c.methods.into_iter().map(|f| revert(f, sm)).collect(),
            ..c
        }),
        Tree::LetE(_) => todo!(),
        Tree::EntryPoint(e) => Tree::EntryPoint(e),
        _ => panic!("Invalid tree in revert")
    }
}

// TODO a "redefine" phase, so we have declarations in the proper places.
pub fn to_cssa(alloc: &mut Allocator, sm: &mut SymbolManager) {
    fn insert_copy(
        pred: usize,
        assign: Tree,
        nodes: &mut Vec<CfgNode>,
    ) {
        if let Some(last_element) = nodes[pred].content.pop_back() {
            let insert_back = match &last_element {
                Tree::Switch(_) | Tree::If(_) | Tree::Loop(_) |
                Tree::Block(_) | Tree::Continue(_) | Tree::Break(_) => false,
                Tree::Return(_) => panic!("Invalid return transition"),
                Tree::Try(_) => todo!(),
                _ => true
            };
            if insert_back {
                nodes[pred].content.push_back(last_element);
                nodes[pred].content.push_back(assign);
            } else {
                nodes[pred].content.push_back(assign);
                nodes[pred].content.push_back(last_element);
            }
        } else {
            nodes[pred].content.push_back(assign);
        }
    }

    fn process_phi(
        el: Tree,
        mp: &HashMap<Symbol, HashSet<Id>>,
        nodes: &mut Vec<CfgNode>,
        sm: &mut SymbolManager
    ) -> (Tree, Tree) {
        let (name, typ, ops) = match el {
            Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, args })),
                name: Some(nm),
                typ,
            }) => (nm, typ, args),
            _ => panic!()
        };
        let old_args: Vec<Symbol> = ops.into_iter().map(|op| match op {
            Operand::V(sym) => sym, _ => panic!("")
        }).collect();
        let new_args: Vec<Symbol> = old_args.iter().map(|sym| sm.refresh(sym)).collect();
        for (&old_sym, &new_sym) in old_args[1..].iter().zip(new_args[1..].iter()) {
            let assign = Tree::LetP(PrimStatement {
                name: Some(new_sym),
                exp: Some(Operand::V(old_sym)),
                typ,
            });
            let pred = mp.get(&old_sym)
                .expect(&format!("Variable {} not defined.", sm.uname(old_sym)))
                .iter().next().expect("Invalid Stat Run.");
            insert_copy(*pred, assign, nodes);
        }
        let wrapped_args = new_args.into_iter().map(|sym| Operand::V(sym)).collect();
        let assign_name = sm.refresh(&name);
        let res_phi = Tree::LetP(PrimStatement {
            exp: Some(Operand::T(ExprTree { op: Operation::Phi, args: wrapped_args })),
            name: Some(assign_name),
            typ,
        });
        let res_assign = Tree::LetP(PrimStatement {
            exp: Some(Operand::V(assign_name)),
            name: Some(name),
            typ,
        });
        (res_phi, res_assign)
    }

    let (mp, _) = stat(alloc);
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        let mut phis = TreeContainer::new();
        let mut assigns = TreeContainer::new();
        while let Some(cur) = nodes[id].content.pop_front() {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, .. })),
                ..
            }) = cur {
                let (phi, assign) = process_phi(cur, &mp, nodes, sm);
                phis.push_back(phi);
                assigns.push_back(assign);
            } else {
                nodes[id].content.push_front(cur);
                let content = std::mem::take(&mut nodes[id].content);
                nodes[id].content = phis + assigns + content;
                break;
            }
        }
    }
}

pub fn coalesce_cssa(alloc: &mut Allocator, sm: &mut SymbolManager) {
    todo!();
}

pub fn out_of_cssa(alloc: &mut Allocator, sm: &SymbolManager) {
    let mut subst: Substitution<Symbol> = Substitution::new();
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        for cur in &nodes[id].content {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, args })),
                name: Some(nm),
                ..
            }) = cur {
                args.iter().for_each(|arg| match arg {
                    Operand::V(sym) => subst.add_subst(*sym, *nm),
                    _ => panic!("")
                });
            }
        }
    }

    for id in 0..nodes.len() {
        let mut res = TreeContainer::new();
        while let Some(mut cur) = nodes[id].content.pop_front() {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, .. })),
                ..
            }) = cur {} else {
                substtree(&mut cur, &mut subst);
                res.push_back(cur);
            }
        }
        nodes[id].content = res;
    }
}


fn substtree(root: &mut Tree, state: &mut Substitution<Symbol>) {
    match root {
        Tree::LetP(ref mut p) => { 
            p.exp.as_mut().map(|e| substop(e, state));
            p.name.as_mut().map(|e| *e = state.subst(*e));
        },
        Tree::Switch(ref mut s) => substop(&mut s.arg, state),
        Tree::Loop(ref mut l) => substop(&mut l.cond, state),
        Tree::If(ref mut i) => substop(&mut i.cond, state),
        Tree::Return(r) => { r.val.as_mut().map(|e| substop(e, state)); },
        Tree::Block(_) | Tree::Break(_) | Tree::Continue(_) => (),
        _ => panic!("Invalid Tree")
    }
}

fn substop(op: &mut Operand, state: &Substitution<Symbol>) {
    use ArrayExpression as A;
    match op {
        Operand::This(c) => (),
        Operand::Super(s) => (),
        Operand::V(ref mut sym) => { *sym = state.subst(*sym); }
        Operand::T(ExprTree { op, args }) => args.iter_mut().for_each(|a| substop(a, state)),
        Operand::A(A::Empty(a)) => a.iter_mut().for_each(|a| substop(a, state)),
        Operand::A(A::Initializer(a)) => substarray(a, state),
        _ => ()
    }
}

fn substarray(a: &mut ArrayInitializer, state: &Substitution<Symbol>) {
    use ElementInitializer as E;
    a.iter_mut().for_each(|e| match e.as_mut() {
        E::Expr(op) => substop(op, state),
        E::ArrayInitializer(c) => substarray(c, state)
    })
}


// Might not need any of this on second thought...
fn liveout(alloc: &mut Allocator, sm: &SymbolManager) -> Vec<HashSet<Symbol>> {
    fn dfs(node: Id, v: &mut Vec<Id>, alloc: &Allocator, visited: &mut Vec<bool>) {
        visited[node] = true;
        let cfg = alloc.grab(node);
        for &child in &cfg.children {
            if visited[child] { continue }
            dfs(child, v, alloc, visited)
        }
        v.push(node);
    }

    fn merge(a: &mut HashSet<Symbol>, b: &HashSet<Symbol>) {
        for el in b { a.insert(*el); }
    }

    // A variable is live out of a block if it is live-out of it's successors,
    // OR if it is used by its successors.
    let mut postorder = Vec::new();
    postorder.reserve(alloc.len());
    dfs(0, &mut postorder, alloc, &mut vec![false; alloc.len()]);
    let mut live = vec![HashSet::new(); alloc.len()];
    let used_by_block = used(alloc, sm);
    loop {
        let mut changed = false;
        for &node in postorder.iter() {
            let children = &alloc.grab(node).children;
            let mut new_live = HashSet::new();
            for &child in children {
                merge(&mut new_live, &used_by_block[child]);
                merge(&mut new_live, &live[child]);
            }
            if live[node] != new_live {
                live[node] = new_live;
                changed = true;
            }
        }
        if !changed { break }
    }
    live
}

fn used(alloc: &mut Allocator, sm: &SymbolManager) -> Vec<HashSet<Symbol>> {
    let mut res = vec![HashSet::new(); alloc.len()];
    for (i, node) in alloc.nodes.iter().enumerate() {
        for item in &node.content {
            match item {
                Tree::LetP(p) => if let Some(e) = p.exp.as_ref() { useop(e,  &mut res[i], sm) }
                Tree::Block(_) | Tree::Break(_) | Tree::Continue(_) => (),
                Tree::Switch(s) => useop(&s.arg,  &mut res[i], sm),
                Tree::Loop(l) => useop(&l.cond,  &mut res[i], sm),
                Tree::If(l) => useop(&l.cond,  &mut res[i], sm),
                Tree::Return(r) => if let Some(e) = r.val.as_ref() { useop(e,  &mut res[i], sm) },
                Tree::Try(_) => todo!(),
                _ => panic!("Invalid node in Block")
            }
        }
    }
    res
}

// I hate having to write this every time.
fn useop(e: &Operand, mp: &mut HashSet<Symbol>, sm: &SymbolManager) {
    match e {
        Operand::This(_) | Operand::Super(_) |
        Operand::C(_) | Operand::Tp(_) => (),
        Operand::V(sym) => { mp.insert(*sym); },
        Operand::T(ExprTree { args, op: Operation::New }) => {
            args[1..].iter().for_each(|e| useop(e, mp, sm))
        }
        Operand::T(ExprTree { args, op: Operation::InvokeVirtual | Operation::InvokeStatic }) => {
            useop(&args[0], mp, sm);
            args[2..].iter().for_each(|e| useop(e, mp, sm))
        },
        Operand::T(ExprTree { args, .. }) => 
            args.iter().for_each(|e| useop(e, mp, sm)),
        Operand::A(a) => match a {
            ArrayExpression::Empty(v) => v.iter().for_each(|e| useop(e, mp, sm)),
            ArrayExpression::Initializer(a) => use_array(a, mp, sm)
        }
    }
}

fn use_array(a: &ArrayInitializer, mp: &mut HashSet<Symbol>, sm: &SymbolManager) {
    for item in a {
        match item.as_ref() {
            ElementInitializer::Expr(e) => useop(e, mp, sm),
            ElementInitializer::ArrayInitializer(child) => use_array(child, mp, sm)
        }
    }
}

fn fix_declarations(alloc: &mut Allocator) {
    let mut decl = HashMap::new();
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        nodes[id].content.retain(|item| {
            match item {
                Tree::LetP(PrimStatement { name: Some(sym), typ, exp }) 
                if *typ != Typ::Void => {
                    decl.insert(*sym, *typ);
                    *typ = Typ::Void;
                    exp.is_some()
                },
                _ => true
            }
        })
    }
    
    'top: for (name, tp) in decl {
        for tree in nodes[0].content.iter_mut() {
            match tree {
                Tree::LetP(PrimStatement { name: Some(n), typ, .. })
                if name == *n => { *typ = tp; continue 'top; },
                _ => ()
            }
        }
        nodes[0].content.push_front(Tree::LetP({
            let lit = defaultLit(tp);
            PrimStatement {
                name: Some(name), typ: tp,
                exp: Some(Operand::C(lit))
            }
        }));
    }
}

fn defaultLit(typ: Typ) -> Literal {
    match typ {
        Typ::Unknown => panic!("Unknown type!"),
        Typ::Void => panic!("Void type!"),
        Typ::Bool => Literal::Bool(false),
        Typ::Char => Literal::Char('\0'),
        Typ::Byte => Literal::Byte(0),
        Typ::Int => Literal::Int(0),
        Typ::Short => Literal::Short(0),
        Typ::Long => Literal::Long(0),
        Typ::Float => Literal::Float(0.0),
        Typ::Double => Literal::Double(0.0),
        Typ::Str => Literal::Null,
        Typ::Array(_) => Literal::Null,
        Typ::Class(_) => Literal::Null
    }
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
        ast = Box::new(super::revert(*ast, &mut sm));
        // print(&ast, &sm);
        assert!(true)
    }
}