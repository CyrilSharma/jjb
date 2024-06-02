use std::collections::HashMap;
use std::collections::HashSet;

use crate::ir::*;
use crate::graph::*;
use crate::substitution::Substitution;
use crate::symbolmanager::{Symbol, SymbolManager};
pub fn transform(alloc: &mut Allocator, sm: &mut SymbolManager) {
    fn insert_copy(pred: usize, assign: Tree, nodes: &mut Vec<CfgNode>) {
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
    ) -> (Tree, Vec<Operand>) {
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
        (res_phi, vec![Operand::V(name), Operand::V(assign_name), Operand::Tp(typ)])
    }

    let (mp, _) = crate::ssa::stat(alloc);
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        let mut phis = TreeContainer::new();
        let mut pcopy_args = Vec::new();
        while let Some(cur) = nodes[id].content.pop_front() {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, .. })),
                ..
            }) = cur {
                let (phi, assign) = process_phi(cur, &mp, nodes, sm);
                phis.push_back(phi);
                pcopy_args.extend(assign);
            } else {
                nodes[id].content.push_front(cur);
                let content = std::mem::take(&mut nodes[id].content);
                let assign = Tree::LetP(PrimStatement {
                    name: None,
                    typ: Typ::Void,
                    exp: Some(Operand::T(ExprTree {
                        op: Operation::Pcopy,
                        args: pcopy_args
                    }))
                });
                phis.push_back(assign);
                nodes[id].content = phis + content;
                break;
            }
        }
    }
}

pub fn coalesce(alloc: &mut Allocator, sm: &mut SymbolManager) {
    let mut interference = interference(alloc, sm);
    let nodes = &mut alloc.nodes;
    let mut subst = Substitution::new();
    // I'll implement fixed point iteration one day
    for iter in 0..10 {
        for id in 0..nodes.len() {
            let list = std::mem::take(&mut nodes[id].content);
            nodes[id].content = coalesce_list(list, &mut subst, &mut interference)
        }
    }

    fn coalesce_list(
        mut list: TreeContainer,
        subst: &mut Substitution<Symbol>,
        interference: &mut HashMap<Symbol, HashSet<Symbol>>
    ) -> TreeContainer {
        let mut res = TreeContainer::new();
        while let Some(item) = list.pop_front() {
            match item {
                Tree::LetP(p) => {
                    let tree = coalesce_prim(p, subst, interference);
                    tree.map(|t| res.push_back(t));
                },
                mut other => {
                    substtree(&mut other, subst);
                    res.push_back(other);
                }
            }
        }
        res
    }

    fn coalesce_prim(
        p: PrimStatement,
        subst: &mut Substitution<Symbol>,
        interference: &mut HashMap<Symbol, HashSet<Symbol>>
    ) -> Option<Tree> {
        match (p.name, p.exp) {
            (Some(n), None) => {
                let nn = subst.subst(n);
                Some(Tree::LetP(PrimStatement {
                    name: Some(nn),
                    typ: p.typ,
                    exp: None
                }))
            },
            (Some(n), Some(mut e)) => {
                let nn = subst.subst(n);
                substop(&mut e, &subst);
                if let Operand::V(sym) = e {
                    if !interference[&nn].contains(&sym) {
                        for item in std::mem::take(interference.get_mut(&sym).expect("")) {
                            let entry = interference.entry(nn).or_insert(HashSet::new());
                            entry.insert(item);
                        }
                        subst.add_subst(nn, sym);
                        return None;
                    }
                }
                return Some(Tree::LetP(PrimStatement {
                    name: Some(nn),
                    typ: p.typ,
                    exp: Some(e)
                }))
            }
            (None, Some(mut e)) => {
                substop(&mut e, &subst);
                Some(Tree::LetP(PrimStatement {
                    name: None,
                    typ: p.typ,
                    exp: Some(e)
                }))
            }
            (None, None) => panic!("Invalid Primitive")
        }
    }
}

pub fn interference(alloc: &mut Allocator, sm: &mut SymbolManager) -> HashMap<Symbol, HashSet<Symbol>> {
    let mut interference = HashMap::new();
    let live = liveout(alloc, sm);
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        let mut cur = live[id].clone();
        for item in nodes[id].content.iter().rev() {
            match item {
                Tree::LetP(ref p) => match &p.exp {
                    Some(e) => useop(&e, &mut cur, sm),
                    None => ()
                },
                _ => ()
            }
        }
        for &sym1 in &cur {
            for &sym2 in &cur {
                if sym1 == sym2 { continue }
                let entry = interference.entry(sym1).or_insert(HashSet::new());
                entry.insert(sym2);
            }
        }
    }
    interference
}

pub fn revert(alloc: &mut Allocator, sm: &SymbolManager) {
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

    // To be replaced by the actual pcopy routine.
    fn pcopy(args: Vec<Operand>) -> TreeContainer {
        let mut i = 0;
        let mut res = TreeContainer::new();
        while i < args.len() {
            let (name, exp, typ) = match (args[i].clone(), args[i+1].clone(), args[i+2].clone()) {
                (Operand::V(n), e, Operand::Tp(t)) => (n, e, t),
                _ => panic!("")
            };
            let stmt = Tree::LetP(PrimStatement {
                name: Some(name), typ, exp: Some(exp)
            });
            res.push_back(stmt);
            i += 3;
        }
        res
    }

    for id in 0..nodes.len() {
        let mut res = TreeContainer::new();
        while let Some(mut cur) = nodes[id].content.pop_front() {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, .. })),
                ..
            }) = cur {}
            else if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Pcopy, args })),
                ..
            }) = cur {
                res.append(pcopy(args));
            } else {
                substtree(&mut cur, &mut subst);
                res.push_back(cur);
            }
        }
        nodes[id].content = res;
    }
}


fn substtree(root: &mut Tree, state: &Substitution<Symbol>) {
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