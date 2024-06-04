use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Sub;

use crate::dsu;
use crate::dsu::Dsu;
use crate::flatten;
use crate::graph;
use crate::ir::*;
use crate::graph::*;
use crate::substitution::Substitution;
use crate::symbolmanager::{Symbol, SymbolManager};
pub fn transform(alloc: &mut Allocator, sm: &mut SymbolManager) {
    fn back_insert(pred: usize, assign: Tree, nodes: &mut Vec<CfgNode>) {
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

    fn process_phi(el: Tree, sm: &mut SymbolManager) -> (
        Tree, (Symbol, Symbol, Typ), Vec<(Symbol, Symbol, Typ)>,
    ) {
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
        let wrapped_args = new_args.iter().map(|sym| Operand::V(*sym)).collect();
        let assign_name = sm.refresh(&name);
        let res_phi = Tree::LetP(PrimStatement {
            exp: Some(Operand::T(ExprTree { op: Operation::Phi, args: wrapped_args })),
            name: Some(assign_name),
            typ,
        });
        let mut pred_args = Vec::new();
        for (&old, &new) in old_args[1..].iter().zip(new_args[1..].iter()) {
            pred_args.push((new, old, typ));
        }
        (res_phi, (name, assign_name, typ), pred_args)
    }

    fn pcopy(args: Vec<(Symbol, Symbol, Typ)>) -> Tree {
        let mut nargs = Vec::new();
        for (new, old, t) in args {
            nargs.extend(vec![
                Operand::V(new),
                Operand::V(old),
                Operand::Tp(t)
            ]);
        }
        Tree::LetP(PrimStatement {
            name: None,
            typ: Typ::Void,
            exp: Some(Operand::T(ExprTree {
                op: Operation::Pcopy, args: nargs
            }))
        })
    }

    fn insert_pcopies(
        pred_args: &Vec<(Symbol, Symbol, Typ)>,
        mp: &HashMap<Symbol, HashSet<Id>>,
        nodes: &mut Vec<CfgNode>,
        sm: &SymbolManager
    ) {
        let mut bmap = HashMap::new();
        for &(new, old, tp) in pred_args {
            let pred = mp.get(&old)
                .expect(&format!("Variable {} not defined.", sm.uname(old)))
                .iter().next().expect("Invalid Stat Run.");
            let entry = bmap.entry(pred).or_insert(Vec::new());
            entry.push((new, old, tp));
        }
        for (block, args) in bmap {
            back_insert(*block, pcopy(args), nodes);
        }
    }

    let (mp, _) = crate::ssa::stat(alloc);
    let nodes = &mut alloc.nodes;
    let mut pred_pcopies = Vec::new();
    for id in 0..nodes.len() {
        let mut phis = TreeContainer::new();
        let mut pcopy_args = Vec::new();
        while let Some(cur) = nodes[id].content.pop_front() {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, .. })),
                ..
            }) = cur {
                let (phi, copy_args, pred_args) = process_phi(cur, sm);
                phis.push_back(phi);
                pcopy_args.push(copy_args);
                pred_pcopies.extend(pred_args);
            } else {
                nodes[id].content.push_front(cur);
                let content = std::mem::take(&mut nodes[id].content);
                if pcopy_args.len() > 0 { phis.push_back(pcopy(pcopy_args)) }
                nodes[id].content = phis + content;
                break;
            }
        }
    }
    insert_pcopies(&pred_pcopies, &mp, nodes, sm);
}

pub fn coalesce(alloc: &mut Allocator, sm: &mut SymbolManager) {
    let interference = interference(alloc, sm);
    let mut value = Dsu::new(sm.numsyms());
    let mut moverelated = Vec::new();
    for id in 0..alloc.nodes.len() {
        precompute(
            &alloc.nodes[id].content,
            &mut moverelated,
            &mut value
        )
    }


    let mut subst = build_subst(moverelated, interference, value);
    for _ in 0..10 { // I'll implement fixed point iteration one day
        for id in 0..alloc.nodes.len() {
            let list = std::mem::take(&mut alloc.nodes[id].content);
            alloc.nodes[id].content = coalesce_list(list, &mut subst);
        }
    }

    fn precompute(
        list: &TreeContainer,
        moverelated: &mut Vec<(Symbol, Symbol)>,
        value: &mut Dsu
    ) -> () {
        for item in list {
            match item {
                Tree::LetP(PrimStatement { name: Some(name), exp: Some(Operand::V(sym)), .. }) => {
                    value.join(name.id, sym.id);
                    moverelated.push((*name, *sym))
                },
                Tree::LetP(PrimStatement { name: None, exp: Some(Operand::T(
                    ExprTree { op: Operation::Pcopy, args }
                )), ..  }) => {
                    for i in (0..args.len()).step_by(3) {
                        match (&args[i], &args[i+1]) {
                            (Operand::V(a), Operand::V(b)) => {
                                value.join(a.id, b.id);
                                moverelated.push((*a, *b))
                            },
                            _ => ()
                        }
                    }
                },
                _ => ()
            }
        }
    }

    fn build_subst(
        moverelated: Vec<(Symbol, Symbol)>,
        mut interference: HashMap<Symbol, HashSet<Symbol>>,
        mut value: Dsu
    ) -> Substitution<Symbol> {
        let mut components = Dsu::new(value.e.len());
        let mut res = Substitution::new();
        let symlist: Vec<Symbol> = interference.iter().map(|(a, _)| *a).collect();
        let mut tryfuse = |a: Symbol, b: Symbol| {
            if components.same(a.id, b.id) { return false }
            if interference[&a].contains(&b) && !value.same(a.id, b.id) { return false }
            if let Some(edges) = interference.get_mut(&b) {
                for item in std::mem::take(edges) {
                    let entry = interference.entry(a).or_insert(HashSet::new());
                    entry.insert(item);
                }
            }
            // res.add_subst(b, a);
            components.join(a.id, b.id);
            return true;
        };

        for (a, b) in moverelated { tryfuse(a, b); }
        for (i, &a) in symlist.iter().enumerate() {
            for (j, &b) in symlist.iter().enumerate() {
                if i >= j { break }
                tryfuse(a, b);
            }
        }
        for &a in symlist.iter() {
            let parent = components.find(a.id);
            if parent == a.id { continue; }
            res.add_subst(a, Symbol { id: parent })
        }
        res
    }

    fn coalesce_list(
        mut list: TreeContainer,
        subst: &mut Substitution<Symbol>
    ) -> TreeContainer {
        let mut res = TreeContainer::new();
        while let Some(item) = list.pop_front() {
            match item {
                Tree::LetP(p) => {
                    let tree = coalesce_prim(p, subst);
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
        mut p: PrimStatement,
        subst: &mut Substitution<Symbol>
    ) -> Option<Tree> {
        p.name.as_mut().map(|n| *n = subst.subst(*n));
        p.exp.as_mut().map(|n| substop(n, subst));
        if let (Some(name), Some(Operand::V(sym))) = (&p.name, &p.exp) {
            if *name == *sym { return None }
        }
        return Some(Tree::LetP(PrimStatement {
            name: p.name,
            typ: p.typ,
            exp: p.exp
        }));
    }
}

fn interference(alloc: &mut Allocator, sm: &mut SymbolManager) -> HashMap<Symbol, HashSet<Symbol>> {
    let mut interference = HashMap::new();
    let mut add_interference = |name: Symbol, cur: &HashSet<Symbol>| {
        let entry = interference.entry(name).or_insert(HashSet::new());
        for &sym in cur.iter() { entry.insert(sym); }
    };
    let remove_def = |name, cur: &mut HashSet<Symbol>| cur.remove(&name);
    let live = liveout(alloc, sm);
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        let mut cur = live[id].clone();
        for item in nodes[id].content.iter().rev() {
            match item {
                Tree::LetP(PrimStatement { name: None, exp: Some(Operand::T(
                    ExprTree { op: Operation::Pcopy, ref args }
                )), ..  }) => {
                    for i in (0..args.len()).step_by(3) {
                        match &args[i] {
                            Operand::V(a) => add_interference(*a, &mut cur),
                            _ => ()
                        }
                    }
                    for i in (0..args.len()).step_by(3) {
                        match &args[i] {
                            Operand::V(a) => {
                                useop(&args[i+1], &mut cur, sm);
                                remove_def(*a, &mut cur);
                            },
                            _ => ()
                        }
                    }
                },
                Tree::LetP(ref p) => {
                    if let Some(n) = &p.name { add_interference(*n, &mut cur) }
                    if let Some(e) = &p.exp { useop(&e, &mut cur, sm) }
                    if let Some(n) = &p.name { remove_def(*n, &mut cur); }
                },
                _ => ()
            }
        }
    }
    interference
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
    // for (block, usage) in used_by_block.iter().enumerate() {
    //     println!("block {}", block);
    //     for &item in usage {
    //         println!("-- {}", sm.uname(item));
    //     }
    // }
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

    // for (block, usage) in live.iter().enumerate() {
    //     println!("block {}", block);
    //     for &item in usage {
    //         println!("-- {}", sm.uname(item));
    //     }
    // }

    live
}

fn used(alloc: &mut Allocator, sm: &SymbolManager) -> Vec<HashSet<Symbol>> {
    let mut res = vec![HashSet::new(); alloc.len()];
    for (i, node) in alloc.nodes.iter().enumerate() {
        for item in node.content.iter().rev() {
            match item {
                Tree::LetP(p) => {
                    if let Some(e) = p.exp.as_ref() { useop(e,  &mut res[i], sm) }
                    if let Some(n) = p.name.as_ref() { res[i].remove(n); }
                },
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
        Operand::T(ExprTree { args, op: Operation::Pcopy }) => {
            for i in (0..args.len()).step_by(3) {
                match (&args[i], &args[i+1]) {
                    (Operand::V(a), Operand::V(b)) => {
                        mp.insert(*b);
                        mp.remove(a);
                    },
                    _ => panic!("Invalid Pcopy!")
                }
            }
        },
        Operand::T(ExprTree { args, op: Operation::Phi }) => {
            args[1..].iter().for_each(|e| useop(e, mp, sm))
        },
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

pub fn revert(tree: Tree, sm: &mut SymbolManager) -> Tree {
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| revert(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let (mut allocator, next_map) = graph::build(f.body);
            transform(&mut allocator, sm);
            // print_graph(&allocator.nodes, sm);
            // coalesce(&mut allocator, sm);
            // print_graph(&allocator.nodes, sm);
            // YOU MUST DO FLATTENING HERE.
            // It actually cannot mess anything up,
            // We don't allow inlining into Phi functions
            // And it doesn't matter if var_that_will_be_inlined = c;
            // Was supposed to have its name changed, there was only one possible invocation site anyways.
            revert_graph(&mut allocator, sm);
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

pub fn revert_graph(alloc: &mut Allocator, sm: &mut SymbolManager) {
    let mut subst: Substitution<Symbol> = Substitution::new();
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        for cur in &nodes[id].content {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, args })),
                name: Some(nm),
                ..
            }) = cur {
                args.iter().skip(1).for_each(|arg| match arg {
                    Operand::V(sym) => subst.add_subst(*sym, *nm),
                    _ => panic!("")
                });
            }
        }
    }
    // subst.bake();

    fn emit_copy(dest: Symbol, src: Symbol, t: Typ) -> Tree {
        Tree::LetP(PrimStatement {
            name: Some(dest),
            exp: Some(Operand::V(src)),
            typ: t
        })
    }

    // Stolen shamelessly from https://inria.hal.science/inria-00349925v1/document
    fn pcopy(args: Vec<Operand>, subst: &Substitution<Symbol>, sm: &mut SymbolManager) -> TreeContainer {
        let mut i = 0;
        let mut res = TreeContainer::new();
        while i < args.len() {
            let (name, exp, typ) = match (args[i].clone(), args[i+1].clone(), args[i+2].clone()) {
                (Operand::V(a), Operand::V(b), Operand::Tp(t)) => (
                    subst.subst(a), Operand::V(subst.subst(b)), t
                ),
                _ => panic!("")
            };
            let stmt = Tree::LetP(PrimStatement {
                name: Some(name), typ, exp: Some(exp)
            });
            res.push_back(stmt);
            i += 3;
        }
        return res;
        
        // let (mut edges, mut types) = (Vec::new(), HashMap::new());
        // for i in (0..args.len()).step_by(3) {
        //     let (dest, src, typ) = match (&args[i], &args[i+1], &args[i+2]) {
        //         (Operand::V(a), Operand::V(b), Operand::Tp(t)) => {
        //             if a == b { continue }
        //             (*a, *b, t)
        //         },
        //         _ => panic!("Invalid Pcopy")
        //     };
        //     if src == dest { continue }
        //     edges.push((dest, src));
        //     types.insert(dest, typ);
        // }

        // let (mut ready, mut todo) = (Vec::new(), Vec::new());
        // let (mut loc, mut pred) = (HashMap::new(), HashMap::new());
        // for &(dest, src) in &edges {
        //     // Where to find the "original" value of things we need to copy.
        //     loc.insert(src, src);
        //     // an assignment dest = src is considered as an edge dest <- src.
        //     pred.insert(dest, src);
        //     // Contains all the destinations of copies we need to process.
        //     todo.push(dest);
        // }

        // // Ready contains variables who we can safely copy into.
        // for &(dest, _) in &edges {
        //     // dest is never used as the src for anything, so it's value isn't needed.
        //     // (keep in mind it's still used as a dest!) and so we can queue it. i.e let 
        //     // whoever wants to copy in do so.
        //     if !loc.contains_key(&dest) { ready.push(dest); }
        // }

        // let temp = sm.fresh("cycle_breaker");
        // let mut res = TreeContainer::new();
        // while let Some(cycle) = todo.pop() {
        //     // dest's value has been stored (or it didn't need to be)
        //     // let dest's predecessor copy in.
        //     while let Some(dest) = ready.pop() {
        //         let srcvar = pred[&dest];
        //         let srcloc = loc[&srcvar];
        //         let tp = types[&dest];
        //         res.push_back(emit_copy(dest, srcloc, *tp));
        //         loc.insert(srcvar, dest);
        //         // If the variable previously wasn't queued
        //         // And if we need to stash something in the variable, queue it.
        //         if srcloc == srcvar && pred.contains_key(&srcvar) {
        //             ready.push(srcvar);
        //         }
        //     }

        //     // We've processed all the tree nodes, so this node is definitely part of a cycle.
        //     // However, if it's predecessor was already stored in this node, then we must've already
        //     // Dealt with it. This can happen if the cycle node was attached to some other tree.
        //     if cycle != loc[&pred[&cycle]] {
        //         res.push_back(emit_copy(temp, cycle, *types[&cycle]));
        //         loc.insert(cycle, temp);
        //         // We've backed it up. It's safe to let things copy in now.
        //         ready.push(cycle);
        //     }
        // }
        // res
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
                res.append(pcopy(args, &subst, sm));
            } else {
                substtree(&mut cur, &subst);
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

// Make the dominating definition the only declaration,
// If there is no dominating definition, insert one.
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

#[allow(unused)]
fn print_interference(interference: &HashMap<Symbol, HashSet<Symbol>>, sm: &SymbolManager) {
    println!("digraph G {{");
    println!(r#"  node [shape=box, fontname="Helvetica", fontsize=12, ordering="out"]"#);
    for (&sym, edges) in interference {
        println!(r#"{} [label=<
    <table border="0" cellborder="0" cellspacing="0">
      <tr><td> {} </td></tr>   
    </table>
  >]"#, sm.uname(sym), sm.uname(sym));
        for (i, &nbr) in edges.iter().enumerate() {
            println!("  {} -> {}", sm.uname(sym), sm.uname(nbr));
        }
    }
    println!("}}");
}


#[allow(unused)]
fn print_live(live: &Vec<HashSet<Symbol>>, alloc: &mut Allocator, sm: &SymbolManager) {
    println!("digraph G {{");
    println!(r#"  node [shape=box, fontname="Helvetica", fontsize=12, ordering="out"]"#);
    for (block, vars) in live.iter().enumerate() {
        println!(r#"{} [label=<
    <table border="0" cellborder="0" cellspacing="0">
      <tr><td> Node {} </td></tr>
{}
    </table>
  >]"#, block, block, vars.iter().map(|v| sm.uname(*v)).collect::<Vec<String>>().join(", "));
        for &var in &alloc.nodes[block].children {
            println!("  {} -> {}", block, var);
        }
    }
    println!("}}");
}

#[cfg(test)]
mod test {
    use tree_sitter::Parser;
    use crate::flatten::{self, flatten};
    use crate::{cssa, ir::*, ssa};
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
            public static void main(String[] args) {
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
        ast = Box::new(ssa::transform(*ast, &mut sm));
        ast = optimize(ast.as_ref(), &mut sm);
        // ast = Box::new(flatten::flatten(*ast, &mut sm));
        ast = Box::new(super::revert(*ast, &mut sm));
        // print(&ast, &sm);
        // assert!(false)
    }
}