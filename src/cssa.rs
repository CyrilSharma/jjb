use std::collections::HashMap;
use std::collections::HashSet;

use crate::dsu;
use crate::dsu::Dsu;
use crate::flatten;
use crate::graph;
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
                if pcopy_args.len() > 0 {
                    let assign = Tree::LetP(PrimStatement {
                        name: None,
                        typ: Typ::Void,
                        exp: Some(Operand::T(ExprTree {
                            op: Operation::Pcopy,
                            args: pcopy_args
                        }))
                    });
                    phis.push_back(assign);
                }
                nodes[id].content = phis + content;
                break;
            }
        }
    }
}

pub fn coalesce(alloc: &mut Allocator, sm: &mut SymbolManager) {
    let mut interference = interference(alloc, sm);
    print_interference(&interference, sm);
    let nodes = &mut alloc.nodes;
    let mut subst = Substitution::new();
    let mut dsu = Dsu::new(sm.numsyms());
    // I'll implement fixed point iteration one day
    for iter in 0..10 {
        for id in 0..nodes.len() {
            let list = std::mem::take(&mut nodes[id].content);
            nodes[id].content = coalesce_list(list, &mut subst, &mut interference, &mut dsu)
        }
    }

    fn coalesce_list(
        mut list: TreeContainer,
        subst: &mut Substitution<Symbol>,
        interference: &mut HashMap<Symbol, HashSet<Symbol>>,
        value: &mut Dsu
    ) -> TreeContainer {
        let mut res = TreeContainer::new();
        while let Some(item) = list.pop_front() {
            match item {
                Tree::LetP(p) => {
                    let tree = coalesce_prim(p, subst, interference, value);
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
        subst: &mut Substitution<Symbol>,
        interference: &mut HashMap<Symbol, HashSet<Symbol>>,
        value: &mut Dsu
    ) -> Option<Tree> {
        p.name.as_mut().map(|n| *n = subst.subst(*n));
        p.exp.as_mut().map(|n| substop(n, subst));
        if let (Some(name), Some(Operand::V(sym))) = (&p.name, &p.exp) {
            if *name == *sym { return None }
            if !interference[name].contains(&sym) || value.same(name.id, sym.id) {
                if let Some(edges) = interference.get_mut(&sym) {
                    for item in std::mem::take(edges) {
                        let entry = interference.entry(*name).or_insert(HashSet::new());
                        entry.insert(item);
                    }
                }
                subst.add_subst(*name, *sym);
                println!("I coalesced something!");
                return None;
            }
            value.join(name.id, sym.id);
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
    let live = liveout(alloc, sm);
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        let mut cur = live[id].clone();
        for item in nodes[id].content.iter().rev() {
            match item {
                Tree::LetP(ref p) => {
                    if let Some(n) = &p.name {
                        let entry = interference.entry(*n).or_insert(HashSet::new());
                        for &sym in &cur { entry.insert(sym); }
                    }
                    if let Some(e) = &p.exp { useop(&e, &mut cur, sm); }
                    if let Some(n) = &p.name { cur.remove(n); }
                }
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
    for (block, usage) in used_by_block.iter().enumerate() {
        println!("block {}", block);
        for &item in usage {
            println!("-- {}", sm.uname(item));
        }
    }
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

    for (block, usage) in live.iter().enumerate() {
        println!("block {}", block);
        for &item in usage {
            println!("-- {}", sm.uname(item));
        }
    }

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
            let mut i = 0;
            while i < args.len() {
                match (&args[i], &args[i+1]) {
                    (Operand::V(a), Operand::V(b)) => {
                        mp.insert(*b);
                        mp.remove(a);
                    },
                    _ => panic!("Invalid Pcopy!")
                }
                i += 3;
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


pub fn revert(tree: Tree, sm: &mut SymbolManager) -> Tree {
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| revert(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let (mut allocator, next_map) = graph::build(f.body);
            transform(&mut allocator, sm);
            println!("Before Coalescing");
            print_graph(&allocator.nodes, sm);
            coalesce(&mut allocator, sm);
            println!("After Coalescing");
            print_graph(&allocator.nodes, sm);
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

pub fn revert_graph(alloc: &mut Allocator, sm: &SymbolManager) {
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
        ast = Box::new(ssa::transform(*ast, &mut sm));
        ast = optimize(ast.as_ref(), &mut sm);
        ast = Box::new(super::revert(*ast, &mut sm));
        print(&ast, &sm);
        assert!(false)
    }
}