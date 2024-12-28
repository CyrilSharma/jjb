use crate::container::ContainerHelpers;
use crate::dsu::Dsu;
use crate::graph;
use crate::graph::*;
use crate::ir::*;
use crate::substitution::Substitution;
use crate::symbolmanager::{Symbol, SymbolManager};
use std::collections::HashMap;
use std::collections::HashSet;

pub fn transform(alloc: &mut Allocator, sm: &mut SymbolManager) {
    fn back_insert(pred: usize, assign: Tree, nodes: &mut Vec<CfgNode>) {
        if let Some(last_element) = nodes[pred].content.pop_back() {
            let insert_back = match &last_element {
                Tree::Switch(_)
                | Tree::If(_)
                | Tree::Loop(_)
                | Tree::Block(_)
                | Tree::Continue(_)
                | Tree::Break(_) => false,
                Tree::Return(_) => panic!("Invalid return transition"),
                Tree::Try(_) => todo!(),
                _ => true,
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
        sm: &mut SymbolManager,
    ) -> (Tree, (Symbol, Symbol, Typ), Vec<(Symbol, Symbol, Typ)>) {
        let (name, typ, ops) = match el {
            Tree::LetP(PrimStatement {
                exp:
                    Some(Operand::T(ExprTree {
                        op: Operation::Phi,
                        args,
                    })),
                name: Some(nm),
                typ,
            }) => (nm, typ, args),
            _ => panic!(),
        };
        let old_args: Vec<Symbol> = ops
            .into_iter()
            .map(|op| match op {
                Operand::V(sym) => sym,
                _ => panic!(""),
            })
            .collect();
        let new_args: Vec<Symbol> = old_args.iter().map(|sym| sm.refresh(sym)).collect();
        let wrapped_args = new_args.iter().map(|sym| Operand::V(*sym)).collect();
        let assign_name = sm.refresh(&name);
        let res_phi = Tree::LetP(PrimStatement {
            exp: Some(Operand::T(ExprTree {
                op: Operation::Phi,
                args: wrapped_args,
            })),
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
            nargs.extend(vec![Operand::V(new), Operand::V(old), Operand::Tp(t)]);
        }
        Tree::LetP(PrimStatement {
            name: None,
            typ: Typ::Void,
            exp: Some(Operand::T(ExprTree {
                op: Operation::Pcopy,
                args: nargs,
            })),
        })
    }

    fn insert_pcopies(
        pred_args: &Vec<(Symbol, Symbol, Typ)>,
        mp: &HashMap<Symbol, HashSet<Id>>,
        nodes: &mut Vec<CfgNode>,
        sm: &SymbolManager,
    ) {
        let mut bmap = HashMap::new();
        for &(new, old, tp) in pred_args {
            let pred = mp
                .get(&old)
                .expect(&format!("Variable {} not defined.", sm.uname(old)))
                .iter()
                .next()
                .expect("Invalid Stat Run.");
            let entry = bmap.entry(pred).or_insert(Vec::new());
            entry.push((new, old, tp));
        }
        for (block, args) in bmap {
            back_insert(*block, pcopy(args), nodes);
        }
    }

    let (mp, _) = graph::stat(alloc);
    let nodes = &mut alloc.nodes;
    let mut pred_pcopies = Vec::new();
    for id in 0..nodes.len() {
        let mut phis = TreeContainer::new();
        let mut pcopy_args = Vec::new();
        while let Some(cur) = nodes[id].content.pop_front() {
            if let Tree::LetP(PrimStatement {
                exp:
                    Some(Operand::T(ExprTree {
                        op: Operation::Phi, ..
                    })),
                ..
            }) = cur
            {
                let (phi, copy_args, pred_args) = process_phi(cur, sm);
                phis.push_back(phi);
                pcopy_args.push(copy_args);
                pred_pcopies.extend(pred_args);
            } else {
                if pcopy_args.len() > 0 {
                    phis.push_back(pcopy(pcopy_args))
                }
                let mut content = std::mem::take(&mut nodes[id].content);
                let mut res = phis;
                // Because we popped a normal piece of the node's content
                // Whilst trying to grab all the Phi nodes.
                res.push_back(cur);
                res.extend(content);
                nodes[id].content = res;
                break;
            }
        }
    }
    insert_pcopies(&pred_pcopies, &mp, nodes, sm);
}

pub fn coalesce(alloc: &mut Allocator, sm: &mut SymbolManager) {
    let mut value = Dsu::new(sm.numsyms());
    let mut moverelated = Vec::new();
    for id in 0..alloc.nodes.len() {
        precompute(&alloc.nodes[id].content, &mut moverelated, &mut value)
    }

    let interf = interference(alloc, sm);
    let mut subst = build_subst(moverelated, interf, value, sm);
    for _ in 0..10 {
        // I'll implement fixed point iteration one day
        for id in 0..alloc.nodes.len() {
            let list = std::mem::take(&mut alloc.nodes[id].content);
            alloc.nodes[id].content = coalesce_list(list, &mut subst);
        }
    }

    // In this case, i_41 has the same value as i_122,
    // But i_122 never has the same value as i_41.
    // How to encode this in value...

    // i_41 = i_122
    // i_122 = i_41 + 1
    // They use the dominance tree to address
    // Perhaps, I should compute the dominance tree,
    // Then just use their methods instead of handrolling my own interference graph.
    fn precompute(
        list: &TreeContainer,
        moverelated: &mut Vec<(Symbol, Symbol)>,
        value: &mut Dsu,
    ) -> () {
        for item in list {
            match item {
                Tree::LetP(PrimStatement {
                    name: Some(name),
                    exp: Some(Operand::V(sym)),
                    ..
                }) => {
                    value.join(name.id, sym.id);
                    moverelated.push((*name, *sym))
                }
                Tree::LetP(PrimStatement {
                    name: None,
                    exp:
                        Some(Operand::T(ExprTree {
                            op: Operation::Pcopy,
                            args,
                        })),
                    ..
                }) => {
                    for i in (0..args.len()).step_by(3) {
                        match (&args[i], &args[i + 1]) {
                            (Operand::V(a), Operand::V(b)) => {
                                value.join(a.id, b.id);
                                moverelated.push((*a, *b))
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
        }
    }

    fn build_subst(
        moverelated: Vec<(Symbol, Symbol)>,
        mut interference: Interference,
        mut value: Dsu,
        sm: &mut SymbolManager,
    ) -> Substitution<Symbol> {
        let symlist: Vec<Symbol> = interference.data.iter().map(|(a, _)| *a).collect();
        let mut tryfuse = |a: Symbol, b: Symbol| {
            if interference.interferes(a, b, &mut value) {
                return;
            }
            // println!("Coalescesing {}, {}", sm.uname(a), sm.uname(b));
            interference.merge(a, b);
        };
        for (a, b) in moverelated {
            tryfuse(a, b);
        }
        for (i, &a) in symlist.iter().enumerate() {
            for (j, &b) in symlist.iter().enumerate() {
                if i >= j {
                    break;
                }
                tryfuse(a, b);
            }
        }
        interference.to_subst()
    }

    fn coalesce_list(mut list: TreeContainer, subst: &mut Substitution<Symbol>) -> TreeContainer {
        let mut res = TreeContainer::new();
        while let Some(item) = list.pop_front() {
            match item {
                Tree::LetP(p) => {
                    let tree = coalesce_prim(p, subst);
                    tree.map(|t| res.push_back(t));
                }
                mut other => {
                    substtree(&mut other, subst);
                    res.push_back(other);
                }
            }
        }
        res
    }

    fn coalesce_prim(mut p: PrimStatement, subst: &mut Substitution<Symbol>) -> Option<Tree> {
        p.name.as_mut().map(|n| *n = subst.subst(*n));
        p.exp.as_mut().map(|n| substop(n, subst));
        if let (Some(name), Some(Operand::V(sym))) = (&p.name, &p.exp) {
            if *name == *sym {
                return None;
            }
        }
        return Some(Tree::LetP(PrimStatement {
            name: p.name,
            typ: p.typ,
            exp: p.exp,
        }));
    }
}

struct Interference {
    data: HashMap<Symbol, HashSet<Symbol>>,
    equivalences: HashMap<Symbol, Vec<Symbol>>,
    subst: Substitution<Symbol>,
}

impl Interference {
    fn new() -> Self {
        Interference {
            data: HashMap::new(),
            equivalences: HashMap::new(),
            subst: Substitution::new(),
        }
    }

    fn interferes(&self, a: Symbol, b: Symbol, value: &mut Dsu) -> bool {
        let (a, b) = (self.subst.subst(a), self.subst.subst(b));
        let (av, bv) = (vec![a], vec![b]);
        let acomp = self.equivalences.get(&a).unwrap_or(&av);
        let bcomp = self.equivalences.get(&b).unwrap_or(&bv);
        // print!("a: "); for &asym in acomp { print!("{} ", asym.id) } println!();
        // print!("b: "); for &bsym in bcomp { print!("{} ", bsym.id) } println!();
        for asym in acomp {
            for bsym in bcomp {
                if value.same(asym.id, bsym.id) {
                    continue;
                }
                if self.data[&asym].contains(&bsym) {
                    return true;
                }
            }
        }
        false
    }

    fn merge(&mut self, a: Symbol, b: Symbol) {
        let (a, b) = (self.subst.subst(a), self.subst.subst(b));
        if a == b {
            return;
        }
        let mut bcomp = std::mem::take(self.equivalences.entry(b).or_insert(vec![b]));
        let acomp = self.equivalences.entry(a).or_insert(vec![a]);
        acomp.extend(bcomp);
        self.subst.add_subst(b, a);
    }

    fn add_interference(&mut self, a: Symbol, b: &HashSet<Symbol>) {
        for &sym in b {
            let entry = self.data.entry(a).or_insert(HashSet::new());
            entry.insert(sym);
            let entry = self.data.entry(sym).or_insert(HashSet::new());
            entry.insert(a);
        }
    }

    fn to_subst(mut self) -> Substitution<Symbol> {
        self.subst.bake();
        self.subst
    }
}

// Easily invalidated. But it will do for now.
fn interference(alloc: &mut Allocator, sm: &mut SymbolManager) -> Interference {
    let mut interference = Interference::new();
    let live = graph::liveout(alloc, sm);
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        let mut cur = live[id].clone();
        for item in nodes[id].content.iter().rev() {
            match item {
                Tree::LetP(PrimStatement {
                    name: None,
                    exp:
                        Some(Operand::T(ExprTree {
                            op: Operation::Pcopy,
                            ref args,
                        })),
                    ..
                }) => {
                    for i in (0..args.len()).step_by(3) {
                        match &args[i] {
                            Operand::V(a) => interference.add_interference(*a, &cur),
                            _ => (),
                        }
                    }
                    for i in (0..args.len()).step_by(3) {
                        match &args[i] {
                            Operand::V(a) => {
                                useop(&args[i + 1], &mut cur, sm);
                                cur.remove(a);
                            }
                            _ => (),
                        }
                    }
                }
                Tree::LetP(ref p) => {
                    if let Some(n) = &p.name {
                        interference.add_interference(*n, &cur)
                    }
                    if let Some(e) = &p.exp {
                        useop(&e, &mut cur, sm)
                    }
                    if let Some(n) = &p.name {
                        cur.remove(n);
                    }
                }
                Tree::Block(_) | Tree::Break(_) | Tree::Continue(_) => (),
                Tree::Switch(s) => useop(&s.arg, &mut cur, sm),
                Tree::Loop(l) => useop(&l.cond, &mut cur, sm),
                Tree::If(l) => useop(&l.cond, &mut cur, sm),
                Tree::Return(r) => {
                    if let Some(e) = r.val.as_ref() {
                        useop(e, &mut cur, sm)
                    }
                }
                Tree::Try(_) => todo!(),
                _ => panic!("Invalid node in Block"),
            }
        }
    }

    let mut fusephi = |name, args: &Vec<Operand>| {
        for arg in &args[1..] {
            let sym = match arg {
                Operand::V(sym) => *sym,
                _ => panic!(),
            };
            interference.merge(name, sym);
        }
    };

    for id in 0..nodes.len() {
        for item in nodes[id].content.iter() {
            match item {
                Tree::LetP(PrimStatement {
                    name: Some(name),
                    exp:
                        Some(Operand::T(ExprTree {
                            op: Operation::Phi,
                            ref args,
                        })),
                    ..
                }) => fusephi(*name, args),
                _ => (),
            }
        }
    }
    interference
}

pub fn revert(tree: Tree, sm: &mut SymbolManager) -> Tree {
    match tree {
        Tree::Program(stmts) => Tree::Program(stmts.into_iter().map(|f| revert(f, sm)).collect()),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let (mut allocator, next_map) = graph::build(f.body);
            transform(&mut allocator, sm);
            coalesce(&mut allocator, sm);
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
        _ => panic!("Invalid tree in revert"),
    }
}

pub fn revert_graph(alloc: &mut Allocator, sm: &mut SymbolManager) {
    fn emit_copy(dest: Symbol, src: Symbol, t: Typ) -> Tree {
        Tree::LetP(PrimStatement {
            name: Some(dest),
            exp: Some(Operand::V(src)),
            typ: t,
        })
    }

    // Stolen shamelessly from https://inria.hal.science/inria-00349925v1/document
    fn pcopy(args: Vec<Operand>, sm: &mut SymbolManager) -> TreeContainer {
        let (mut edges, mut types) = (Vec::new(), HashMap::new());
        for i in (0..args.len()).step_by(3) {
            let (dest, src, typ) = match (&args[i], &args[i + 1], &args[i + 2]) {
                (Operand::V(a), Operand::V(b), Operand::Tp(t)) => (*a, *b, t),
                _ => panic!("Invalid Pcopy"),
            };
            if src == dest {
                continue;
            }
            edges.push((dest, src));
            types.insert(dest, typ);
        }

        let (mut ready, mut todo) = (Vec::new(), Vec::new());
        let (mut loc, mut pred) = (HashMap::new(), HashMap::new());
        for &(dest, src) in &edges {
            // Where to find the "original" value of things we need to copy.
            loc.insert(src, src);
            // an assignment dest = src is considered as an edge dest <- src.
            pred.insert(dest, src);
            // Contains all the destinations of copies we need to process.
            todo.push(dest);
        }

        // Ready contains variables who we can safely copy into.
        for &(dest, _) in &edges {
            // dest is never used as the src for anything, so it's value isn't needed.
            // (keep in mind it's still used as a dest!) and so we can queue it. i.e let
            // whoever wants to copy in do so.
            if !loc.contains_key(&dest) {
                ready.push(dest);
            }
        }

        let mut res = TreeContainer::new();
        while let Some(cycle) = todo.pop() {
            // dest's value has been stored (or it didn't need to be)
            // let dest's predecessor copy in.
            while let Some(dest) = ready.pop() {
                let srcvar = pred[&dest];
                let srcloc = loc[&srcvar];
                let tp = types[&dest];
                res.push_back(emit_copy(dest, srcloc, *tp));
                loc.insert(srcvar, dest);
                // If the variable previously wasn't queued
                // And if we need to stash something in the variable, queue it.
                if srcloc == srcvar && pred.contains_key(&srcvar) {
                    ready.push(srcvar);
                }
            }

            // We've processed all the tree nodes, so this node is definitely part of a cycle.
            // However, if it's predecessor was already stored in this node, then we must've already
            // Dealt with it. This can happen if the cycle node was attached to some other tree.
            if cycle != loc[&pred[&cycle]] {
                // Technically, we only need to make a new one once per unique type.
                // TODO: do that ^.
                let temp = sm.fresh("cycle_breaker");
                res.push_back(emit_copy(temp, cycle, *types[&cycle]));
                loc.insert(cycle, temp);
                // We've backed it up. It's safe to let things copy in now.
                ready.push(cycle);
            }
        }
        res
    }

    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        let mut res = TreeContainer::new();
        while let Some(cur) = nodes[id].content.pop_front() {
            if let Tree::LetP(PrimStatement {
                exp:
                    Some(Operand::T(ExprTree {
                        op: Operation::Phi, ..
                    })),
                ..
            }) = cur
            {
            } else if let Tree::LetP(PrimStatement {
                exp:
                    Some(Operand::T(ExprTree {
                        op: Operation::Pcopy,
                        args,
                    })),
                ..
            }) = cur
            {
                res.extend(pcopy(args, sm));
            } else {
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
        }
        Tree::Switch(ref mut s) => substop(&mut s.arg, state),
        Tree::Loop(ref mut l) => substop(&mut l.cond, state),
        Tree::If(ref mut i) => substop(&mut i.cond, state),
        Tree::Return(r) => {
            r.val.as_mut().map(|e| substop(e, state));
        }
        Tree::Block(_) | Tree::Break(_) | Tree::Continue(_) => (),
        _ => panic!("Invalid Tree"),
    }
}

fn substop(op: &mut Operand, state: &Substitution<Symbol>) {
    use ArrayExpression as A;
    match op {
        Operand::This(c) => (),
        Operand::Super(s) => (),
        Operand::V(ref mut sym) => {
            *sym = state.subst(*sym);
        }
        Operand::T(ExprTree { op, args }) => args.iter_mut().for_each(|a| substop(a, state)),
        Operand::A(A::Empty(a)) => a.iter_mut().for_each(|a| substop(a, state)),
        Operand::A(A::Initializer(a)) => substarray(a, state),
        _ => (),
    }
}

fn substarray(a: &mut ArrayInitializer, state: &Substitution<Symbol>) {
    use ElementInitializer as E;
    a.iter_mut().for_each(|e| match e.as_mut() {
        E::Expr(op) => substop(op, state),
        E::ArrayInitializer(c) => substarray(c, state),
    })
}

// Make the dominating definition the only declaration,
// If there is no dominating definition, insert one.
fn fix_declarations(alloc: &mut Allocator) {
    let mut decl = HashMap::new();
    let nodes = &mut alloc.nodes;
    for id in 0..nodes.len() {
        nodes[id].content.retain(|item| match item {
            Tree::LetP(PrimStatement {
                name: Some(sym),
                typ,
                exp,
            }) if *typ != Typ::Void => {
                decl.insert(*sym, *typ);
                *typ = Typ::Void;
                exp.is_some()
            }
            _ => true,
        })
    }

    'top: for (name, tp) in decl {
        for tree in nodes[0].content.iter_mut() {
            match tree {
                Tree::LetP(PrimStatement {
                    name: Some(n), typ, ..
                }) if name == *n => {
                    *typ = tp;
                    continue 'top;
                }
                _ => (),
            }
        }
        nodes[0].content.push_front(Tree::LetP({
            let lit = defaultLit(tp);
            PrimStatement {
                name: Some(name),
                typ: tp,
                exp: Some(Operand::C(lit)),
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
        Typ::Class(_) => Literal::Null,
    }
}

#[allow(unused)]
fn print_interference(interference: &HashMap<Symbol, HashSet<Symbol>>, sm: &SymbolManager) {
    println!("digraph G {{");
    println!(r#"  node [shape=box, fontname="Helvetica", fontsize=12, ordering="out"]"#);
    for (&sym, edges) in interference {
        println!(
            r#"{} [label=<
    <table border="0" cellborder="0" cellspacing="0">
      <tr><td> {} </td></tr>   
    </table>
  >]"#,
            sm.uname(sym),
            sm.uname(sym)
        );
        for (i, &nbr) in edges.iter().enumerate() {
            println!("  {} -> {}", sm.uname(sym), sm.uname(nbr));
        }
    }
    println!("}}");
}

#[cfg(test)]
mod test {
    use crate::converter::convert;
    use crate::flatten::{self, flatten};
    use crate::graph::print_graph;
    use crate::hoist::hoist;
    use crate::optimizer::optimize;
    use crate::parameters::Parameters;
    use crate::printer::print;
    use crate::ssa;
    use crate::symbolmanager::SymbolManager;
    use crate::typeinfer::typeinfer;
    use tree_sitter::Parser;

    #[test]
    pub fn f() {
        let text = r#"
        class Test {
            public static void main(String[] args) {
                int i = 0, j = 0, k = 0;
                label1: while (i++ < 10) {
                    label2: while (j++ < 10) {
                        label3: while (k++ < 10) {
                            System.out.printf("%d %d %d\n", i, j, k);
                            if ((i ^ j) < (i ^ k)) {
                                continue label2;
                            }
                            if (i < 4) { continue label1; }
                            break label3;
                        }
                        if (j > 3) { continue label2; }
                        break label1;
                    }
                }
            }
          }
        "#;

        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_java::language())
            .expect("Error loading Java grammar");
        let tree = parser.parse(text, None).unwrap();
        let mut sm = SymbolManager::new();
        let class_name = "Test".to_string();
        let params = Parameters {
            entry_class: class_name,
            entry_name: "main".to_string(),
        };
        let mut ast = convert(tree.root_node(), text.as_bytes(), &params, &mut sm);
        ast = hoist(ast.as_ref(), &mut sm);
        typeinfer(ast.as_mut(), &mut sm);
        ast = Box::new(ssa::transform(*ast, &mut sm));
        ast = optimize(ast.as_ref(), &mut sm);
        print(&ast, &sm);
        ast = Box::new(flatten::flatten(*ast, &mut sm));
        ast = Box::new(super::revert(*ast, &mut sm));
        print(&ast, &sm);
    }
}
