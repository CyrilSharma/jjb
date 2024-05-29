use std::collections::HashMap;
use crate::symbolmanager::SymbolManager;
use crate::printer::str_print;
use crate::container::ContainerIntoIter;
use crate::ir::*;
use crate::symbolmanager::Symbol;

#[allow(unused)]
pub fn print_graph(nodes: &[CfgNode], sm: &SymbolManager) {
    println!("digraph G {{");
    println!(r#"  node [shape=box, fontname="Helvetica", fontsize=12, ordering="out"]"#);
    for node in nodes {
        let res = node.content.iter().map(|n| format!(
            "      <tr><td>{}</td></tr>",
            str_print(n, sm).trim()
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("'", "&#39;")
        )).collect::<Vec<_>>().join("\n");
        println!(
r#"  {} [label=<
    <table border="0" cellborder="0" cellspacing="0">
      <tr><td> Node {} </td></tr>   
{}
    </table>
  >]"#, node.id, node.id, res);
        for &child in &node.children {
            println!("  {} -> {}\n", node.id, child)
        }
    }
    println!("}}");
}

type Id = usize;
#[derive(Clone)]
pub struct CfgNode {
    id: Id,
    content: TreeContainer,
    children: Vec<Id>,
    preds: Vec<Id>
}

impl Default for CfgNode {
    fn default() -> Self {
        Self {
            id: Default::default(),
            content: TreeContainer::new(),
            children: Default::default(),
            preds: Default::default()
        }
    }
}

pub struct Allocator { nodes: Vec<CfgNode> }
impl Allocator {
    pub fn new() -> Self {
        Self { nodes: vec![] }
    }
    pub fn alloc(&mut self) -> Id {
        let res = self.nodes.len();
        self.nodes.push(CfgNode::default());
        res
    }
    pub fn len(&self) -> usize { self.nodes.len() }
    pub fn set(&mut self, id: Id, content: TreeContainer, children: Vec<Id>) {
        self.nodes[id].id = id;
        self.nodes[id].content = content;
        for child in children { self.add_child(id, child) }
    }
    pub fn grab(&self, id: Id) -> &CfgNode {
        &self.nodes[id]
    }
    pub fn grab_mut(&mut self, id: Id) -> &mut CfgNode {
        &mut self.nodes[id]
    }
    pub fn add_child(&mut self, parent: Id, child: Id) {
        let p = self.grab_mut(parent);
        p.children.push(child);
        let c = self.grab_mut(child);
        c.preds.push(parent);
    }
}

mod transform {
    use fixedbitset::FixedBitSet;
    use crate::ir::{ExprTree, Operand};
    use std::collections::HashSet;

    use super::*;
    struct State<'l> {
        stack: HashMap<Symbol, Vec<Symbol>>,
        sm: &'l mut SymbolManager,
    }

    impl<'l> State<'l> {
        fn add_name(&mut self, sym: Symbol, nname: Symbol, push_count: &mut HashMap<Symbol, usize>) {
            let stk = self.stack.entry(sym).or_insert(Vec::new());
            stk.push(nname);
            let entry = push_count.entry(sym).or_insert(0);
            *entry += 1;
        }
        fn get_name(&mut self, sym: Symbol) -> Symbol {
            *self.stack.get(&sym).unwrap_or(
                &vec![sym] // if the symbol wasn't declared, it's a class and is reserved...
            ).last().expect("Undefined Name")
        }
        fn restore_stack(&mut self, push_count: &mut HashMap<Symbol, usize>) {
            for (sym, stack) in self.stack.iter_mut() {
                if let Some(amt) = push_count.get(&sym) {
                    stack.truncate(stack.len() - amt);
                }
            }
        }
    }

    pub fn transform(tree: TreeContainer, sm: &mut SymbolManager) -> Allocator {
        let mut allocator = super::graph::build(tree);
        transform_graph(&mut allocator, sm);
        allocator
    }

    // https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/5/
    pub fn transform_graph(alloc: &mut Allocator, sm: &mut SymbolManager) {
        let (doms, dominance_frontier) = dominance(alloc);
        let mut idom: Vec<Vec<usize>> = vec![Vec::new(); alloc.len()];
        for (i, d) in doms.into_iter().enumerate() { if d != i { idom[d].push(i) } }
        let vars = stat(alloc);
        for (v, mut block_set) in vars {
            let mut blocks: Vec<Id> = block_set.clone().into_iter().collect();
            while let Some(vassigned) = blocks.pop() {
                for block in dominance_frontier[vassigned].ones() {
                    insert_phi(v, block, alloc);
                    if !block_set.contains(&block) {
                        block_set.insert(block);
                        blocks.push(block);
                    }
                }
            }
        }
        
        let mut state = State { stack: HashMap::new(), sm };
        rename(0, &mut state, alloc, &idom);
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

    pub fn stat(alloc: &Allocator) -> HashMap<Symbol, HashSet<usize>> {
        let mut mp = HashMap::new();
        for id in 0..alloc.len() {
            let content = &alloc.grab(id).content;
            for tree in content {
                match tree {
                    Tree::LetP(p) => if let Some(pname) = p.name {
                        let id_set = mp.entry(pname).or_insert(HashSet::new());
                        id_set.insert(id);
                    },
                    Tree::Block(_) | Tree::Switch(_) | Tree::LetI(_) | 
                    Tree::Loop(_) | Tree::If(_) | Tree::Return(_) |
                    Tree::Break(_) | Tree::Continue(_) | Tree::EntryPoint(_) => (),
                    _ => panic!("Invalid Tree Type in SSA")
                }
            }
        }
        mp
    }

    pub fn insert_phi(v: Symbol, block: Id, alloc: &mut Allocator) {
        fn phi(v: Symbol) -> Tree {
            Tree::LetP(PrimStatement {
                name: Some(v),
                typ: Typ::Unknown,
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
        node.content.push_front(phi(v));
    }

    // https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/5/
    fn rename(id: Id, state: &mut State, alloc: &mut Allocator, idom: &Vec<Vec<Id>>) {
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
        for &child in &idom[id] { rename(child, state, alloc, idom) }
        state.restore_stack(&mut push_count);
    }

    fn rename_operand(op: &mut Operand, state: &mut State) {
        match op {
            Operand::This(_) | Operand::Super(_) |
            Operand::C(_) | Operand::Tp(_) => (),
            Operand::V(sym) => *sym = state.get_name(*sym),
            Operand::T(ExprTree { op, args }) => match op {
                Operation::InstanceOf | Operation::Throw => todo!(),
                Operation::Ternary => panic!("Ternary Operators should have been hoisted!"),
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
}

mod graph {
    use super::*;
    struct State {
        alloc: Allocator,
        label_map: HashMap<Symbol, Id>,
        break_map: HashMap<Symbol, Id>
    }
    
    impl State {
        pub fn new() -> Self {
            Self {
                alloc: Allocator::new(),
                label_map: HashMap::new(),
                break_map: HashMap::new()
            } 
        }
        pub fn use_label(&mut self, label: Symbol, body: Id, next: Id) {
            self.label_map.insert(label, body).map(|s| panic!("Overwriting label: {:?}", label));
            self.break_map.insert(label, next).map(|s| panic!("Overwriting label: {:?}", label));
        }
        pub fn continue_id_from_label(&self, label: Symbol) -> Id{
            *self.label_map.get(&label).expect(&format!(
                "Label {:?} does not exist in map", label)
            )
        }
        pub fn break_id_from_label(&self, label: Symbol) -> Id{
            *self.break_map.get(&label).expect(&format!(
                "Label {:?} does not exist in map", label)
            )
        }
        pub fn add_children(&mut self, node: &mut CfgNode, children: &[Id]) {
            node.children = children.to_vec();
            for &child in children { self.alloc.add_child(node.id, child) }
        }
        pub fn connect(&mut self, ids: &[Id], target: Id) {
            for &child in ids.as_ref() { self.alloc.add_child(child, target) }
        }
    }

    pub fn build(tree: TreeContainer) -> Allocator {
        let mut state = State::new();
        build_graph(tree.into_iter(), &mut state);
        state.alloc
    }
    
    fn build_graph(mut iter: ContainerIntoIter<Tree>, state: &mut State) -> (Id, Box<[Id]>) {
        let nid = state.alloc.alloc();
        let mut content = TreeContainer::new();
        let tails = Vec::new();
        let mut cur = iter.next();
        while let Some(stmt) = cur {
            let next = iter.clone();
            match stmt {
                Tree::Block(BlockStatement { label, bbody }) => {
                    let (nhead, ntails) = build_graph(next, state);
                    state.use_label(label, nid, nhead);
                    let (bhead, btails) = build_graph(bbody.into_iter(), state);
                    state.connect(&btails, nhead);
                    content.push_back(Tree::Block(BlockStatement {
                        label, bbody: TreeContainer::new()
                    }));
                    state.alloc.set(nid, content, vec![bhead]);
                    return (nid, ntails);
                },
                Tree::Switch(SwitchStatement { arg, label, cases, default }) => {
                    let (nhead, ntails) = build_graph(next, state);
                    state.use_label(label, nid, nhead);
                    let mut ncases = Vec::new();
                    let mut children = Vec::new();
                    let mut prev: Option<Box<[Id]>> = None;
                    for (ops, content) in cases {
                        let (head, tails) = build_graph(content.into_iter(), state);
                        if let Some(p) = prev { state.connect(&p, head) };
                        children.push(head);
                        ncases.push((ops, TreeContainer::new()));
                        prev = Some(tails);
                    }
                    let (dhead, dtails) = build_graph(default.into_iter(), state);
                    if let Some(p) = prev { state.connect(&p, dhead) };
                    state.connect(&dtails, nhead);
                    children.push(dhead);
                    content.push_back(Tree::Switch(SwitchStatement {
                        arg, label, cases: ncases,
                        default: TreeContainer::new(),
                    }));
                    state.alloc.set(nid, content, children);
                    return (nid, ntails);
                },
                Tree::Loop(LoopStatement { cond, label, lbody, dowhile }) => {
                    let (nhead, ntails) = build_graph(next, state);
                    state.use_label(label, state.alloc.len(), nhead);
                    let (lhead, ltails) = build_graph(lbody.into_iter(), state);
                    state.connect(&ltails, nhead);
                    state.connect(&ltails, lhead);
                    content.push_back(Tree::Loop(LoopStatement {
                        cond, label, lbody: TreeContainer::new(), dowhile
                    }));
                    let children = if !dowhile { vec![lhead, nhead] }
                        else { vec![lhead] };
                    state.alloc.set(nid, content, children);
                    return (nid, ntails);
                },
                Tree::If(IfStatement { cond, label, btrue, bfalse }) => {
                    let (nhead, ntails) = build_graph(next, state);
                    state.use_label(label, nid, nhead);
                    let (thead, ttails) = build_graph(btrue.into_iter(), state);
                    let (fhead, ftails) = build_graph(bfalse.into_iter(), state);
                    state.connect(&ttails, nhead);
                    state.connect(&ftails, nhead);
                    content.push_back(Tree::If(IfStatement {
                        cond, label,
                        btrue: TreeContainer::new(),
                        bfalse: TreeContainer::new(),
                    }));
                    state.alloc.set(nid, content, vec![thead, fhead]);
                    return (nid, ntails);
                },
                Tree::Return(r) => {
                    content.push_back(Tree::Return(r));
                    state.alloc.set(nid, content, vec![]);
                    return (nid, tails.into_boxed_slice())
                },
                Tree::Continue(label) => {
                    content.push_back(Tree::Continue(label));
                    state.alloc.set(nid, content, vec![state.continue_id_from_label(label)]);
                    return (nid, tails.into_boxed_slice())
                },
                Tree::Break(label) => {
                    content.push_back(Tree::Break(label));
                    state.alloc.set(nid, content, vec![state.break_id_from_label(label)]);
                    return (nid, tails.into_boxed_slice());
                },
                other => content.push_back(other)
            }
            cur = iter.next();
        }
        state.alloc.set(nid, content, vec![]);
        return (nid, vec![nid].into_boxed_slice());
    }
}


#[cfg(test)]
mod test {
    use tree_sitter::Parser;
    use crate::ir::*;
    use crate::converter::convert;
    use crate::hoist::hoist;
    use crate::parameters::Parameters;
    use crate::symbolmanager::SymbolManager;
    use crate::typeinfer::typeinfer;
    use super::print_graph;

    #[test]
    pub fn f() {
        let text = r#"
        public class Test {
            public static void main(String[] args) {
                int cnt = 0;
                while (cnt++ < 50) {
                    switch (cnt % 5) {
                        case 0: do { cnt++; } while (cnt < 10);
                        case 1: break;
                        case 2: do { cnt++; } while (cnt < 1);;
                        case 3: continue;
                        case 4: cnt *= cnt;
                    }
                }
                System.out.println(cnt);
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
        match ast.as_ref() {
            Tree::Program(p) => match p.iter().next().expect("") {
                Tree::LetC(c) => match c.methods.back().expect("") {
                    Tree::LetF(f) => {
                        let graph = super::transform::transform(f.body.clone(), &mut sm);
                        print_graph(&graph.nodes, &sm);
                        super::transform::verify(&graph, &sm).expect("Valid SSA")
                    },
                    _ => panic!("Missing Function"),
                },
                _ => panic!("Missing Class"),
            },
            _ => panic!("Missing Program")
        }
        assert!(true)
    }
}