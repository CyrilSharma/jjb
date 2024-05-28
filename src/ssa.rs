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

pub struct Allocator {
    nodes: Vec<CfgNode>
}

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
        self.nodes[id] = CfgNode { preds: Vec::new(), id, content, children };
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
        idom: Vec<Id>,
        sm: &'l mut SymbolManager,
    }

    impl<'l> State<'l> {
        fn add_name(&self, sym: Symbol, nname: Symbol) {
            let entry = self.stack.entry(sym).or_insert(Vec::new());
            entry.push(nname);
        }
    }

    pub fn transform(tree: TreeContainer, sm: &mut SymbolManager) {
        let mut allocator = Allocator::new();
        super::graph::build(tree, &mut allocator);
        transform_graph(&mut allocator, sm);
    }

    // https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/5/
    pub fn transform_graph(alloc: &mut Allocator, sm: &mut SymbolManager) {
        let (idom, dominance_frontier) = dominance(alloc);
        let mut vars = HashMap::new();
        stat(0, alloc, &mut vars);
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
        
        let mut state = State {
            idom,
            stack: HashMap::new(),
            sm,
        };
        
    }

    pub fn insert_phi(v: Symbol, block: Id, alloc: &Allocator) {
        fn phi(v: Symbol) -> Tree {
            Tree::LetP(PrimStatement {
                name: v,
                typ: Typ::Void,
                exp: Some(Operand::T(ExprTree {
                    op: Operation::Phi,
                    args: vec![Operand::V(v)],
                })),
            })
        }
        
        fn mutate_head(head: &mut Tree, v: Symbol) -> bool {
            if let Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree { op: Operation::Phi, args })),
                name: sym,
                ..
            }) = head {
                if *sym != v { return false; }
                args.push(Operand::V(v));
                return true;
            }
            return false;
        }

        let node = alloc.grab_mut(block);
        if let Some(head) = node.content.iter_mut().next() {
            let had_phi = mutate_head(head, v);
            if had_phi { return }
        }
        node.content.push_front(phi(v));
    }

    pub fn rename(id: Id, state: &mut State, alloc: &mut Allocator) {
        let content = alloc.grab_mut(id).content.iter_mut();
        for tree in content {
            match tree {
                Tree::LetP(p) => {
                    match p.exp {
                        Some(op) => todo!(),
                        _ => ()
                    }
                    let nname = state.sm.refresh(&p.name);
                    state.add_name(p.name, nname);
                    p.name = nname;
                },
                Tree::Switch(s) => todo!(),
                Tree::If(i) => todo!(),
                Tree::Return(r) => match r.val {
                    Some(op) => todo!(),
                    _ => ()
                },
                Tree::Loop(_) => todo!(),
                Tree::LetI(_) | Tree::Break(_) |
                Tree::Continue(_) | Tree::EntryPoint(_) => (),
                Tree::Try(_) => todo!(),
                _ => panic!("Invalid Tree Type in SSA")
            }
        }

        let children = alloc.grab(id).children.clone();
        for child in children {
            if state.idom[child] != id { continue }
            rename(child, state, alloc);
        }

        // Some stack popping.
        todo!()
    }

    pub fn stat(id: Id, alloc: &Allocator, mp: &mut HashMap<Symbol, HashSet<Id>>) {
        let content = &alloc.grab(id).content;
        for tree in content {
            match tree {
                Tree::LetP(p) => {
                    let id_set = mp.entry(p.name).or_insert(HashSet::new());
                    id_set.insert(id);
                },
                Tree::Switch(_) | Tree::Loop(_) |
                Tree::If(_) | Tree::Return(_) |
                Tree::LetI(_) | Tree::Break(_) |
                Tree::Continue(_) | Tree::EntryPoint(_) => (),
                Tree::Try(_) => todo!(),
                _ => panic!("Invalid Tree Type in SSA")
            }
        }
        for &child in &alloc.grab(id).children {
            stat(child, alloc, mp);
        }
    }

    pub fn dominance(alloc: &Allocator) -> (Vec<Id>, Vec<FixedBitSet>) {
        fn dfs(node: Id, v: &mut Vec<Id>, alloc: &Allocator) {
            let cfg = alloc.grab(node);
            for &child in &cfg.children { dfs(child, v, alloc) }
            v.push(node);
        }

        // https://web.archive.org/web/20210422111834/https://www.cs.rice.edu/~keith/EMBED/dom.pdf
        fn intersect(a: Id, b: Id, doms: &Vec<usize>) -> Id {
            let mut fingera = a;
            let mut fingerb = b;
            while fingera != fingerb {
                while fingera < fingerb {
                    fingera = doms[fingera];
                }
                while fingerb < fingera {
                    fingerb = doms[fingerb];
                }
            }
            return fingera;
        }

        // N^2 but with incredible constant factor.
        let undefined: usize = alloc.len(); 
        let (mut postorder, mut index) = (Vec::new(), Vec::new());
        postorder.reserve(alloc.len());
        index.reserve(alloc.len());
        dfs(0, &mut postorder, alloc);
        for (i, p) in postorder.iter().enumerate() {
            index[*p] = i;
        }
        let mut doms = vec![undefined; alloc.len()];
        doms[0] = 0;
        loop {
            let mut changed = false;
            for &node in postorder.iter().rev().skip(1) {
                let preds = &alloc.grab(node).preds;
                let mut new_idom = undefined;
                for &p in preds {
                    if doms[p] == undefined { continue }
                    if new_idom == undefined { new_idom = index[p]; continue }
                    new_idom = intersect(index[p], index[new_idom], &doms);
                }
                if doms[node] != new_idom {
                    doms[node] = new_idom;
                    changed = true;
                }
            }
            if !changed { break }
        }

        let mut df: Vec<FixedBitSet> = Vec::new();
        for b in 0..alloc.len() {
            if alloc.grab(b).children.len() < 2 { continue; } 
            for &p in &alloc.grab(b).preds {
                let mut runner = p;
                while runner != doms[b] {
                    df[runner].set(b, true);
                    runner = doms[runner];
                }
            }
        }
        (doms, df)
    }
    
}

mod graph {
    use super::*;
    struct State<'l> {
        alloc: &'l mut Allocator,
        label_map: HashMap<Symbol, Id>,
        break_map: HashMap<Symbol, Id>
    }
    
    impl<'l> State<'l> {
        pub fn new(alloc: &'l mut Allocator) -> Self {
            Self { alloc, label_map: HashMap::new(), break_map: HashMap::new() } 
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

    pub fn build(tree: TreeContainer, alloc: &mut Allocator) -> Vec<CfgNode> {
        let mut state = State::new(alloc);
        build_graph(tree.into_iter(), &mut state);
        alloc.nodes.clone()
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
                    state.use_label(label, nid, nhead);
                    let (lhead, ltails) = build_graph(lbody.into_iter(), state);
                    state.connect(&ltails, nhead);
                    content.push_back(Tree::Loop(LoopStatement {
                        cond, label, lbody: TreeContainer::new(), dowhile
                    }));
                    let children = vec![lhead];
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
    use crate::optimizer::optimize;
    use crate::parameters::Parameters;
    use crate::symbolmanager::SymbolManager;
    use crate::typeinfer::typeinfer;

    use super::{print_graph, Allocator};

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
        // ast = optimize(ast.as_ref(), &mut sm);
        typeinfer(ast.as_mut(), &mut sm);
        match ast.as_ref() {
            Tree::Program(p) => match p.iter().next().expect("") {
                Tree::LetC(c) => match c.methods.back().expect("") {
                    Tree::LetF(f) => {
                        let mut alloc = Allocator::new();
                        let graph = super::graph::build(f.body.clone(), &mut alloc);
                        print_graph(&graph, &sm);
                    },
                    other => panic!("Missing Function"),
                },
                other => panic!("Missing Class"),
            },
            other => panic!("Missing Program")
        }
        assert!(true)
        
    }
}