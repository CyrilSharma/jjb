use std::collections::HashMap;
use fixedbitset::FixedBitSet;

use crate::printer::str_print;
use crate::container::ContainerIntoIter;
use crate::symbolmanager::{Symbol, SymbolManager};
use crate::ir::*;

pub type Id = usize;
#[derive(Clone)]
pub struct CfgNode {
    pub id: Id,
    pub content: TreeContainer,
    pub children: Vec<Id>,
    pub preds: Vec<Id>
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

pub struct Allocator { pub nodes: Vec<CfgNode> }
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

struct State {
    alloc: Allocator,
    label_map: HashMap<Symbol, Id>,
    break_map: HashMap<Symbol, Id>,
    next_map: HashMap<Id, Id>
}

impl State {
    pub fn new() -> Self {
        Self {
            alloc: Allocator::new(),
            label_map: HashMap::new(),
            break_map: HashMap::new(),
            next_map: HashMap::new()
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

pub fn build(tree: TreeContainer) -> (Allocator, HashMap<Id, Id>) {
    let mut state = State::new();
    build_graph(tree.into_iter(), &mut state);
    (state.alloc, state.next_map)
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
                state.next_map.insert(nid, nhead);
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
                state.next_map.insert(nid, nhead);
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
                state.next_map.insert(nid, nhead);
                state.use_label(label, state.alloc.len(), nhead);
                let (lhead, ltails) = build_graph(lbody.into_iter(), state);
                state.connect(&ltails, nhead);
                state.connect(&ltails, lhead);
                content.push_back(Tree::Loop(LoopStatement {
                    cond, label, lbody: TreeContainer::new(), dowhile
                }));
                state.alloc.set(nid, content, vec![lhead]);
                return (nid, ntails);
            },
            Tree::If(IfStatement { cond, label, btrue, bfalse }) => {
                let (nhead, ntails) = build_graph(next, state);
                state.next_map.insert(nid, nhead);
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

pub fn fold(mut allocator: Allocator, next_map: &HashMap<Id, Id>) -> TreeContainer {
    fold_graph(0, &mut allocator, next_map)
}

fn fold_graph(id: Id, allocator: &mut Allocator, next_map: &HashMap<Id, Id>) -> TreeContainer {
    let node = std::mem::take(&mut allocator.nodes[id]);
    let mut content = node.content;
    let children = node.children;
    if let Some(s) = content.back_mut() {
        match s {
            Tree::Block(b) => {
                b.bbody = fold_graph(children[0], allocator, next_map);
            },
            Tree::Switch(s) => {
                let mut i = 0;
                while i < s.cases.len() {
                    s.cases[i].1 = fold_graph(children[i], allocator, next_map);
                    i += 1;
                }
                s.default = fold_graph(children[i], allocator, next_map);
            }
            Tree::Loop(l) => {
                l.lbody = fold_graph(children[0], allocator, next_map);
            },
            Tree::If(istmt) => {
                istmt.btrue = fold_graph(children[0], allocator, next_map);
                istmt.bfalse = fold_graph(children[1], allocator, next_map);
            },
            Tree::Try(_) => todo!(),
            _ => ()
        }
    }
    if let Some(n) = next_map.get(&id) {
        content.append(fold_graph(*n, allocator, next_map));
    }
    content
}

pub fn dominance(alloc: &Allocator) -> Vec<Id> {
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
    doms
}

pub fn dominance_frontier(doms: &Vec<Id>, alloc: &Allocator) -> Vec<FixedBitSet> {
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
    df
}

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