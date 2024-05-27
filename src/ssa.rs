use std::collections::HashMap;
use crate::{container::ContainerIntoIter, ir::*, symbolmanager::Symbol};

type Id = usize;
struct CfgNode {
    id: Id,
    content: TreeContainer,
    children: Vec<Id>,
    parent: Id
}

impl Default for CfgNode {
    fn default() -> Self {
        Self {
            id: Default::default(),
            content: TreeContainer::new(),
            children: Default::default(),
            parent: Default::default()
        }
    }
}

struct Allocator {
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
    pub fn set(&mut self, id: Id, content: TreeContainer, children: Vec<Id>) {
        self.nodes[id] = CfgNode { parent: 0, id, content, children };
    }
    pub fn grab(&mut self, id: Id) -> &mut CfgNode {
        &mut self.nodes[id]
    }
    pub fn add_child(&mut self, parent: Id, child: Id) {
        let p = self.grab(parent);
        p.children.push(child);
        let c = self.grab(child);
        c.parent = parent;
    }
}

mod build {
    use super::*;
    struct State<'l> {
        alloc: &'l mut Allocator,
        label_map: HashMap<Symbol, Id>
    }
    
    impl<'l> State<'l> {
        pub fn use_label(&mut self, label: Symbol, body: Id) {
            self.label_map.insert(label, body).map(|s| panic!("Overwriting label: {:?}", label));
        }
        pub fn id_from_label(&self, label: Symbol) -> Id{
            *self.label_map.get(&label).expect(&format!(
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
    
    fn build_graph(mut iter: ContainerIntoIter<Tree>, state: &mut State) -> (Id, Box<[Id]>) {
        let mut nid = state.alloc.alloc();
        let mut content = TreeContainer::new();
        let mut tails = Vec::new();
        let mut cur = iter.next();
        while let Some(stmt) = cur {
            let next = iter.clone();
            match stmt {
                Tree::Block(BlockStatement { label, bbody }) => {
                    state.use_label(label, nid);
                    let (nhead, ntails) = build_graph(next, state);
                    let (bhead, btails) = build_graph(bbody.into_iter(), state);
                    state.connect(&btails, nhead);
                    content.push_back(Tree::Block(BlockStatement {
                        label, bbody: TreeContainer::new()
                    }));
                    state.alloc.set(nid, content, vec![nhead]);
                    return (nid, ntails);
                },
                Tree::Switch(SwitchStatement { arg, label, cases, default }) => {
                    state.use_label(label, nid);
                    let (nhead, ntails) = build_graph(next, state);
                    let mut ncases = Vec::new();
                    let mut children = Vec::new();
                    for (ops, content) in cases {
                        let (head, tails) = build_graph(content.into_iter(), state);
                        state.connect(&tails, nhead);
                        children.push(head);
                        ncases.push((ops, TreeContainer::new()));
                    }
                    let (dhead, dtails) = build_graph(default.into_iter(), state);
                    for &child in dtails.as_ref() { state.alloc.add_child(child, nhead) }
                    state.alloc.set(nid, content, children);
                    return (nid, ntails);
                },
                Tree::Loop(LoopStatement { cond, label, lbody, dowhile }) => {
                    state.use_label(label, nid);
                    let (nhead, ntails) = build_graph(next, state);
                    let (lhead, ltails) = build_graph(lbody.into_iter(), state);
                    state.connect(&ltails, nhead);
                    content.push_back(Tree::Loop(LoopStatement {
                        cond, label, lbody: TreeContainer::new(), dowhile
                    }));
                    let children = if !dowhile { vec![lhead, nhead] }
                        else { vec![lhead] };
                    state.alloc.set(nid, content, children);
                    return (nid, ntails);
                },
                Tree::If(IfStatement { cond, label, btrue, bfalse }) => {
                    state.use_label(label, nid);
                    let (nhead, ntails) = build_graph(next, state);
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
                    state.alloc.set(nid, content, vec![state.id_from_label(label)]);
                    return (nid, tails.into_boxed_slice())
                },
                Tree::Break(label) => {
                    content.push_back(Tree::Break(label));
                    state.alloc.set(nid, content, vec![]);
                    return (nid, vec![nid].into_boxed_slice());
                },
                other => content.push_back(other)
            }
            cur = iter.next();
        }
        state.alloc.set(nid, content, vec![]);
        return (nid, vec![nid].into_boxed_slice());
    }
}