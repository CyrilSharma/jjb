use std::collections::HashMap;
use crate::symbolmanager::SymbolManager;
use crate::parameters::Parameters;
use crate::printer::str_print;
use crate::container::ContainerIntoIter;
use crate::ir::*;
use crate::symbolmanager::Symbol;

type Id = usize;
#[derive(Clone)]
pub struct CfgNode {
    id: Id,
    content: TreeContainer,
    children: Vec<Id>,
    parent: Id
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
        pub fn new(alloc: &'l mut Allocator) -> Self {
            Self { alloc, label_map: HashMap::new() } 
        }
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
                    state.use_label(label, nid);
                    let (nhead, ntails) = build_graph(next, state);
                    let (bhead, btails) = build_graph(bbody.into_iter(), state);
                    state.connect(&btails, nhead);
                    content.push_back(Tree::Block(BlockStatement {
                        label, bbody: TreeContainer::new()
                    }));
                    state.alloc.set(nid, content, vec![bhead]);
                    return (nid, ntails);
                },
                Tree::Switch(SwitchStatement { arg, label, cases, default }) => {
                    state.use_label(label, nid);
                    let (nhead, ntails) = build_graph(next, state);
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
                        let graph = super::build::build(f.body.clone(), &mut alloc);
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