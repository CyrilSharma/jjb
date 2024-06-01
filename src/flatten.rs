use std::collections::HashMap;
use std::collections::HashSet;
use crate::graph::Allocator;
use crate::graph::Id;
use crate::optimizer;
use crate::symbolmanager;
use crate::{graph, ir::*};
use crate::symbolmanager::{Symbol, SymbolManager};
use std::hash::Hash;

#[derive(Clone, PartialEq, Debug)]
struct Set<T: Eq + Hash + Clone> {
    inner: HashSet<T>
}
impl<T: Eq + Hash + Clone> Set<T> {
    fn new() -> Self {
        Set { inner: HashSet::new() }
    }

    fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value)
    }

    fn contains(&self, value: &T) -> bool {
        self.inner.contains(value)
    }

    fn union(&mut self, other: &Set<T>) {
        for item in &other.inner {
            self.inner.insert(item.clone());
        }
    }

    fn remove(&mut self, other: &Set<T>) {
        for item in &other.inner {
            self.inner.remove(item);
        }
    }
}

// Implementing IntoIterator for borrowing the set (immutable)
impl<'a, T: Eq + Hash + Clone> IntoIterator for &'a Set<T> {
    type Item = &'a T;
    type IntoIter = std::collections::hash_set::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

// Implementing IntoIterator for owning the set (consuming it)
impl<T: Eq + Hash + Clone> IntoIterator for Set<T> {
    type Item = T;
    type IntoIter = std::collections::hash_set::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

// Implementing FromIterator to create Set from iterator
impl<T: Eq + Hash + Clone> FromIterator<T> for Set<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = Set::new();
        for item in iter {
            set.insert(item);
        }
        set
    }
}

pub fn flatten(tree: Tree, sm: &mut SymbolManager) -> Tree {
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| flatten(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let t = Tree::LetF(f);
            let census = optimizer::census::census(&t);
            let f = match t { Tree::LetF(f) => f, _ => panic!() };
            let (mut allocator, next_map) = graph::build(f.body);
            flatten_graph(&mut allocator, census, sm);
            FunDeclaration {
                body: graph::fold(allocator, &next_map),
                ..f
            }
        }),
        Tree::LetC(c) => Tree::LetC(ClassDeclaration {
            methods: c.methods.into_iter().map(|f| flatten(f, sm)).collect(),
            ..c
        }),
        Tree::LetE(_) => todo!(),
        Tree::EntryPoint(e) => Tree::EntryPoint(e),
        _ => panic!("Invalid tree in flatten")
    }
}

fn flatten_graph(alloc: &mut Allocator, mut census: HashMap<Symbol, usize>, sm: &mut SymbolManager) {
    fn dfs(node: Id, v: &mut Vec<Id>, alloc: &Allocator, visited: &mut Vec<bool>) {
        visited[node] = true;
        let cfg = alloc.grab(node);
        for &child in &cfg.children {
            if visited[child] { continue }
            dfs(child, v, alloc, visited)
        }
        v.push(node);
    }

    let mut count = 0;
    let mut idtosym = HashMap::new();
    let mut symtoids = HashMap::new();
    let mut gen = vec![Set::new(); alloc.len()];
    for id in 0..alloc.len() {
        let node = alloc.grab(id);
        for child in &node.content {
            match child {
                Tree::LetP(p) => if let Some(n) = p.name {
                    gen[id].insert(count);
                    idtosym.insert(count, n);
                    let entry = symtoids.entry(n).or_insert(Set::new());
                    entry.insert(count);
                },
                Tree::Try(_) => todo!(),
                _ => ()
            }
            count += 1;
        }
    }

    let mut postorder = Vec::new();
    postorder.reserve(alloc.len());
    dfs(0, &mut postorder, alloc, &mut vec![false; alloc.len()]);
    let mut defs = vec![Set::new(); alloc.len()];
    loop {
        let mut changed = false;
        for &node in postorder.iter().rev() {
            let preds = &alloc.grab(node).preds;
            let mut new_defs = Set::new();
            for &p in preds { new_defs.union(&defs[p]) }
            for &id in &gen[node] {
                let sym = idtosym[&id];
                new_defs.remove(&symtoids[&sym]);
            }
            new_defs.union(&gen[node]);
            if defs[node] != new_defs {
                defs[node] = new_defs;
                changed = true;
            }
        }
        if !changed { break }
    }

    graph::print_graph(&alloc.nodes, sm);
    // println!("defs =>");
    // for (i, vec) in defs.iter().enumerate() {
    //     println!("--> block {}", i);
    //     for item in vec {
    //         println!("----- item: {}", item);
    //     }
    // }

    for iter in 0..4 {
        let mut count = 0;
        let mut values = HashMap::new();
        for node in 0..alloc.len() {
            let mut predset = Set::new();
            let mut symtodefs = HashMap::new();
            for &pred in &alloc.grab(node).preds {
                predset.union(&defs[pred]);
            }

            for &id in &predset {
                let sym = idtosym[&id];
                let entry = symtodefs.entry(sym).or_insert(vec![]);
                entry.push(id);
            }

            if iter == 0 {
                println!("symtodefs {} =>", node);
                for (&sym, id) in &symtodefs {
                    println!("-> sym: {}", sm.uname(sym));
                    for item in id {
                        println!("---- item: {}", item);
                    }
                }
            }

            let node = alloc.grab_mut(node);
            let mut res = TreeContainer::new();
            while let Some(mut item) = node.content.pop_front() {
                match item {
                    Tree::LetP(ref mut p) => match (p.name, p.exp.as_mut()) {
                        (None, None) => panic!("Invalid Primitive"),
                        (None, Some(e)) => {
                            inline(e, &mut census, &values, &symtodefs);
                            res.push_back(item);
                        },
                        (Some(n), None) => {
                            symtodefs.remove(&n);
                            symtodefs.insert(n, vec![count]);
                            res.push_back(item)
                        },
                        (Some(n), Some(e)) => {
                            symtodefs.remove(&n);
                            symtodefs.insert(n, vec![count]);
                            if let Some(1) = census.get(&n) {
                                inline(e, &mut census, &values, &symtodefs);
                                values.insert(count, e.clone());
                                res.push_back(item)
                            } else if let Some(0) = census.get(&n) {
                                
                            } else {
                                inline(e, &mut census, &values, &symtodefs);
                                res.push_back(item)
                            }
                        },
                    },
                    Tree::Block(_) => res.push_back(item),
                    Tree::Switch(ref mut s) => {
                        inline(&mut s.arg, &mut census, &values, &symtodefs);
                        res.push_back(item)
                    }
                    Tree::Loop(ref mut l) => {
                        inline(&mut l.cond, &mut census, &values, &symtodefs);
                        res.push_back(item)
                    },
                    Tree::If(ref mut i) => {
                        inline(&mut i.cond, &mut census, &values, &symtodefs);
                        res.push_back(item)
                    },
                    Tree::Return(ref mut r) => {
                        r.val.as_mut().map(|e| inline(e, &mut census, &values, &symtodefs));
                        res.push_back(item)
                    },
                    Tree::Break(_) => res.push_back(item),
                    Tree::Continue(_) => res.push_back(item),
                    Tree::Try(_) => todo!(),
                    other => res.push_back(other)
                }
                count += 1;
            }
            node.content = res;
        }
    }
}

fn inline(
    e: &mut Operand,
    census: &mut HashMap<Symbol, usize>,
    values: &HashMap<Id, Operand>,
    symtodefs: &HashMap<Symbol, Vec<Id>>) {
    match e {
        Operand::This(_) | Operand::Super(_) |
        Operand::C(_) | Operand::Tp(_) => (),
        Operand::V(sym) => {
            let res = symtodefs.get(&sym);
            if let Some(defs) = res {
                if defs.len() != 1 { return }
                let cnt = *census.get(&sym).unwrap_or(&0);
                if cnt > 1 { return }
                if let Some(v) = values.get(&defs[0]) {
                    if let Some(el) = census.get_mut(&sym) {
                        *el -= 1;
                    }
                    *e = v.clone();
                }
            }
        },
        Operand::T(ExprTree { args, op: Operation::New }) => {
            args[1..].iter_mut().for_each(|e| inline(e, census, values, symtodefs))
        }
        Operand::T(ExprTree { args, op: Operation::InvokeVirtual | Operation::InvokeStatic }) => {
            inline(&mut args[0], census, values, symtodefs);
            args[2..].iter_mut().for_each(|e| inline(e, census, values, symtodefs))
        },
        Operand::T(ExprTree { args, .. }) => 
            args.iter_mut().for_each(|e| inline(e, census, values, symtodefs)),
        Operand::A(a) => match a {
            ArrayExpression::Empty(v) => v.iter_mut().for_each(|e| inline(e, census, values, symtodefs)),
            ArrayExpression::Initializer(a) => inline_array(a, census, values, symtodefs)
        }
    }
}

fn inline_array(
    a: &mut ArrayInitializer,
    census: &mut HashMap<Symbol, usize>,
    values: &HashMap<Id, Operand>,
    symtodefs: &HashMap<Symbol, Vec<Id>>) {
    for item in a {
        match item.as_mut() {
            ElementInitializer::Expr(e) => inline(e, census, values, symtodefs),
            ElementInitializer::ArrayInitializer(child) => inline_array(child, census, values, symtodefs)
        }
    }
}


#[cfg(test)]
mod test {
    use tree_sitter::Parser;
    use crate::{ir::*, ssa};
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
                int a=1, b=-2, c=3;
                for (int it = 0; it < 10; it++) {
                    a += b;
                    b -= c;
                }
                System.out.printf(
                    "%d %d %d\n", a, b, c
                );
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
        ast = Box::new(ssa::revert(*ast, &mut sm));
        print(&ast, &sm);
        ast = Box::new(super::flatten(*ast, &mut sm));
        print(&ast, &sm);
        assert!(false)
    }
}