use std::collections::HashMap;
use std::collections::HashSet;
use crate::graph::Allocator;
use crate::graph::Id;
use crate::optimizer;
use crate::{graph, ir::*};
use crate::symbolmanager::{Symbol, SymbolManager};
use std::hash::Hash;

#[derive(Clone, PartialEq)]
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
            // ... this is why we can't have nice things.
            let f = match t { Tree::LetF(f) => f, _ => panic!() };
            let (mut allocator, next_map) = graph::build(f.body);
            flatten_graph(&mut allocator, census);
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

fn flatten_graph(alloc: &mut Allocator, census: HashMap<Symbol, usize>) {
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
                    entry.insert(id);
                },
                Tree::Try(_) => todo!(),
                _ => ()
            }
            count += 1;
        }
    }


    // TODO: should defs be indexed by symbol as well?
    // Think on it. Once we know all definitions that correspond to a symbol
    let mut postorder = Vec::new();
    postorder.reserve(alloc.len());
    dfs(0, &mut postorder, alloc, &mut vec![false; alloc.len()]);
    let mut defs = vec![Set::new(); alloc.len()];
    loop {
        let mut changed = false;
        for &node in postorder.iter().rev().skip(1) {
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

    let mut count = 0;
    let mut values = HashMap::new();
    for node in 0..alloc.len() {
        let mut symtodefs = HashMap::new();
        for &id in &defs[node] {
            let sym = idtosym[&id];
            let entry = symtodefs.entry(sym).or_insert(vec![]);
            entry.push(id);
        }

        // This would be better if we could just remove the nodes that changed.
        // But that's part of the unstable api...
        let node = alloc.grab_mut(node);
        let mut newcontent = TreeContainer::new();
        for child in node.content.iter_mut() {
            match child {
                Tree::LetP(p) => 'body: {
                    if let Some(e) = &mut p.exp { inline(e, &census, &values, &symtodefs) }
                    if let Some(n) = &p.name {
                        let cnt = *census.get(&n).unwrap_or(&0);
                        if cnt <= 1 {
                            p.exp.as_ref().map(|e| values.insert(count, e.clone()));
                            break 'body;
                        }
                    }
                    newcontent.push_back(child.clone())
                }
                Tree::Try(_) => todo!(),
                other => newcontent.push_back(other.clone())
            }
            count += 1;
        }
        node.content = newcontent;
    }
}

fn inline(
    e: &mut Operand,
    census: &HashMap<Symbol, usize>,
    values: &HashMap<Id, Operand>,
    symtodefs: &HashMap<Symbol, Vec<Id>>) {
    match e {
        Operand::This(_) | Operand::Super(_) |
        Operand::C(_) | Operand::Tp(_) => (),
        Operand::V(sym) => {
            let defs = symtodefs.get(&sym).expect("Sym To Defs missing Symbol");
            if defs.len() != 1 { return }
            let cnt = *census.get(&sym).unwrap_or(&0);
            if cnt > 1 { return }
            *e = values.get(&defs[0]).expect("Values missing Symbol").clone()
        },
        Operand::T(ExprTree { args, .. }) => args.iter_mut().for_each(|e| inline(e, census, values, symtodefs)),
        Operand::A(a) => match a {
            ArrayExpression::Empty(v) => v.iter_mut().for_each(|e| inline(e, census, values, symtodefs)),
            ArrayExpression::Initializer(a) => inline_array(a, census, values, symtodefs)
        }
    }
}

fn inline_array(
    a: &mut ArrayInitializer,
    census: &HashMap<Symbol, usize>,
    values: &HashMap<Id, Operand>,
    symtodefs: &HashMap<Symbol, Vec<Id>>) {
    for item in a {
        match item.as_mut() {
            ElementInitializer::Expr(e) => inline(e, census, values, symtodefs),
            ElementInitializer::ArrayInitializer(child) => inline_array(child, census, values, symtodefs)
        }
    }
}
