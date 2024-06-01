use std::collections::HashMap;
use crate::optimizer;
use crate::ir::*;
use crate::symbolmanager::{Symbol, SymbolManager};

pub fn flatten(tree: Tree, sm: &mut SymbolManager) -> Tree {
    let census = optimizer::census::census(&tree);
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| flatten(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            FunDeclaration {
                body: flattenlist(f.body, &census),
                name: f.name,
                args: f.args,
                modifiers: f.modifiers,
                throws: f.throws,
                return_typ: f.return_typ,
                constructor: f.constructor
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

fn flattenlist(mut list: TreeContainer, census: &HashMap<Symbol, usize>) -> TreeContainer {
    let mut res = TreeContainer::new();
    let mut values = HashMap::new();
    while let Some(mut item) = list.pop_front() {
        match item {
            Tree::LetP(ref mut p) => match (p.name, p.exp.as_mut()) {
                (None, None) => panic!("Invalid Primitive"),
                (None, Some(e)) => {
                    inline(e, census, &values);
                    res.push_back(item);
                },
                (Some(_), None) => res.push_back(item),
                (Some(n), Some(e)) => {
                    if let Operand::T(ExprTree { op: Operation::Phi, .. }) = e {
                        // inline(e, census, &values);
                        res.push_back(item)
                    } else if let Some(1) = census.get(&n) {
                        inline(e, census, &values);
                        values.insert(n, e.clone());
                    } else {
                        inline(e, census, &values);
                        res.push_back(item)
                    }
                },
            }
            Tree::Block(_) => res.push_back(item),
            Tree::Switch(ref mut s) => {
                inline(&mut s.arg, census, &values);
                res.push_back(item)
            }
            Tree::Loop(ref mut l) => {
                inline(&mut l.cond, census, &values);
                res.push_back(item)
            },
            Tree::If(ref mut i) => {
                inline(&mut i.cond, census, &values);
                res.push_back(item)
            },
            Tree::Try(_) => todo!(),
            Tree::Return(ref mut r) => {
                r.val.as_mut().map(|e| inline(e, census, &values));
                res.push_back(item)
            },
            Tree::Break(_) => res.push_back(item),
            Tree::Continue(_) => res.push_back(item),
            _ => panic!("Invalid tree in flattenlist")
        }
    }
    res
}

fn inline(
    e: &mut Operand,
    census: &HashMap<Symbol, usize>,
    values: &HashMap<Symbol, Operand>) {
    match e {
        Operand::This(_) | Operand::Super(_) |
        Operand::C(_) | Operand::Tp(_) => (),
        Operand::V(sym) => {
            let cnt = *census.get(&sym).unwrap_or(&0);
            if cnt > 1 { return }
            *e = values.get(&sym).expect("Values missing Symbol").clone()
        },
        Operand::T(ExprTree { args, .. }) => args.iter_mut().for_each(|e| inline(e, census, values)),
        Operand::A(a) => match a {
            ArrayExpression::Empty(v) => v.iter_mut().for_each(|e| inline(e, census, values)),
            ArrayExpression::Initializer(a) => inline_array(a, census, values)
        }
    }
}

fn inline_array(
    a: &mut ArrayInitializer,
    census: &HashMap<Symbol, usize>,
    values: &HashMap<Symbol, Operand>) {
    for item in a {
        match item.as_mut() {
            ElementInitializer::Expr(e) => inline(e, census, values),
            ElementInitializer::ArrayInitializer(child) => inline_array(child, census, values)
        }
    }
}
