use std::collections::HashMap;
use crate::optimizer;
use crate::ir::*;
use crate::printer;
use crate::symbolmanager::{Symbol, SymbolManager};

pub fn flatten(tree: Tree, sm: &mut SymbolManager) -> Tree {
    let mut census = optimizer::census::census(&tree);
    match tree {
        Tree::Program(stmts) => Tree::Program(
            stmts.into_iter().map(|f| flatten(f, sm)).collect()
        ),
        Tree::LetI(i) => Tree::LetI(i),
        Tree::LetF(f) => Tree::LetF({
            let mut body = f.body;
            for _ in 0..5 { body = flattenlist(body, &mut census, sm) }
            FunDeclaration {
                body,
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

fn flattenlist(mut list: TreeContainer, census: &mut HashMap<Symbol, usize>, sm: &mut SymbolManager) -> TreeContainer {
    let mut res = TreeContainer::new();
    let mut values = HashMap::new();
    while let Some(mut item) = list.pop_front() {
        match item {
            Tree::LetP(ref mut p) => {                
                match (p.name, p.exp.as_mut()) {
                (None, None) => panic!("Invalid Primitive"),
                (None, Some(e)) => {
                    inline(e, census, &values);
                    res.push_back(item);
                },
                (Some(_), None) => res.push_back(item),
                (Some(n), Some(e)) => {
                    // println!("n: {}, census.get(n) = {:?}", sm.uname(n), census.get(&n));
                    // WARNING. CHECK TO MAKE SURE THE OPERATION IS STABLE AND PURE.
                    if let Operand::T(ExprTree { op: Operation::Phi, .. }) = e {
                        // inline(e, census, &values);
                        res.push_back(item)
                    } else if let Some(1) = census.get(&n) {
                        inline(e, census, &values);
                        values.insert(n, e.clone());
                        res.push_back(item)
                    } else if let Some(0) = census.get(&n) {

                    } else {
                        inline(e, census, &values);
                        res.push_back(item)
                    }
                },
            }}
            Tree::Block(BlockStatement { label, mut bbody }) => {
                bbody = flattenlist(bbody, census, sm);
                res.push_back(Tree::Block(BlockStatement { label, bbody }))
            }
            Tree::Switch(SwitchStatement { mut arg, label, cases, mut default }) => {
                inline(&mut arg, census, &values);
                let mut newcases = Vec::new();
                for (ops, case) in cases.into_iter() {
                    newcases.push((ops, flattenlist(case, census, sm)))
                }
                default = flattenlist(default, census, sm);
                res.push_back(Tree::Switch(SwitchStatement { arg, label, cases: newcases, default }))
            }
            Tree::Loop(LoopStatement { mut cond, label, mut lbody, dowhile }) => {
                inline(&mut cond, census, &values);
                lbody = flattenlist(lbody, census, sm);
                res.push_back(Tree::Loop(LoopStatement { cond, label, lbody, dowhile }))
            },
            Tree::If(IfStatement { mut cond, label, mut btrue, mut bfalse }) => {
                inline(&mut cond, census, &values);
                btrue = flattenlist(btrue, census, sm);
                bfalse = flattenlist(bfalse, census, sm);
                res.push_back(Tree::If(IfStatement { cond, label, btrue, bfalse }))
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
    census: &mut HashMap<Symbol, usize>,
    values: &HashMap<Symbol, Operand>) {
    match e {
        Operand::This(_) | Operand::Super(_) |
        Operand::C(_) | Operand::Tp(_) => (),
        Operand::V(sym) => {
            let cnt = *census.get(&sym).unwrap_or(&0);
            if cnt > 1 { return }
            if let Some(v) = values.get(&sym) {
                if let Some(cnt) = census.get_mut(&sym) {
                    *cnt -= 1;
                }
                *e = v.clone();
            }
        },
        Operand::T(ExprTree { args, op: Operation::Pcopy }) => (),
        Operand::T(ExprTree { args, op: Operation::New }) => {
            args[1..].iter_mut().for_each(|e| inline(e, census, values))
        }
        Operand::T(ExprTree { args, op: Operation::InvokeVirtual | Operation::InvokeStatic }) => {
            inline(&mut args[0], census, values);
            args[2..].iter_mut().for_each(|e| inline(e, census, values))
        },
        Operand::T(ExprTree { args, .. }) => 
            args.iter_mut().for_each(|e| inline(e, census, values)),
        Operand::A(a) => match a {
            ArrayExpression::Empty(v) => v.iter_mut().for_each(|e| inline(e, census, values)),
            ArrayExpression::Initializer(a) => inline_array(a, census, values)
        }
    }
}

fn inline_array(
    a: &mut ArrayInitializer,
    census: &mut HashMap<Symbol, usize>,
    values: &HashMap<Symbol, Operand>) {
    for item in a {
        match item.as_mut() {
            ElementInitializer::Expr(e) => inline(e, census, values),
            ElementInitializer::ArrayInitializer(child) => inline_array(child, census, values)
        }
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
                int i = 0, j = 0, k = 0;
                while (i++ < 10) {
                    while (j++ < 10) {
                        while (k++ < 10) {
            
                        }
                        if ((j ^ k) < (i ^ k)) break;
                    }
                    if ((j ^ i) < (k ^ j)) break;
                }
                System.out.printf(
                    "%d %d %d",
                    i, j, k
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
        ast = Box::new(super::flatten(*ast, &mut sm));
        print(&ast, &sm);
        ast = Box::new(cssa::revert(*ast, &mut sm));
        print(&ast, &sm);
    }
}

