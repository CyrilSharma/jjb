use std::collections::HashMap;
use jjb::converter::convert;
use jjb::ir::{BlockStatement, ClassDeclaration, ContDeclaration, FunDeclaration, IfStatement, ImportDeclaration, LoopStatement, PrimStatement, ReturnStatement, SwitchStatement, Tree};
use jjb::symbolmaker::{Symbol, SymbolMaker};
use jjb::printer::print;
use tree_sitter::Parser;

struct CheckState {
    var_syms: HashMap<Symbol, u32>,
    fun_syms: HashMap<Symbol, u32>,
    class_syms: HashMap<Symbol, u32>,
    label_syms: HashMap<Symbol, u32>
}
impl CheckState {
    fn new() -> Self {
        Self {
            var_syms: HashMap::new(),
            fun_syms: HashMap::new(),
            class_syms: HashMap::new(),
            label_syms: HashMap::new()
        }
    }
}

fn inc<T: std::cmp::Eq + std::hash::Hash>(mp: &mut HashMap<T, u32>, key: T) {
    *mp.entry(key).or_insert(0) += 1;
}

fn census(root: &Tree) -> CheckState {
    let mut state = CheckState::new();
    traverse(root, &mut state);
    state
}

#[allow(unused)]
fn traverse(root: &Tree, state: &mut CheckState) {
    match root {
        Tree::LetI(ImportDeclaration { path, body }) => traverse(body, state),
        Tree::LetF(FunDeclaration { name, args, modifiers, throws, return_typ, body }) => {
            inc(&mut state.fun_syms, *name);
            for (sym, _) in args { inc(&mut state.var_syms, *sym) }
            body.as_ref().map(|b| traverse(b, state));
        },
        Tree::LetC(ClassDeclaration { name, members, methods, extends, body }) => {
            inc(&mut state.class_syms, *name);
            for (sym, _) in members { inc(&mut state.var_syms, *sym) }
            for method in methods { traverse(&method, state) }
            traverse(body, state);
        }
        Tree::LetE(_) => todo!(),
        Tree::LetCont(ContDeclaration { name, body }) => traverse(body, state),
        Tree::LetP(PrimStatement { name, typ, label, exp, body }) =>  {
            inc(&mut state.var_syms, *name);
            label.map(|l| inc(&mut state.label_syms, l));
            traverse(body, state)
        },
        Tree::Block(BlockStatement { label, bbody, body }) => {
            bbody.as_ref().map(|b| traverse(b, state));
            inc(&mut state.label_syms, *label);
            traverse(body, state)
        },
        Tree::Switch(SwitchStatement { arg, label, cases, default, body }) => {
            for (ops, code) in cases { traverse(code, state) }
            default.as_ref().map(|d| traverse(d, state));
            traverse(body, state)
        },
        Tree::Loop(LoopStatement { cond, label, lbody, body }) => {
            inc(&mut state.label_syms, *label);
            lbody.as_ref().map(|l| traverse(l, state));
            traverse(body, state);
        },
        Tree::If(IfStatement { cond, label, btrue, bfalse, body }) => {
            inc(&mut state.label_syms, *label);
            traverse(btrue, state);
            bfalse.as_ref().map(|b| traverse(b, state));
            traverse(body, state);
        }
        Tree::Try(_) => todo!(),
        Tree::Return(ReturnStatement { val }) => (),
        Tree::Continue(label) => inc(&mut state.label_syms, *label),
        Tree::Break(label) => inc(&mut state.label_syms, *label),
        Tree::EntryPoint(_) => (),
        Tree::Terminal => ()
    }
}


fn test(source: &str, checker: impl Fn(CheckState)) {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
    let tree = parser.parse(&source, None).unwrap();
    let mut sm = SymbolMaker::new();
    let ast = convert(tree.root_node(), source.as_bytes(), &mut sm);
    let state: CheckState = census(&ast);
    print(&ast, &sm);
    checker(state);
}


macro_rules! test_method {
    ($func:ident, $str:tt, $checker:expr) => {
        #[test]
        fn $func() {
            test(&format!(r#"
                public class Test {{
                    public static void main(String[] args) {{
                        {}
                    }}
                }}
            "#, $str), $checker);
        }
    };
}

test_method!(label_0, r#"
    int i = 0;
    label0: {
        label1: {
            label2: {
                i++;
            }
        }
    }

    label0: {
        label1: {
            label2: {
                i++;
            }
        }
    }
"#, |c| { assert_eq!(c.label_syms.keys().len(), 6) } );