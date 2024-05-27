use std::collections::{HashMap, HashSet};
use jjb::converter::convert;
use jjb::ir::*;
use jjb::parameters::Parameters;
use jjb::symbolmanager::{Symbol, SymbolManager};
use jjb::printer::print;
use tree_sitter::Parser;

struct CheckState {
    var_syms: HashMap<Symbol, u32>,
    fun_syms: HashMap<Symbol, u32>,
    class_syms: HashMap<Symbol, u32>,
    label_syms: HashMap<Symbol, u32>,
    fun_apps: HashMap<Symbol, u32>
}
impl CheckState {
    fn new() -> Self {
        Self {
            var_syms: HashMap::new(),
            fun_syms: HashMap::new(),
            class_syms: HashMap::new(),
            label_syms: HashMap::new(),
            fun_apps: HashMap::new()
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
        Tree::Program(stmts) => stmts.iter().for_each(|s| traverse(s, state)),
        Tree::LetI(ImportDeclaration { path }) => (),
        Tree::LetF(f) => {
            inc(&mut state.fun_syms, f.name);
            for (sym, _) in &f.args { inc(&mut state.var_syms, *sym) }
            f.body.iter().for_each(|b| traverse(b, state));
        },
        Tree::LetC(ClassDeclaration { name, members, methods, extends }) => {
            inc(&mut state.class_syms, *name);
            for method in methods { traverse(&method, state) }
        }
        Tree::LetE(_) => todo!(),
        Tree::LetP(PrimStatement { name, typ, exp }) =>  {
            if *typ != Typ::Void { inc(&mut state.var_syms, *name) }
            exp.as_ref().map(|e| operand(e, state));
        },
        Tree::Block(BlockStatement { label, bbody }) => {
            bbody.iter().for_each(|b| traverse(b, state));
            inc(&mut state.label_syms, *label);
        },
        Tree::Switch(SwitchStatement { arg, label, cases, default }) => {
            for (ops, code) in cases { code.iter().for_each(|t| traverse(t, state)) }
            for d in default { traverse(d, state) }
        },
        Tree::Loop(LoopStatement { cond, label, lbody, dowhile }) => {
            inc(&mut state.label_syms, *label);
            for l in lbody { traverse(l, state) };
        },
        Tree::If(IfStatement { cond, label, btrue, bfalse }) => {
            inc(&mut state.label_syms, *label);
            for b in btrue { traverse(b, state) }
            for b in bfalse { traverse(b, state) }
        }
        Tree::Try(_) => todo!(),
        Tree::Return(ReturnStatement { val }) => (),
        Tree::Continue(label) => inc(&mut state.label_syms, *label),
        Tree::Break(label) => inc(&mut state.label_syms, *label),
        Tree::EntryPoint(_) => ()
    }
}

fn operand(op: &Operand, state: &mut CheckState) {
    match op {
        Operand::This(_) => (),
        Operand::Super(_) => (),
        Operand::C(_) => (),
        Operand::V(sym) => inc(&mut state.var_syms, *sym),
        Operand::T(ExprTree { op, args }) => match op {
            jjb::ir::Operation::InstanceOf => todo!(),
            jjb::ir::Operation::New => {
                match args[0] {
                    Operand::V(sym) => inc(&mut state.fun_apps, sym),
                    _ => assert!(false, "Invalid Call operation!")
                };
            }
            jjb::ir::Operation::InvokeStatic | jjb::ir::Operation::InvokeVirtual => {
                match args[1] {
                    Operand::V(sym) => inc(&mut state.fun_apps, sym),
                    _ => assert!(false, "Invalid Call operation!")
                };
                args[2..].iter().for_each(|arg| operand(arg, state));
            },
            _ => { args.iter().for_each(|arg| operand(arg, state)); }
        },
        other => todo!()
    }
}


fn test(source: &str, checker: impl Fn(CheckState, &SymbolManager)) {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
    let tree = parser.parse(&source, None).unwrap();
    let mut sm = SymbolManager::new();
    let params = Parameters { entry_name: "main".to_string(), entry_class: "Test".to_string() };
    let ast = convert(tree.root_node(), source.as_bytes(), &params, &mut sm);
    let state: CheckState = census(&ast);
    print(&ast, &sm);
    checker(state, &sm);
}


macro_rules! test_method {
    ($func:ident, $str:tt, $checker:expr) => {
        #[test]
        fn $func() {
            test(&format!(r#"
                public class Test {{
                    public static void main() {{
                        {}
                    }}
                }}
            "#, $str), $checker);
        }
    };
}

macro_rules! test_classes {
    ($func:ident, $str:tt, $checker:expr) => {
        #[test]
        fn $func() {
            test(&format!(r#"
                public class Source {{
                    public static void main(String[] args) {{
                        Test.main();
                    }}
                }}
                {}
            "#, $str), $checker);
        }
    };
}

test_method!(label_1, r#"
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
"#, |c, _| {
    assert_eq!(c.label_syms.keys().len(), 6)
});

test_method!(var_1, r#"
    int x = 0;
    for (x = 1; x < 10; x++) {
        while (x++ < 10) {
            for (int x = 3; x < 4; x++) {
                break;
            }
        }
    }
    x++;
"#, |c, sm| {
    let mut xcount: u32 = 0;
    for key in c.var_syms.keys() {
        match sm.name(*key) {
            "x" => xcount += 1,
            _ => ()
        }
    }
    assert_eq!(xcount, 2)
});

test_classes!(classes_1, r#"
    class Point {
        int x;
        int y;
        Point(int _x, int _y) {
            x = _x;
            y = _y;
        }

        public int getx() {
            return x;
        }

        public int gety() {
            return y;
        }
    }
    class Test {
        public static void main() {
            int x = 0, y = 2;
            Point p = new Point(x, y);
            p.x = 4;
            p.y = 5;
            System.out.printf("%d %d\n", p.getx(), p.gety());
        }
    }
"#, |c, sm| {
    let mut used = HashSet::new();
    for key in c.fun_apps.keys() {
        let name = sm.name(*key);
        match name.as_ref() {
            "Point" | "getx" | "gety" => { used.insert(key); },
            _ => ()
        };
    }

    let mut def = HashSet::new();
    for key in c.fun_syms.keys() {
        let name = sm.name(*key);
        match name.as_ref() {
            "Point" | "getx" | "gety" => { def.insert(key); },
            _ => ()
        };
    }
    assert_eq!(def, used);
    assert_eq!(def.len(), 3)
});