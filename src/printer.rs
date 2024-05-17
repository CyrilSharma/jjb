use std::borrow::Cow;
use crate::ir::*;
use crate::symbolmaker::{Symbol, SymbolMaker};
use std::fs::File;
use std::io::{self, BufWriter, Write};

#[allow(dead_code)]
pub fn print(tree: &Tree, sm: &SymbolMaker) {
    let stdout = io::stdout();
    let handle = stdout.lock();
    let mut state = PrintState { level: 0, sm, buf: handle };
    print_tree(tree, &mut state);
}

#[allow(dead_code)]
pub fn str_print(tree: &Tree, sm: &SymbolMaker, buf: &mut Vec<u8>) {
    let mut state = PrintState { level: 0, sm, buf };
    print_tree(tree, &mut state);
}

struct PrintState<'l, W: Write> {
    level: u32,
    sm: &'l SymbolMaker,
    buf: W
}

impl<'l, W: Write> PrintState<'l, W> {
    fn uname(&self, sym: Symbol) -> String { self.sm.uname(sym) }
    fn println(&mut self, text: &str) {
        writeln!(self.buf, "{}{}", "  ".repeat(self.level as usize), text)
            .expect("Write failed!")
    }
    fn indent<F>(&mut self, f: F)
        where F: FnOnce(&mut PrintState<W>) {
        self.level += 1;
        f(self);
        self.level -= 1;
    }
}

fn print_tree(tree: &Tree, state: &mut PrintState<'_, impl Write>) {
    match tree {
        Tree::LetI(ImportDeclaration { path, body }) => {
            state.println(&format!("import {};", path));
            print_tree(body, state);
        },
        Tree::LetP(PrimStatement { name, typ, exp, body, label }) => {
            if *typ != Typ::Void {
                if let Some(e) = exp {
                    let tp = serialize_tp(typ, state);
                    let op = serialize_op(e, state);
                    state.println(&format!("{} {} = {};",
                        tp, state.uname(*name), op
                    ));
                } else {
                    let tp = serialize_tp(typ, state);
                    state.println(&format!("{} {};",
                        tp, state.uname(*name),
                    ));
                }
            } else {
                if let Some(e) = exp {
                    let op = serialize_op(e, state);
                    state.println(&format!("{};", op));
                }
            }
            print_tree(body, state);
        },
        Tree::LetF(FunDeclaration { name, args, modifiers, throws, return_typ, body }) => {
            let mut header = modifiers.join(" ");
            if modifiers.len() != 0 { header.push_str(" ") }
            return_typ.as_ref().map(|tp| header.push_str(&format!("{} ",
                serialize_tp(tp, state)
            )));
            header.push_str(&format!("{}({})",
                state.uname(*name),
                args.iter().map(|(sym, typ)|
                    format!("{} {}", serialize_tp(typ, state), state.uname(*sym)))
                    .collect::<Vec<_>>()
                    .join(", ")
                    
            ));
            if throws.len() != 0 {
                header.push_str(" throws ");
                header.push_str(&throws.join(", "));
            }
            state.println(&format!("{} {{", header));
            body.as_ref().map(|b| state.indent(|s| print_tree(b, s)));
            state.println("}");
        },
        Tree::LetC(ClassDeclaration { name, members, methods, extends, body }) => {
            let mut header = format!("class {}", state.uname(*name));
            if let Some(e) = extends {
                header.push_str(&format!(" extends {}", &state.uname(*e)));
            }
            state.println(&format!("{} {{", header));
            for (sym, tp) in members {
                let tp = serialize_tp(tp, state);
                state.indent(|state| state.println(&format!("{} {};",
                    tp, state.uname(*sym)),
                ));
            }
            methods.iter().for_each(|method| state.indent(|state| print_tree(method, state)));
            state.println("}");
            print_tree(body, state);
        },
        Tree::LetE(_) => todo!(),
        Tree::Switch(SwitchStatement { arg, cases, default, body, label }) =>  {
            let op = serialize_op(arg, state);
            state.println(&format!("{}: switch ({}) {{",
                state.uname(*label), op
            ));
            state.indent(|state| {
                for (ops, tree) in cases {
                    for op in ops {
                        let sop = serialize_op(op, state);
                        state.println(&format!("case {}: ", sop));
                    }
                    state.indent(|state| print_tree(tree,state));
                }
                state.println("default: ");
            });
            default.as_ref().map(|d| state.indent(|state| print_tree(d, state)));
            state.println("}");
            print_tree(body, state);
        },
        Tree::Loop(LoopStatement { cond, lbody, body, label }) => {
            let sop = serialize_op(cond, state);
            state.println(&format!("{}: while ({}) {{",
                state.uname(*label), sop
            ));
            lbody.as_ref().map(|t| state.indent(|state| print_tree(t, state)));
            state.println("}");
            print_tree(body, state);
        }
        Tree::If(IfStatement { cond, btrue, bfalse, body, label }) => {
            let op = serialize_op(cond, state);
            state.println(&format!("{}: if ({}) {{",
                state.uname(*label), op
            ));
            state.indent(|state| print_tree(btrue, state));
            if bfalse.is_some() {
                state.println("} else {");
                bfalse.as_ref().map(|x| state.indent(|state| print_tree(x, state)));
            }
            state.println("}");
            print_tree(body, state);
        },
        Tree::Try(_) => todo!(),
        Tree::Return(ReturnStatement { val }) => {
            if let Some(v) = val.as_ref() {
                let op = serialize_op(v, state);
                state.println(&format!("return {};", op));
            } else {
                state.println("return;");
            }
        },
        Tree::EntryPoint(sym) => state.println(
            &format!("// {}();", state.uname(*sym))
        ),
        Tree::Block(BlockStatement { label, bbody, body }) => {
            let mut buf = format!("{}: ", state.uname(*label));
            buf += "{";
            state.println(&buf);
            bbody.as_ref().map(|b| state.indent(|state| print_tree(b.as_ref(), state)));
            state.println("}");
            print_tree(body, state);
        },
        Tree::LetCont(ContDeclaration { name, body }) => {
            state.println(&format!("/* -- continuation {} -- */", state.uname(*name)));
            print_tree(body, state);
        },
        Tree::Continue(sym) => state.println(&format!("continue {};", state.uname(*sym))),
        Tree::Break(sym) => state.println(&format!("break {};", state.uname(*sym))),
        Tree::Terminal => ()
        // other => todo!()
    }
}

fn serialize_array_initializer(init: &ArrayInitializer, state: &mut PrintState<'_, impl Write>) -> String {
    let ArrayInitializer { tp, ops, dims } = init;
    let exp = ops.iter().map(|item| match item.as_ref() {
        ElementInitializer::Expr(exp) => serialize_op(exp, state),
        ElementInitializer::ArrayInitializer(a) => serialize_array_initializer(a, state)
    }).collect::<Vec<_>>()
        .join(", ");
    format!("{} {{ {} }}", serialize_tp(&tp, state), exp)
}

fn serialize_op(op: &Operand, state: &mut PrintState<'_, impl Write>) -> String {
    match op {
        Operand::Super => "super".to_string(),
        Operand::This => "this".to_string(),
        Operand::C(lit) => serialize_lit(lit),
        Operand::V(sym) => state.uname(*sym),
        Operand::A(array) => match array {
            ArrayExpression::Empty(aempty_box) => {
                let ArrayEmpty { tp, ops, dims } = aempty_box.as_ref();
                let exp = ops.iter().map(|item| format!("[{}]", serialize_op(item, state)))
                    .collect::<Vec<_>>()
                    .join("");
                format!(
                    "{}{}{}", serialize_tp(&tp, state), exp,
                    "[]".repeat(dims - ops.len())
                )
            }
            ArrayExpression::Initializer(initial_box) =>
                serialize_array_initializer(initial_box.as_ref(), state)
        },
        Operand::T(ExprTree { op, args }) => {
            use Operation::*;
            // Probably factor this out eventually.
            match op {
                Add | Sub | Mul | Div | Mod |
                Set | PSet | SSet | MSet | DSet | ModSet |
                AndSet | OrSet | XorSet | ShrSet |
                UshrSet | ShlSet | Eq | Neq | G | L |
                GEq | LEq | LAnd | LOr | LNot | Shl |
                Shr | UShr | And | Or | Xor | InstanceOf |
                Phi if args.len() == 2 => format!("({}) {} ({})",
                    serialize_op(&args[0], state),
                    serialize_operator(*op),
                    serialize_op(&args[1], state)
                ),
                PreInc | PreDec | Not | LNot | Sub if args.len() == 1 => format!("{}({})",
                    serialize_operator(*op),
                    serialize_op(&args[0], state),
                ),
                PostInc | PostDec if args.len() == 1 => format!("({}){}",
                    serialize_op(&args[0], state),
                    serialize_operator(*op),
                ),
                Ternary if args.len() == 3 => format!("({}) ? ({}) : ({})",
                    serialize_op(&args[0], state),
                    serialize_op(&args[1], state),
                    serialize_op(&args[2], state),
                ),
                Assert if args.len() == 2 => format!("assert ({}):({})",
                    serialize_op(&args[0], state),
                    serialize_op(&args[1], state),
                ),
                Assert if args.len() == 1 => format!("assert ({}):({})",
                    serialize_op(&args[0], state),
                    serialize_op(&args[1], state),
                ),
                New if args.len() > 1 => format!("new {}({})", 
                    serialize_op(&args[0], state),
                    args[1..].iter()
                        .map(|arg| serialize_op(arg, state))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                ArrayNew if args.len() == 1 => format!("new {}", 
                    serialize_op(&args[0], state)
                ),
                Call if args.len() >= 2 => format!("{}.{}({})",
                    serialize_op(&args[0], state),
                    serialize_op(&args[1], state),
                    args[2..].iter()
                        .map(|arg| serialize_op(arg, state))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Access if args.len() == 2 => format!("{}.{}",
                    serialize_op(&args[0], state),
                    serialize_op(&args[1], state)
                ),
                Index => todo!(),
                operator => panic!(
                    "Unhandled Operator {} with nargs: {}",
                    serialize_operator(*operator), args.len()
                )
            }
        }
    }
}

fn serialize_operator(operator: Operation) -> &'static str {
    use Operation::*;
    match operator {
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Mod => "%",
        PreInc => "++",
        PreDec => "--",
        PostInc => "++",
        PostDec => "--",
        Set => "=",
        PSet => "+=",
        SSet => "-=", 
        MSet => "*=",
        DSet => "/=",
        ModSet => "%=",
        AndSet => "&=",
        OrSet => "|=",
        XorSet => "^=",
        ShrSet => ">>=",
        UshrSet => ">>>=",
        ShlSet => "<<=",
        Eq => "==",
        Neq => "!=",
        G => ">",
        L => "<",
        GEq => ">=",
        LEq => "<=",
        LAnd => "&&",
        LOr => "||",
        LNot => "!",
        Not => "~",
        Shl => "<<",
        Shr => ">>",
        UShr => ">>>",
        And => "&",
        Or => "|",
        Xor => "^",
        InstanceOf => "instanceof",
        Assert => "assert",
        Index => "index",
        Throw => "throw",

        Phi => "phi",

        Ternary => "ternary",
        Access => "access",
        New => "new",
        ArrayNew => "new",
        Call => "call"
    }

}

fn serialize_lit(lit: &Literal) -> String {
    match lit {
        Literal::Null => "null".to_string(),
        Literal::Bool(b) => b.to_string(), 
        Literal::Char(c) => c.to_string(),
        Literal::Byte(b) => b.to_string(),
        Literal::Int(i) => i.to_string(),
        Literal::Short(s) => s.to_string(),
        Literal::Long(l) => l.to_string(),
        Literal::Float(f) => f.to_string(),
        Literal::Double(d) => d.to_string(),
        Literal::String(s) => s.clone()
    }
}

fn serialize_tp(tp: &Typ, state: &mut PrintState<'_, impl Write>) -> Cow<'static, str> {
    use Typ as T;
    use Cow::Borrowed as B;
    use Cow::Owned as O;
    match tp {
        T::Void => B("void"),
        T::Bool => B("boolean"),
        T::Char => B("char"),
        T::Byte => B("byte"),
        T::Int => B("int"),
        T::Short => B("short"),
        T::Long => B("long"),
        T::Float => B("float"),
        T::Double => B("double"),
        T::Str => B("String"),
        T::Array(ArrayTyp { eltype, dims }) =>  O(format!("{}{}",
            serialize_tp(eltype, state),
            "[]".repeat(*dims as usize)
        )),
        T::Class(s) => O(state.uname(*s))
    }
}