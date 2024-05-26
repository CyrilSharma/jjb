use std::borrow::Cow;
use crate::ir::*;
use crate::parameters::Parameters;
use crate::symbolmanager::{Symbol, SymbolManager};
use std::io::{self, Write};

#[allow(dead_code)]
pub fn print(tree: &Tree, sm: &SymbolManager, params: &Parameters) {
    let stdout = io::stdout();
    let handle = stdout.lock();
    let mut state = PrintState { level: 0, sm, buf: handle, params };
    print_tree(tree, &mut state);
}

#[allow(dead_code)]
pub fn str_print(tree: &Tree, sm: &SymbolManager, buf: &mut Vec<u8>, params: &Parameters) {
    let mut state = PrintState { level: 0, sm, buf, params };
    print_tree(tree, &mut state);
}

struct PrintState<'l, W: Write> {
    level: u32,
    sm: &'l SymbolManager,
    buf: W,
    params: &'l Parameters
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
        Tree::Program(stmts) => stmts.iter().for_each(|s| print_tree(s, state)),
        Tree::LetI(ImportDeclaration { path }) => {
            state.println(&format!("import {};", path));
        },
        Tree::LetP(PrimStatement { name, typ, exp }) => {
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
        },
        Tree::LetF(FunDeclaration { name, args, modifiers, throws, return_typ, body, constructor }) => {
            let mut header = modifiers.join(" ");
            if modifiers.len() != 0 { header.push_str(" ") }
            if !constructor { header.push_str(&format!("{} ", serialize_tp(return_typ, state))); }
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
            body.iter().for_each(|b| state.indent(|s| print_tree(b, s)));
            state.println("}");
        },
        Tree::LetC(ClassDeclaration { name, members, methods, extends }) => {
            let mut header = "".to_string();
            if state.params.entry_class == state.sm.name(*name) { header.push_str("public ") }
            header.push_str(&format!("class {}", state.uname(*name)));
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
        },
        Tree::LetE(_) => todo!(),
        Tree::Switch(SwitchStatement { arg, cases, default, label }) =>  {
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
                    state.indent(|state| tree.iter().for_each(
                        |s| { print_tree(s, state); }
                    ));
                }
                state.println("default: ");
            });
            default.iter().for_each(|d| state.indent(|state| print_tree(d, state)));
            state.println("}");
        },
        Tree::Loop(LoopStatement { cond, lbody, label, dowhile }) => {
            let sop = serialize_op(cond, state);
            if *dowhile { state.println(&format!("{}: do {{", state.uname(*label))); } 
            else { state.println(&format!("{}: while ({}) {{", state.uname(*label), sop));}
            lbody.iter().for_each(|t| state.indent(|state| print_tree(t, state)));
            if *dowhile { state.println(&format!("}} while ({});", sop)); } 
            else { state.println("}"); }
        }
        Tree::If(IfStatement { cond, btrue, bfalse, label }) => {
            let op = serialize_op(cond, state);
            state.println(&format!("{}: if ({}) {{",
                state.uname(*label), op
            ));
            state.indent(|state| btrue.iter().for_each(|t| print_tree(t, state)));
            if bfalse.len() != 0 {
                state.println("} else {");
                bfalse.iter().for_each(|x| state.indent(|state| print_tree(x, state)));
            }
            state.println("}");
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
        Tree::Block(BlockStatement { label, bbody }) => {
            let mut buf = format!("{}: ", state.uname(*label));
            buf += "{";
            state.println(&buf);
            bbody.iter().for_each(|t| state.indent(|state| print_tree(t, state)));
            state.println("}");
        },
        Tree::Continue(sym) => state.println(&format!("continue {};", state.uname(*sym))),
        Tree::Break(sym) => state.println(&format!("break {};", state.uname(*sym)))
    }
}

fn serialize_array_initializer(ops: &ArrayInitializer, state: &mut PrintState<'_, impl Write>) -> String {
    let exp = ops.iter().map(|item| match item.as_ref() {
        ElementInitializer::Expr(exp) => serialize_op(exp, state),
        ElementInitializer::ArrayInitializer(a) => serialize_array_initializer(a, state)
    }).collect::<Vec<_>>()
        .join(", ");
    format!("{{ {} }}", exp)
}

fn serialize_op(op: &Operand, state: &mut PrintState<'_, impl Write>) -> String {
    match op {
        Operand::Super(_) => "super".to_string(),
        Operand::This(_) => "this".to_string(),
        Operand::C(lit) => format!("{}", lit),
        Operand::V(sym) => state.uname(*sym),
        Operand::A(_) => panic!("This case is handled seperately"),
        Operand::Tp(_) => panic!("This case is handled seperately"),
        Operand::T(ExprTree { op, args }) => {
            use Operation::*;
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
                ArrayNew if args.len() == 2 => serialize_array(&args[0], &args[1], state),
                InvokeVirtual | InvokeVirtual if args.len() >= 2 => format!("{}.{}({})",
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
                Index if args.len() == 2 => format!("{}[{}]",
                    serialize_op(&args[0], state),
                    serialize_op(&args[1], state)
                ),
                operator => panic!(
                    "Unhandled Operator {} with nargs: {}",
                    serialize_operator(*operator), args.len()
                )
            }
        }
    }
}

fn serialize_array(optyp: &Operand, op: &Operand, state: &mut PrintState<'_, impl Write>) -> String {
    let asym = if let Operand::Tp(Typ::Array(asym)) = optyp { asym } else { panic!("Unknown Array Type!") };
    let array = if let Operand::A(array) = op { array } else { panic!("Unknown Array Op") };
    let (eltype, dims) = if let Some(ArrayTyp { eltype, dims }) = state.sm.arraytyp(*asym)
        { (eltype, dims) } else { panic!("Invalid Array") };
    match array {
        ArrayExpression::Empty(aempty_box) => {
            let ops = aempty_box.as_ref();
            let exp = ops.iter().map(|item| format!("[{}]", serialize_op(item, state)))
                .collect::<Vec<_>>()
                .join("");
            format!(
                "new {}{}{}", serialize_tp(&eltype, state), exp,
                "[]".repeat((*dims as usize) - ops.len())
            )
        }
        ArrayExpression::Initializer(initial_box) => format!("new {}{}",
            serialize_tp(&Typ::Array(*asym), state),
            serialize_array_initializer(initial_box.as_ref(), state)
        )
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
        InvokeStatic => "invoke_static",
        InvokeVirtual => "invoke_virtual",
    }

}

fn serialize_tp(tp: &Typ, state: &mut PrintState<'_, impl Write>) -> Cow<'static, str> {
    use Typ as T;
    use Cow::Borrowed as B;
    use Cow::Owned as O;
    match tp {
        T::Unknown => B("unknown"),
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
        T::Array(asym) => {
            if let Some(ArrayTyp { eltype, dims }) = state.sm.arraytyp(*asym) {
                O(format!("{}{}", serialize_tp(eltype, state),
                    "[]".repeat(*dims as usize)))
            } else {
                panic!("Unknown ArrayType")
            }
        },
        T::Class(s) => O(state.uname(*s))
    }
}