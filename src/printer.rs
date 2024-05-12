use std::borrow::Cow;
use crate::ir::*;

pub struct Printer<'l> { level: usize, sm: &'l SymbolMaker }
impl<'l> Printer<'l> {
    pub fn new(sm: &'l SymbolMaker) -> Self { Self { level: 0, sm } }
    pub fn print_tree(&mut self, tree: &Tree) {
        macro_rules! scope {
            ($block:block) => {
                self.level += 1;
                $block;
                self.level -= 1;
            };
        }

        match tree {
            Tree::LetI(ImportDeclaration { path, body }) => {
                self.println(&format!("import {};", path));
                self.print_tree(body);
            },
            Tree::LetP(PrimStatement { name, typ, exp, body, label }) => {
                if *typ != Typ::Void {
                    if let Some(e) = exp {
                        self.println(&format!("{} {} = {};",
                            self.serialize_tp(typ),
                            self.sm.uname(*name),
                            self.serialize_op(e)
                        ));
                    } else {
                        self.println(&format!("{} {};",
                            self.serialize_tp(typ),
                            self.sm.uname(*name),
                        ));
                    }
                } else {
                    if let Some(e) = exp {
                        self.println(&format!("{};",
                            self.serialize_op(e)
                        ));
                    }
                }
                self.print_tree(body);
            },
            Tree::LetF(FunDeclaration { name, args, modifiers, throws, return_typ, body }) => {
                let mut header = modifiers.join(" ");
                if modifiers.len() != 0 { header.push_str(" ") }
                header.push_str(&format!("{} {}({})",
                    self.serialize_tp(return_typ),
                    self.sm.uname(*name),
                    args.iter().map(|(sym, typ)|
                        format!("{} {}", self.serialize_tp(typ), self.sm.uname(*sym)))
                        .collect::<Vec<_>>()
                        .join(", ")
                     
                ));
                if throws.len() != 0 {
                    header.push_str(" throws ");
                    header.push_str(&throws.join(", "));
                }
                self.println(&format!("{} {{", header));
                scope!({ body.as_ref().map(|b| self.print_tree(b)) });
                self.println("}");
            },
            Tree::LetC(ClassDeclaration { name, members, methods, extends, body }) => {
                let mut header = format!("class {}", self.sm.uname(*name));
                if let Some(e) = extends {
                    header.push_str(&format!(" extends {}", &self.sm.uname(*e)));
                }
                self.println(&format!("{} {{", header));
                scope!({
                    for (sym, tp) in members {
                        self.println(&format!("{} {};", self.serialize_tp(tp), self.sm.uname(*sym)));
                    }
                    for method in methods {
                        self.print_tree(method);
                    }
                });
                self.println("}");
                self.print_tree(body);
            },
            Tree::LetE(_) => todo!(),
            Tree::Switch(SwitchStatement { arg, cases, default, body, label }) =>  {
                self.println(
                    &format!("{}: switch ({}) {{",
                    self.sm.uname(*label),
                    self.serialize_op(arg))
                );
                scope!({
                    for (ops, tree) in cases {
                        for op in ops {
                            self.println(&format!("case {}: ", self.serialize_op(op)));
                        }
                        scope!({ self.print_tree(tree) });
                    }
                    self.println("default: ");
                    if let Some(d) = default { scope!({ self.print_tree(d) }); } 
                });
                self.println("}");
                self.print_tree(body);
            },
            Tree::Loop(LoopStatement { cond, lbody, body, label }) => {
                self.println(&format!(
                    "{}: while ({}) {{",
                    self.sm.uname(*label),
                    self.serialize_op(cond))
                );
                scope!({ lbody.as_ref().map(|t| self.print_tree(t)) });
                self.println("}");
                self.print_tree(body);
            }
            Tree::If(IfStatement { cond, btrue, bfalse, body, label }) => {
                self.println(
                    &format!("{}: if ({}) {{",
                    self.sm.uname(*label),
                    self.serialize_op(cond))
                );
                scope!({ self.print_tree(btrue) });
                self.println("} else {");
                scope!({ bfalse.as_ref().map(|x| self.print_tree(x)) });
                self.println("}");
                self.print_tree(body);
            },
            Tree::Try(_) => todo!(),
            Tree::Return(ReturnStatement { val }) => {
                if let Some(v) = val.as_ref() {
                    self.println(&format!("return {};", self.serialize_op(v)));
                } else {
                    self.println("return;");
                }
            },
            Tree::EntryPoint(sym) => self.println(
                &format!("// {}();", self.sm.uname(*sym))
            ),
            Tree::Block(BlockStatement { label, bbody, body }) => {
                let mut buf = "".to_owned();
                label.map(|l| buf += &format!("{}: ", self.sm.uname(l)));
                buf += "{";
                self.println(&buf);
                scope!({ self.print_tree(&bbody) });
                self.println("}");
                self.print_tree(body);
            },
            Tree::LetCont(ContDeclaration { name, body }) => {
                self.println(&format!("/* -- continuation {} -- */", self.sm.uname(*name)));
                self.print_tree(body);
            },
            Tree::Continue(sym) => self.println(&format!("continue {};", self.sm.uname(*sym))),
            Tree::Break(sym) => self.println(&format!("break {};", self.sm.uname(*sym))),
            Tree::Terminal => ()
            // other => todo!()
        }
    }

    fn serialize_op(&self, op: &Operand) -> String {
        match op {
            Operand::This => "this".to_string(),
            Operand::C(lit) => self.serialize_lit(lit),
            Operand::V(sym) => self.sm.uname(*sym),
            Operand::T(ExprTree { op, args }) => {
                use Operation::*;
                // Probably factor this out eventually.
                match op {
                    Add | Sub | Negate | Mul | Div | Mod |
                    Set | PSet | SSet | MSet | DSet | ModSet |
                    AndSet | OrSet | XorSet | ShrSet |
                    UshrSet | ShlSet | Eq | Neq | G | L |
                    GEq | LEq | LAnd | LOr | LNot | Shl |
                    Shr | UShr | And | Or | Xor | InstanceOf |
                    Phi if args.len() == 2 => format!("({}) {} ({})",
                        self.serialize_op(&args[0]),
                        self.serialize_operator(*op),
                        self.serialize_op(&args[1])
                    ),
                    PreInc | PreDec | Not | LNot if args.len() == 1 => format!("{}({})",
                        self.serialize_operator(*op),
                        self.serialize_op(&args[0]),
                    ),
                    PostInc | PostDec if args.len() == 1 => format!("{}({})",
                        self.serialize_op(&args[0]),
                        self.serialize_operator(*op),
                    ),
                    Ternary if args.len() == 3 => format!("({}) ? ({}) : ({})",
                        self.serialize_op(&args[0]),
                        self.serialize_op(&args[1]),
                        self.serialize_op(&args[2]),
                    ),
                    Continue if args.len() == 0 => "continue".to_string(),
                    Break if args.len() == 0 => "break".to_string(),
                    Assert if args.len() == 2 => format!("assert ({}):({})",
                        self.serialize_op(&args[0]),
                        self.serialize_op(&args[1]),
                    ),
                    Assert if args.len() == 1 => format!("assert ({}):({})",
                        self.serialize_op(&args[0]),
                        self.serialize_op(&args[1]),
                    ),
                    Access => todo!(),
                    Index => todo!(),
                    operator => panic!(
                        "Unhandled Operator {} with nargs: {}",
                        self.serialize_operator(*operator), args.len()
                    )
                }
            }
        }
    }

    fn serialize_operator(&self, operator: Operation) -> &str {
        use Operation::*;
        match operator {
            Add => "+",
            Sub => "-",
            Negate => "-",
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
            Eq => "=",
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
        }

    }

    fn serialize_lit(&self, lit: &Literal) -> String {
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

    fn serialize_tp(&self, tp: &Typ) -> Cow<'static, str> {
        use Typ as T;
        use Cow::Borrowed as B;
        use Cow::Owned as O;
        match tp {
            T::Void => B("void"),
            T::Bool => B("bool"),
            T::Char => B("char"),
            T::Byte => B("byte"),
            T::Int => B("int"),
            T::Short => B("short"),
            T::Long => B("long"),
            T::Float => B("float"),
            T::Double => B("double"),
            T::Str => B("String"),
            T::Class(s) => O(self.sm.uname(*s))
        }
    }

    fn println(&self, text: &str) {
        let indent = "  ".repeat(self.level);
        println!("{}{}", indent, text);
    }
}