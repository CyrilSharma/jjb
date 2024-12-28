use std::collections::HashMap;

use crate::ir::*;
use crate::symbolmanager::{Symbol, SymbolManager};

struct State<'l> {
    sm: &'l mut SymbolManager,
    typemap: HashMap<Symbol, Typ>,
}

impl<'l> State<'l> {
    pub fn new(sm: &'l mut SymbolManager) -> Self {
        Self {
            sm,
            typemap: HashMap::new(),
        }
    }
}

pub fn typeinfer(tree: &mut Tree, sm: &mut SymbolManager) {
    let mut state = State::new(sm);
    initialize_directory(tree, &mut state);
    statement(tree, &mut state);
}

fn initialize_directory(tree: &Tree, state: &mut State) {
    match tree {
        Tree::Program(stmts) => {
            for stmt in stmts {
                initialize_directory(stmt, state)
            }
        }
        Tree::LetC(cdecl) => {
            for method in &cdecl.methods {
                initialize_directory(method, state)
            }
            for (sym, tp) in &cdecl.members {
                state.typemap.insert(*sym, tp.clone());
            }
        }
        Tree::LetF(f) => {
            state.typemap.insert(f.name, f.return_typ.clone());
        }
        _ => (),
    }
}

fn statement(tree: &mut Tree, state: &mut State) {
    use Tree as T;
    match tree {
        T::Program(stmts) => stmts.iter_mut().for_each(|s| statement(s, state)),
        T::LetF(f) => {
            for (sym, tp) in &f.args {
                state.typemap.insert(*sym, tp.clone());
            }
            for s in &mut f.body {
                statement(s, state)
            }
        }
        T::LetC(c) => {
            for (sym, tp) in &c.members {
                state.typemap.insert(*sym, tp.clone());
            }
            for method in &mut c.methods {
                statement(method, state)
            }
        }
        T::LetE(_) => todo!(),
        T::LetP(PrimStatement { name, typ, exp }) => {
            if *typ == Typ::Unknown {
                if let Some(e) = exp.as_mut() {
                    *typ = operand(e, state);
                }
                if let Some(n) = name {
                    state.typemap.insert(*n, *typ);
                }
            } else if *typ != Typ::Void {
                if let Some(n) = name {
                    state.typemap.insert(*n, *typ);
                }
            }
        }
        T::Block(BlockStatement { label, bbody }) => {
            bbody.iter_mut().for_each(|s| statement(s, state))
        }
        T::Switch(SwitchStatement {
            arg,
            label,
            cases,
            default,
        }) => {
            operand(arg, state);
            for (ops, code) in cases {
                code.iter_mut().for_each(|s| statement(s, state))
            }
            default.iter_mut().for_each(|s| statement(s, state))
        }
        T::Loop(LoopStatement {
            cond,
            label,
            lbody,
            dowhile,
        }) => {
            operand(cond, state);
            for s in lbody {
                statement(s, state)
            }
        }
        T::If(IfStatement {
            cond,
            label,
            btrue,
            bfalse,
        }) => {
            operand(cond, state);
            for s in btrue {
                statement(s, state)
            }
            for s in bfalse {
                statement(s, state)
            }
        }
        T::Try(_) => todo!(),
        T::LetI(_) | T::Return(_) | T::Break(_) | T::Continue(_) | T::EntryPoint(_) => (),
    }
}

fn type_conform(a: Typ, b: Typ) -> Typ {
    use Typ as T;
    match (&a, &b) {
        (_, _) if a == b => a.clone(),
        (_, _) if a.intrank() > 0 && b.intrank() > 0 => {
            if a.intrank() > b.intrank() {
                return a;
            }
            return b;
        }
        (T::Double, T::Float) => T::Double,
        (T::Float, T::Double) => T::Double,
        (T::Unknown, _) => Typ::Unknown,
        (_, T::Unknown) => Typ::Unknown,
        _ => Typ::Unknown,
    }
}

// Perhaps we will need Hindley–Milner?
// I'm really hoping we've preserved enough information to avoid that.
fn operand(op: &mut Operand, state: &mut State) -> Typ {
    use Literal as L;
    use Operand as O;
    use Operation::*;
    match op {
        O::Tp(_) => panic!("Type operands are handled specially"),
        O::This(sym) => Typ::Class(*sym),
        O::Super(sym) => Typ::Class(*sym),
        O::C(lit) => match lit {
            L::Null => Typ::Unknown,
            L::Bool(_) => Typ::Bool,
            L::Char(_) => Typ::Char,
            L::Byte(_) => Typ::Byte,
            L::Int(_) => Typ::Int,
            L::Short(_) => Typ::Short,
            L::Long(_) => Typ::Long,
            L::Float(_) => Typ::Float,
            L::Double(_) => Typ::Double,
            L::String(_) => Typ::Str,
        },
        O::V(sym) => *state.typemap.get(&sym).unwrap_or(&Typ::Unknown),
        O::A(_) => panic!("Array Operand should be handled specially."),
        O::T(ExprTree { op, args }) => match op {
            Add | Sub | Mul | Div | Mod | Shl | Shr | UShr | And | Or | Xor if args.len() == 2 => {
                let tp1 = operand(&mut args[0], state);
                let tp2 = operand(&mut args[1], state);
                type_conform(tp1, tp2)
            }
            Eq | Neq | G | Operation::L | GEq | LEq | LAnd | LOr | LNot => Typ::Bool,
            Set | PSet | SSet | MSet | DSet | ModSet | AndSet | OrSet | XorSet | ShrSet
            | UshrSet | ShlSet => operand(&mut args[1], state),
            InstanceOf => todo!(),
            Phi => {
                for arg in args[1..].iter_mut() {
                    let typ = operand(arg, state);
                    if typ != Typ::Unknown {
                        return typ;
                    }
                }
                return Typ::Unknown;
            }
            PreInc | PreDec | Not | Sub | PostInc | PostDec if args.len() == 1 => {
                operand(&mut args[0], state)
            }
            Ternary if args.len() == 3 => {
                let tp1 = operand(&mut args[1], state);
                let tp2 = operand(&mut args[2], state);
                type_conform(tp1, tp2)
            }
            Assert => Typ::Unknown,
            New if args.len() > 1 => match &args[0] {
                Operand::V(sym) => Typ::Class(*sym),
                _ => panic!("Invalid new!"),
            },
            ArrayNew if args.len() == 2 => match &args[0] {
                Operand::Tp(tp) => tp.clone(),
                _ => panic!("Invalid ArrayNew"),
            },
            InvokeVirtual | InvokeStatic => match &args[1] {
                O::V(sym) => state
                    .typemap
                    .get(&sym)
                    .expect("Symbol was not found in map")
                    .clone(),
                _ => Typ::Unknown,
            },
            Access if args.len() == 2 => {
                // It is expected that symbols have already been resolved.
                // Hence, field accesses should already be pointing to their corresponding fields.
                match &args[1] {
                    O::This(sym) => Typ::Class(*sym),
                    O::Super(sym) => Typ::Class(*sym),
                    O::C(_) => panic!("Invalid Constant Access"),
                    O::V(sym) => state
                        .typemap
                        .get(&sym)
                        .expect("Symbol was not found in map")
                        .clone(),
                    O::A(_) => panic!("Invalid Array Access"),
                    O::Tp(_) => panic!("Invalid Type Access"),
                    O::T(_) => Typ::Unknown,
                }
            }
            Index if args.len() == 2 => match operand(&mut args[0], state) {
                Typ::Array(asym) => {
                    if let Some(ArrayTyp { eltype, dims }) = state.sm.arraytyp(asym) {
                        *eltype
                    } else {
                        panic!("")
                    }
                }
                _ => panic!("Invalid access"),
            },
            operator => panic!(
                "Unhandled Operator {:?} with nargs: {:?}",
                operator,
                args.len()
            ),
        },
    }
}
