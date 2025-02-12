use std::collections::HashMap;

use crate::container::ContainerHelpers;
use crate::ir::*;
use crate::symbolmanager::{Symbol, SymbolManager};

struct State<'l> {
    sm: &'l mut SymbolManager,
    cont_to_break: HashMap<Symbol, Symbol>,
}

impl<'l> State<'l> {
    fn new(sm: &'l mut SymbolManager) -> Self {
        State {
            sm,
            cont_to_break: HashMap::new(),
        }
    }
}

fn wrap_prim(mut list: TreeContainer, exp: Operand, state: &mut State) -> (TreeContainer, Operand) {
    let name = state.sm.fresh("prim");
    let tree = Tree::LetP(PrimStatement {
        name: Some(name),
        typ: Typ::Unknown,
        exp: Some(exp),
    });
    list.push_back(tree);
    (list, Operand::V(name))
}

pub fn hoist(root: &Tree, sm: &mut SymbolManager) -> Box<Tree> {
    let mut state = State::new(sm);
    Box::new(Tree::Program(statement(root, &mut state)))
}

fn statement(root: &Tree, state: &mut State) -> TreeContainer {
    match root {
        Tree::Program(stmts) => {
            let mut res = TreeContainer::new();
            for s in stmts {
                res.append(&mut statement(s, state))
            }
            res
        }
        Tree::LetI(_) => TreeContainer::make(root.clone()),
        Tree::LetF(fundecl) => {
            let mut body = TreeContainer::new();
            for s in &fundecl.body {
                body.append(&mut statement(&s, state))
            }
            TreeContainer::make(Tree::LetF(FunDeclaration {
                body,
                ..fundecl.clone()
            }))
        }
        Tree::LetC(classdecl) => {
            let mut methods = TreeContainer::new();
            for s in &classdecl.methods {
                methods.append(&mut statement(&s, state))
            }
            TreeContainer::make(Tree::LetC(ClassDeclaration {
                methods,
                ..classdecl.clone()
            }))
        }
        Tree::LetE(_) => todo!(),
        Tree::LetP(PrimStatement {
            exp: Some(Operand::T(ExprTree { op, args })),
            typ: Typ::Void,
            name: None,
        }) if matches!(op, Operation::InvokeVirtual | Operation::InvokeStatic) => {
            let (mut lhsv, l) = (TreeContainer::new(), args[0].clone());
            let mut nargs = vec![l, args[1].clone()];
            for arg in &args[2..] {
                let (mut rhsv, r) = operand(arg, state);
                lhsv.append(&mut rhsv);
                nargs.push(r);
            }
            lhsv.push_back(Tree::LetP(PrimStatement {
                exp: Some(Operand::T(ExprTree {
                    op: *op,
                    args: nargs,
                })),
                typ: Typ::Void,
                name: None,
            }));
            lhsv
        }
        Tree::LetP(primdecl) => {
            if primdecl.exp.is_none() {
                return TreeContainer::make(root.clone());
            }
            let e = primdecl.exp.as_ref().expect("");
            let (mut head, exp) = operand(&e, state);
            head.back_mut().map(|h| match h {
                Tree::LetP(PrimStatement { name, typ, exp }) => {
                    *typ = primdecl.typ.clone();
                }
                _ => panic!("Invalid LetP"),
            });
            if primdecl.typ == Typ::Void {
                return head;
            }
            head.push_back(Tree::LetP(PrimStatement {
                exp: Some(exp),
                ..primdecl.clone()
            }));
            return head;
        }
        Tree::Block(bstmt) => {
            let mut bbody = TreeContainer::new();
            for s in &bstmt.bbody {
                bbody.append(&mut statement(&s, state))
            }
            TreeContainer::make(Tree::Block(BlockStatement {
                bbody,
                ..bstmt.clone()
            }))
        }
        Tree::Switch(sstmt) => {
            let (mut head, arg) = operand(&sstmt.arg, state);
            let mut cases = Vec::new();
            for (ops, code) in &sstmt.cases {
                let mut newcode = TreeContainer::new();
                for s in code {
                    newcode.append(&mut statement(&s, state))
                }
                cases.push((ops.clone(), newcode));
            }
            let default = tail(sstmt.default.clone(), Tree::Break(sstmt.label));
            head.push_back(Tree::Switch(SwitchStatement {
                cases,
                arg,
                default,
                label: sstmt.label,
            }));
            head
        }
        Tree::Loop(lstmt) => {
            let mut res = TreeContainer::new();
            let (mut head, cond) = operand(&lstmt.cond, state);
            let iflabel = state.sm.fresh("while_cond");
            head.push_back(Tree::If(IfStatement {
                cond: cond.clone(),
                label: iflabel,
                btrue: TreeContainer::make(Tree::Break(iflabel)),
                bfalse: TreeContainer::make(Tree::Break(lstmt.label)),
            }));
            let mut lbody = TreeContainer::new();
            if lstmt.dowhile {
                let label = state.sm.fresh("do_block");
                state.cont_to_break.insert(lstmt.label, label);
                let mut bbody = TreeContainer::new();
                lstmt
                    .lbody
                    .iter()
                    .for_each(|s| bbody.append(&mut statement(&s, state)));
                bbody = tail(bbody, Tree::Break(label));
                lbody = TreeContainer::make(Tree::Block(BlockStatement { label, bbody }));
                lbody.append(&mut head);
            } else {
                lbody.append(&mut head);
                lstmt
                    .lbody
                    .iter()
                    .for_each(|s| lbody.append(&mut statement(&s, state)));
            }
            lbody = tail(lbody, Tree::Continue(lstmt.label));
            res.push_back(Tree::Loop(LoopStatement {
                cond: Operand::C(Literal::Bool(true)),
                lbody,
                label: lstmt.label,
                dowhile: lstmt.dowhile,
            }));
            res
        }
        Tree::If(IfStatement {
            cond,
            label,
            btrue,
            bfalse,
        }) => {
            let (mut head, cond) = operand(&cond, state);
            let (mut nbtrue, mut nbfalse) = (TreeContainer::new(), TreeContainer::new());
            btrue
                .iter()
                .for_each(|s| nbtrue.append(&mut statement(s, state)));
            bfalse
                .iter()
                .for_each(|s| nbfalse.append(&mut statement(s, state)));
            head.push_back(Tree::If(IfStatement {
                cond,
                label: *label,
                btrue: nbtrue,
                bfalse: nbfalse,
            }));
            head
        }
        Tree::Try(_) => todo!(),
        Tree::Return(rstmt) => {
            if let Some(e) = &rstmt.val {
                let (mut head, exp) = operand(&e, state);
                head.push_back(Tree::Return(ReturnStatement { val: Some(exp) }));
                return head;
            }
            TreeContainer::make(root.clone())
        }
        Tree::Continue(label) => TreeContainer::make(match state.cont_to_break.get(&label) {
            Some(sym) => Tree::Break(*sym),
            None => Tree::Continue(*label),
        }),
        Tree::Break(label) => TreeContainer::make(Tree::Break(*label)),
        Tree::EntryPoint(sym) => TreeContainer::make(Tree::EntryPoint(*sym)),
        _ => todo!(),
    }
}

fn array_initializer(
    ops: &ArrayInitializer,
    state: &mut State,
) -> (TreeContainer, ArrayInitializer) {
    use ElementInitializer as E;
    let (lhsv, args) = ops
        .iter()
        .map(|item| match item.as_ref() {
            E::Expr(exp) => {
                let (v, e) = operand(exp, state);
                (v, Box::new(E::Expr(e)))
            }
            E::ArrayInitializer(a) => {
                let (v, e) = array_initializer(a, state);
                (v, Box::new(E::ArrayInitializer(e)))
            }
        })
        .collect::<Vec<_>>()
        .into_iter()
        .fold((TreeContainer::new(), Vec::new()), |acc, vec| {
            let (mut v, op) = vec;
            let (mut av, mut aops) = acc;
            av.append(&mut v);
            aops.push(op);
            (av, aops)
        });
    (lhsv, args)
}

fn operand(root: &Operand, state: &mut State) -> (TreeContainer, Operand) {
    use Operation::*;
    match root {
        Operand::This(sym) => (TreeContainer::new(), Operand::This(*sym)),
        Operand::Super(sym) => (TreeContainer::new(), Operand::Super(*sym)),
        Operand::C(lit) => (TreeContainer::new(), Operand::C(lit.clone())),
        Operand::V(sym) => (TreeContainer::new(), Operand::V(*sym)),
        Operand::Tp(t) => (TreeContainer::new(), Operand::Tp(t.clone())),
        Operand::A(array) => match array {
            // We don't use wrap_prim here since that would remove the type argument.
            ArrayExpression::Empty(bempty) => {
                let ops = bempty.as_ref();
                let mut lhsv = TreeContainer::new();
                let mut nops = vec![ops[0].clone()];
                for arg in &ops[1..] {
                    let (mut rhsv, r) = operand(arg, state);
                    lhsv.append(&mut rhsv);
                    nops.push(r);
                }
                (lhsv, Operand::A(ArrayExpression::Empty(Box::new(nops))))
            }
            ArrayExpression::Initializer(a) => {
                let (v, e) = array_initializer(a, state);
                (v, Operand::A(ArrayExpression::Initializer(Box::new(e))))
            }
        },
        Operand::T(ExprTree { op, args }) => match op {
            Add | Sub | Mul | Div | Mod | Eq | Neq | G | L | GEq | LEq | LAnd | LOr | LNot
            | Shl | Shr | UShr | And | Or | Xor | InstanceOf | Phi | Assert | Index | Access
                if args.len() == 2 =>
            {
                let (lhsv, l) = operand(&args[0], state);
                let (mut rhsv, r) = operand(&args[1], state);
                let op = Operand::T(ExprTree {
                    op: *op,
                    args: vec![l, r],
                });
                let mut res = lhsv;
                res.append(&mut rhsv);
                wrap_prim(res, op, state)
            }
            Set | PSet | SSet | MSet | DSet | ModSet | AndSet | OrSet | XorSet | ShrSet
            | UshrSet | ShlSet
                if args.len() == 2 =>
            {
                let (lhsv, l) = assignee(&args[0], state);
                let (mut rhsv, r) = operand(&args[1], state);
                match l {
                    Operand::V(sym) => {
                        let exp = if *op == Set {
                            Some(r)
                        } else {
                            let args = vec![Operand::V(sym), r];
                            Some(Operand::T(ExprTree {
                                op: without_set(*op),
                                args,
                            }))
                        };
                        let mut res = lhsv;
                        res.append(&mut rhsv);
                        res.push_back(Tree::LetP(PrimStatement {
                            name: Some(sym),
                            typ: Typ::Void,
                            exp,
                        }));
                        (res, Operand::V(sym))
                    }
                    _ => {
                        let op = Operand::T(ExprTree {
                            op: *op,
                            args: vec![l.clone(), r],
                        });
                        let mut res = lhsv;
                        res.append(&mut rhsv);
                        res.push_back(Tree::LetP(PrimStatement {
                            name: None,
                            typ: Typ::Void,
                            exp: Some(op),
                        }));
                        (res, l)
                    }
                }
            }
            PreInc | PreDec | PostInc | PostDec if args.len() == 1 => {
                let (lhsv, l) = assignee(&args[0], state);
                match l {
                    // Requires some thunking.
                    Operand::V(sym) => {
                        let newop = match *op {
                            PreDec | PostDec => Sub,
                            PreInc | PostInc => Add,
                            _ => panic!(),
                        };
                        let temp = state.sm.fresh("temp");
                        let args = vec![Operand::V(sym), Operand::C(Literal::Byte(1))];
                        let exp = Some(Operand::T(ExprTree { op: newop, args }));
                        let mut prims = TreeContainer::new();
                        if matches!(op, PostDec | PostInc) {
                            prims.push_back(Tree::LetP(PrimStatement {
                                name: Some(temp),
                                typ: Typ::Unknown,
                                exp: Some(Operand::V(sym)),
                            }));
                        }
                        prims.push_back(Tree::LetP(PrimStatement {
                            name: Some(sym),
                            typ: Typ::Void,
                            exp,
                        }));
                        match op {
                            PreDec | PreInc => {
                                let mut res = lhsv;
                                res.append(&mut prims);
                                (res, Operand::V(sym))
                            }
                            PostDec | PostInc => {
                                let mut res = lhsv;
                                res.append(&mut prims);
                                (res, Operand::V(temp))
                            }
                            _ => panic!(),
                        }
                    }
                    _ => {
                        let op = Operand::T(ExprTree {
                            op: *op,
                            args: vec![l.clone()],
                        });
                        let mut res = lhsv;
                        res.push_back(Tree::LetP(PrimStatement {
                            name: None,
                            typ: Typ::Void,
                            exp: Some(op),
                        }));
                        (res, l)
                    }
                }
            }
            Not | LNot | Sub | Assert if args.len() == 1 => {
                let (v, e) = operand(&args[0], state);
                let op = Operand::T(ExprTree {
                    op: *op,
                    args: vec![e],
                });
                wrap_prim(v, op, state)
            }
            Ternary if args.len() == 3 => {
                let (lhsv, l) = operand(&args[0], state);
                let (mut rhsv, r) = operand(&args[1], state);
                let (mut mhsv, m) = operand(&args[2], state);
                let op = Operand::T(ExprTree {
                    op: *op,
                    args: vec![l, r, m],
                });
                let mut res = lhsv;
                res.append(&mut rhsv);
                res.append(&mut mhsv);
                wrap_prim(res, op, state)
                // Hold off on ternary hoisting until we have
                // Better type resolution....
                // let res = state.sm.fresh("ternary");
                // let mut list = lhsv + rhsv + mhsv;
                // list.push_back(Tree::LetP(PrimStatement {
                //     name: Some(res),
                //     typ: Typ::Unknown,
                //     exp: None
                // }));
                // list.push_back(Tree::If(IfStatement {
                //     cond: l,
                //     label: state.sm.fresh("ternary"),
                //     btrue: TreeContainer::make(Tree::LetP(PrimStatement {
                //         name: Some(res), typ: Typ::Void, exp: Some(r)
                //     })),
                //     bfalse: TreeContainer::make(Tree::LetP(PrimStatement {
                //         name: Some(res), typ: Typ::Void, exp: Some(m)
                //     })),
                // }));
                // (list, Operand::V(res))
            }
            ArrayNew if args.len() == 2 => {
                let (rhsv, r) = operand(&args[1], state);
                let op = Operand::T(ExprTree {
                    op: *op,
                    args: vec![args[0].clone(), r],
                });
                wrap_prim(rhsv, op, state)
            }
            // Access if args.len() == 2 => (TreeContainer::new(), root.clone()),
            New if args.len() > 1 => {
                let mut lhsv = TreeContainer::new();
                let mut nargs = vec![args[0].clone()];
                for arg in &args[1..] {
                    let (mut rhsv, r) = operand(arg, state);
                    lhsv.append(&mut rhsv);
                    nargs.push(r);
                }
                let op = Operand::T(ExprTree {
                    op: *op,
                    args: nargs,
                });
                wrap_prim(lhsv, op, state)
            }
            InvokeStatic | InvokeVirtual if args.len() >= 2 => {
                // One day we will actually split the function call,
                // But for now this will suffice.
                let (mut lhsv, l) = (TreeContainer::new(), args[0].clone());
                let mut nargs = vec![l, args[1].clone()];
                for arg in &args[2..] {
                    let (mut rhsv, r) = operand(arg, state);
                    lhsv.append(&mut rhsv);
                    nargs.push(r);
                }
                let op = Operand::T(ExprTree {
                    op: *op,
                    args: nargs,
                });
                wrap_prim(lhsv, op, state)
            }
            operator => panic!(
                "Unhandled Operator {:?} with nargs: {}",
                operator,
                args.len()
            ),
        },
    }
}

fn assignee(root: &Operand, state: &mut State) -> (TreeContainer, Operand) {
    match root {
        Operand::V(sym) => (TreeContainer::new(), Operand::V(*sym)),
        Operand::T(ExprTree { op, args }) => match op {
            Operation::Access | Operation::Index => {
                let (lhsv, l) = operand(&args[0], state);
                let r = args[1].clone();
                let op = Operand::T(ExprTree {
                    op: *op,
                    args: vec![l, r],
                });
                (lhsv, op)
            }
            other => panic!("Invalid Assignee Op: {:?}", other),
        },
        other => panic!("Invalid Assignee Operand: {:?}", other),
    }
}

fn without_set(op: Operation) -> Operation {
    use Operation::*;
    match op {
        PSet => Add,
        SSet => Sub,
        MSet => Mul,
        DSet => Div,
        ModSet => Mod,
        AndSet => And,
        OrSet => Or,
        XorSet => Xor,
        ShrSet => Shr,
        UshrSet => UShr,
        ShlSet => Shl,
        other => panic!("Invalid operation {:?}", other),
    }
}

fn tail(mut container: TreeContainer, el: Tree) -> TreeContainer {
    use Tree::*;
    if let Some(t) = container.back() {
        match t {
            Return(_) | Break(_) | Continue(_) => return container,
            _ => (),
        }
    }
    container.push_back(el);
    container
}
