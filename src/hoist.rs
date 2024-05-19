use std::collections::HashMap;

use crate::ir::*;
use crate::symbolmaker::{Symbol, SymbolMaker};

struct State<'l> {
    sm: &'l mut SymbolMaker,
    cont_header: HashMap<Symbol, TreeContainer>
}

impl<'l> State<'l> {
    fn new(sm: &'l mut SymbolMaker) -> Self {
        State { sm, cont_header: HashMap::new() }
    }
}

fn wrap_prim(mut list: TreeContainer, exp: Operand, state: &mut State) -> (TreeContainer, Operand) {
    let name = state.sm.fresh("prim");
    let tree = Tree::LetP(PrimStatement {
        name, typ: Typ::Unknown, exp: Some(exp)
    });
    list.push_back(tree);
    (list, Operand::V(name))
}

pub fn hoist(root: &Tree, sm: &mut SymbolMaker) -> Box<Tree> {
    let mut state = State::new(sm);
    Box::new(Tree::Program(statement(root, &mut state)))
}

fn statement(root: &Tree, state: &mut State) -> TreeContainer {
    match root {
        Tree::Program(stmts) => {
            let mut res = TreeContainer::new();
            for s in stmts { res.append(statement(s, state)) }
            res
        },
        Tree::LetI(import) => TreeContainer::make(root.clone()),
        Tree::LetF(fundecl) => {
            let mut body = TreeContainer::new();
            for s in &fundecl.body { body.append(statement(&s, state)) };
            TreeContainer::make(Tree::LetF(FunDeclaration {
                body, ..fundecl.clone()
            }))
        },
        Tree::LetC(classdecl) => {
            let mut methods = TreeContainer::new();
            for s in &classdecl.methods { methods.append(statement(&s, state)) };
            TreeContainer::make(Tree::LetC(ClassDeclaration {
                methods, ..classdecl.clone()
            }))
        }
        Tree::LetE(_) => todo!(),
        Tree::LetP(primdecl) =>  {
            if primdecl.exp.is_none() { return TreeContainer::make(root.clone()) }
            let e = primdecl.exp.as_ref().expect("");
            let (mut head, exp) = operand(&e, state);
            head.back_mut().map(|h| match h {
                Tree::LetP(PrimStatement { name, typ, exp }) => { *typ = primdecl.typ.clone(); }
                _ => panic!("Invalid LetP")
            });
            if primdecl.typ == Typ::Void { return head }
            head.push_back(Tree::LetP(PrimStatement {
                exp: Some(exp), ..primdecl.clone()
            }));
            return head
        },
        Tree::Block(bstmt) => {
            let mut bbody = TreeContainer::new();
            for s in &bstmt.bbody { bbody.append(statement(&s, state)) };
            TreeContainer::make(Tree::Block(BlockStatement { bbody, ..bstmt.clone() }))
        },
        Tree::Switch(sstmt) => {
            let (mut head, arg) = operand(&sstmt.arg, state);
            let mut cases = Vec::new();
            for (ops, code) in &sstmt.cases {
                let mut newcode = TreeContainer::new();
                for s in code { newcode.append(statement(&s, state)) }
                cases.push((ops.clone(), newcode));
            }
            head.push_back(Tree::Switch(SwitchStatement { cases, arg, ..sstmt.clone() }));
            head
        },
        Tree::Loop(lstmt) => {
            let mut res = TreeContainer::new();
            let (head, cond) = operand(&lstmt.cond, state);
            let mut lbody = TreeContainer::new();
            let end: TreeContainer = head.clone().into_iter().map(|s| match s {
                Tree::LetP(pdecl) => Tree::LetP(PrimStatement {
                    typ: Typ::Void, exp: Some(Operand::T(ExprTree {
                        op: Operation::Set,
                        args: vec![
                            Operand::V(pdecl.name),
                            pdecl.exp.expect("Pdecl exp is null.")
                        ]
                    })),
                    name: state.sm.fresh("void")
                }),
                _ => panic!("Invalid Tree returned from Operand")
            }).collect();
            state.cont_header.insert(lstmt.label, end)
                .map(|_| panic!("label: {} already in map!", state.sm.uname(lstmt.label)));
            for s in &lstmt.lbody { lbody.append(statement(&s, state)) };
            if lstmt.dowhile {
                match cond { // The easiest method is to insert a declaration.
                    Operand::T(_) => panic!("Operand should not be a tree."),
                    Operand::V(name) => Some(Tree::LetP(PrimStatement {
                        name, exp: None, typ: Typ::Bool
                    })),
                    _ => None
                }.map(|d| res.push_back(d));
            } else {
                res.append(head);
            }
            res.push_back(Tree::Loop(LoopStatement { cond, lbody, ..lstmt.clone() }));
            res
        },
        Tree::If(IfStatement { cond, label, btrue, bfalse }) => {
            let (mut head, cond) = operand(&cond, state);
            let (mut nbtrue, mut nbfalse) = (TreeContainer::new(), TreeContainer::new());
            btrue.iter().for_each(|s| nbtrue.append(statement(s, state)));
            bfalse.iter().for_each(|s| nbfalse.append(statement(s, state)));
            head.push_back(Tree::If(IfStatement {
                cond,
                label: *label,
                btrue: nbtrue,
                bfalse: nbfalse
            }));
            head
        }
        Tree::Try(_) => todo!(),
        Tree::Return(rstmt) => {
            if let Some(e) = &rstmt.val {
                let (mut head, exp) = operand(&e, state);
                head.push_back(Tree::Return(ReturnStatement {
                    val: Some(exp)
                }));
                return head
            }
            TreeContainer::make(root.clone())
        },
        Tree::Continue(label) => {
            let mut res = state.cont_header.get(&label).expect("Label is not in table!").clone();
            res.push_back(Tree::Continue(*label));
            res
        }
        Tree::Break(label) => TreeContainer::make(Tree::Break(*label)),
        Tree::EntryPoint(sym) => TreeContainer::make(Tree::EntryPoint(*sym)),
        _ => todo!()
    }
}

fn array_initializer(ops: &ArrayInitializer, state: &mut State) -> (TreeContainer, ArrayInitializer) {
    use ElementInitializer as E;
    let (lhsv, args) = ops.iter().map(|item| match item.as_ref() {
        E::Expr(exp) => { 
            let (v, e) = operand(exp, state);
            (v, Box::new(E::Expr(e)))
        }
        E::ArrayInitializer(a) => {
            let (v, e) = array_initializer(a, state);
            (v, Box::new(E::ArrayInitializer(e)))
        }
    }).collect::<Vec<_>>().into_iter().fold((TreeContainer::new(), Vec::new()), |acc, vec| {
        let (v, op) = vec;
        let (mut av, mut aops) = acc;
        av.append(v);
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
                    let (rhsv, r) = operand(arg, state);
                    lhsv.append(rhsv);
                    nops.push(r);
                }
                (lhsv, Operand::A(ArrayExpression::Empty(Box::new(nops))))
            },
            ArrayExpression::Initializer(a) => {
                let (v, e) = array_initializer(a, state);
                (v, Operand::A(ArrayExpression::Initializer(Box::new(e))))
            }
        },
        Operand::T(ExprTree { op, args }) => match op {
            Add | Sub | Mul | Div | Mod | Eq | Neq | G | L |
            GEq | LEq | LAnd | LOr | LNot | Shl |
            Shr | UShr | And | Or | Xor | InstanceOf |
            Phi | Assert | Index | Access if args.len() == 2 => {
                let (lhsv, l) = operand(&args[0], state);
                let (rhsv, r) = operand(&args[1], state);
                let op = Operand::T(ExprTree { op: *op, args: vec![l, r] });
                wrap_prim(lhsv + rhsv, op, state)
            },
            Set | PSet | SSet | MSet | DSet | ModSet |
            AndSet | OrSet | XorSet | ShrSet | UshrSet | ShlSet if args.len() == 2 => {
                let (lhsv, l) = assignee(&args[0], state);
                let (rhsv, r) = operand(&args[1], state);
                let op = Operand::T(ExprTree { op: *op, args: vec![l, r] });
                wrap_prim(lhsv + rhsv, op, state)
            },
            PreInc | PreDec | Not | LNot | Sub | PostInc | PostDec | Assert if args.len() == 1 => {
                let (v, e) = operand(&args[0], state);
                let op = Operand::T(ExprTree { op: *op, args: vec![e] });
                wrap_prim(v, op, state)
            },
            Ternary if args.len() == 3 => {
                let (lhsv, l) = operand(&args[0], state);
                let (rhsv, r) = operand(&args[1], state);
                let (mhsv, m) = operand(&args[2], state);
                let op = Operand::T(ExprTree { op: *op, args: vec![l, r, m] });
                wrap_prim(lhsv + rhsv + mhsv, op, state)
            },
            ArrayNew if args.len() == 2 => {
                let (rhsv, r) = operand(&args[1], state);
                let op = Operand::T(ExprTree { op: *op, args: vec![args[0].clone(), r] });
                wrap_prim(rhsv, op, state)
            },
            // Access if args.len() == 2 => (TreeContainer::new(), root.clone()),
            New if args.len() > 1 => {
                let mut lhsv = TreeContainer::new();
                let mut nargs = vec![args[0].clone()];
                for arg in &args[1..] {
                    let (rhsv, r) = operand(arg, state);
                    lhsv.append(rhsv);
                    nargs.push(r);
                }
                let op = Operand::T(ExprTree { op: *op, args: nargs });
                wrap_prim(lhsv, op, state)
            },
            InvokeStatic | InvokeVirtual if args.len() >= 2 => {
                // One day we will actually split the function call,
                // But for now this will suffice.
                let (mut lhsv, l) = (TreeContainer::new(), args[0].clone());
                let mut nargs = vec![l, args[1].clone()];
                for arg in &args[2..] {
                    let (rhsv, r) = operand(arg, state);
                    lhsv.append(rhsv);
                    nargs.push(r);
                }
                let op = Operand::T(ExprTree { op: *op, args: nargs });
                wrap_prim(lhsv, op, state)
            },
            operator => panic!(
                "Unhandled Operator {:?} with nargs: {}",
                operator, args.len()
            )
        }
    }
}

fn assignee(root: &Operand, state: &mut State) -> (TreeContainer, Operand) {
    match root {
        Operand::V(sym) => (TreeContainer::new(), Operand::V(*sym)),
        Operand::T(ExprTree { op, args }) => match op {
            Operation::Access | Operation::Index => {
                let (lhsv, l) = operand(&args[0], state);
                let r = args[1].clone();
                let op = Operand::T(ExprTree { op: *op, args: vec![l, r] });
                (lhsv, op)
            },
            other => panic!("Invalid Assignee Op: {:?}", other)
        },
        other => panic!("Invalid Assignee Operand: {:?}", other)
    }
}