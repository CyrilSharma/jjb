use crate::container::*;
use crate::ir::{Literal, Tree, TreeContainer, TreeContainerIter, Typ};
use crate::substitution::Substitution;
use crate::symbolmanager::{Symbol, SymbolManager};
use std::collections::HashMap;

#[derive(Clone)]
struct FunDef {
    body: TreeContainer,
    args: Vec<Symbol>,
    rtyp: Typ,
}

struct InlineInfo {
    label: Symbol,
    object: Option<Symbol>,
    name: Option<Symbol>,
}

struct OptimizeState<'l> {
    subst: Substitution<Symbol>,
    census: HashMap<Symbol, usize>,
    f_env: HashMap<Symbol, FunDef>,
    l_env: HashMap<Symbol, TreeContainer>,
    const_env: HashMap<Symbol, Literal>,
    next: Option<TreeContainerIter>,
    inline: Option<InlineInfo>,
    sm: &'l mut SymbolManager,
}

impl<'l> OptimizeState<'l> {
    pub fn new(census: HashMap<Symbol, usize>, sm: &'l mut SymbolManager) -> Self {
        Self {
            census,
            subst: Substitution::new(),
            f_env: HashMap::new(),
            l_env: HashMap::new(),
            const_env: HashMap::new(),
            inline: None,
            next: None,
            sm,
        }
    }

    pub fn applied_once(&self, sym: Symbol) -> bool {
        self.census.get(&sym).map(|c| *c == 1).unwrap_or(false)
    }

    pub fn dead(&self, sym: Symbol) -> bool {
        self.census.get(&sym).map(|_| false).unwrap_or(true)
    }

    pub fn subst(&self, sym: Symbol) -> Symbol {
        self.subst.subst(sym)
    }

    pub fn addf(&mut self, name: Symbol, f: FunDef) {
        self.f_env.insert(name, f);
    }

    pub fn addc(&mut self, sym: Symbol) {
        let mut cont = TreeContainer::new();
        self.next
            .clone()
            .map(|n| n.for_each(|c| cont.push_back(c.clone())));
        self.l_env.insert(sym, cont);
    }

    pub fn add_const(&mut self, name: Symbol, l: Literal) {
        self.const_env.insert(name, l);
    }

    pub fn add_subst(&mut self, name: Symbol, nname: Symbol) {
        self.subst.add_subst(name, nname);
    }

    pub fn with_inline<T>(
        &mut self,
        label: Symbol,
        name: Option<Symbol>,
        object: Option<Symbol>,
        f: impl FnOnce(&mut OptimizeState) -> T,
    ) -> T {
        let inline = Some(InlineInfo {
            label,
            object,
            name,
        });
        let stash = std::mem::replace(&mut self.inline, inline);
        let res = f(self);
        self.inline = stash;
        res
    }

    pub fn withoutApps<T>(&mut self, f: impl FnOnce(&mut OptimizeState) -> T) -> T {
        let f_env = std::mem::take(&mut self.f_env);
        let l_env = std::mem::take(&mut self.l_env);
        let res = f(self);
        self.f_env = f_env;
        self.l_env = l_env;
        res
    }

    pub fn withSubst<T>(
        &mut self,
        args: Vec<Symbol>,
        nargs: Vec<Symbol>,
        f: impl FnOnce(&mut OptimizeState) -> T,
    ) -> T {
        // This can be made more efficient.
        let subst = self.subst.clone();
        for (arg, narg) in args.iter().zip(nargs.iter()) {
            self.subst.add_subst(*arg, *narg)
        }
        let res = f(self);
        self.subst = subst;
        res
    }
}

pub fn optimize(tree: &Tree, sm: &mut SymbolManager) -> Box<Tree> {
    let cur = tree.clone();
    shrink::shrink(cur, sm)
}

pub mod shrink {
    use std::collections::HashSet;

    use super::census;
    use super::{FunDef, OptimizeState as State};
    use crate::ir::*;
    use crate::optimizer::ContainerHelpers;
    use crate::symbolmanager::{Symbol, SymbolManager};

    pub fn shrink(root: Tree, sm: &mut SymbolManager) -> Box<Tree> {
        let counts = census::census(&root);
        let mut state = State::new(counts, sm);
        initialize_state(&root, &mut state);
        // This should either be fixed point iteration or a worklist algorithm.
        // As a naive starting point, you can make this faster by
        // Tracking on a per function basis if it needs restarting.
        // Doing all the functions in parallel.
        Box::new({
            let mut cur = root;
            for _ in 0..20 {
                cur = Tree::Program(traverse(cur, &mut state));
                state.census = census::census(&cur);
            }
            cur
        })
    }

    fn initialize_state(root: &Tree, state: &mut State) {
        match root {
            Tree::Program(s) => {
                for stmt in s {
                    initialize_state(stmt, state)
                }
            }
            Tree::LetC(c) => {
                for method in &c.methods {
                    initialize_state(method, state)
                }
            }
            Tree::LetF(f) => {
                if state.applied_once(f.name) {
                    let args = f.args.iter().map(|(a, tp)| *a).collect();
                    state.addf(
                        f.name,
                        FunDef {
                            body: f.body.clone(),
                            args,
                            rtyp: f.return_typ.clone(),
                        },
                    )
                }
            }
            _ => (),
        }
    }

    fn contdead(root: &Tree, state: &State) -> bool {
        let f = |l: Symbol| (state.applied_once(l));
        match root {
            Tree::Block(t) => f(t.label),
            Tree::Switch(t) => f(t.label),
            Tree::Loop(t) => f(t.label),
            Tree::If(t) => f(t.label),
            Tree::Return(_) | Tree::Break(_) | Tree::Continue(_) => true,
            _ => false,
        }
    }

    // This would probably be cleaner if the next pointer of branchy code
    // Was stored in the tree itself. Probably not worth changing.
    fn traverselist(list: TreeContainer, state: &mut State) -> TreeContainer {
        let mut res = TreeContainer::new();
        let mut iter = list.into_iter();
        let mut cur = iter.next();
        while let Some(stmt) = cur {
            state.next = Some(iter.clone());
            let cdead = contdead(&stmt, state);
            res.extend(traverse(stmt, state));
            if cdead {
                break;
            }
            cur = iter.next();
        }
        state.next = None;
        res
    }

    fn traverse(root: Tree, state: &mut State) -> TreeContainer {
        use Operand as Op;
        use Operation::*;
        match root {
            Tree::Program(stmts) => traverselist(stmts, state),
            Tree::LetI(_) => TreeContainer::make(root),
            Tree::LetF(f) if state.dead(f.name) => TreeContainer::new(),
            Tree::LetF(f) => TreeContainer::make(Tree::LetF(FunDeclaration {
                body: traverselist(f.body, state),
                ..f
            })),
            // The constructor is considered a method + no innner classes / enums.
            Tree::LetC(c) if c.methods.len() == 0 => TreeContainer::new(),
            Tree::LetC(c) => TreeContainer::make(Tree::LetC(ClassDeclaration {
                methods: traverselist(c.methods, state),
                ..c
            })),
            Tree::LetE(_) => todo!(),
            Tree::LetP(PrimStatement {
                exp: None,
                name: Some(name),
                ..
            }) => {
                if state.dead(name) {
                    TreeContainer::new()
                } else {
                    TreeContainer::make(root)
                }
            }
            Tree::LetP(PrimStatement { exp, name, typ })
                if matches!(
                    &exp,
                    Some(Op::T(ExprTree {
                        op: InvokeVirtual | InvokeStatic | New,
                        ..
                    }))
                ) =>
            {
                // Disable function inlining for now.
                let exp = exp.map(|op| substop(op, state));
                TreeContainer::make(Tree::LetP(PrimStatement { exp, name, typ }))

                // let idx = if matches!(op, New) { 0 } else { 1 };
                // let fsym = match args[idx] { Op::V(sym) => sym, _ => panic!("Invalid Call") };
                // if let Some(f) = state.f_env.get(&fsym).cloned() {
                //     let cargs = args.iter().map(|s| match s {
                //         Op::V(sym) => state.subst(*sym),
                //         _ => panic!("Function should only have symbolic arguments!")
                //     }).collect();
                //     let nargs = f.args.clone();
                //     let funname = state.sm.name(fsym).to_owned();
                //     let label = state.sm.fresh(&funname);
                //     let obj = if matches!(op, InvokeVirtual) {
                //         Some(match args[0] { Op::V(sym) => sym, _ => panic!("Invalid Call") })
                //     } else {
                //         None
                //     };

                //     let mut list = TreeContainer::new();
                //     let bbody = state.with_inline(label, *name, obj, |s|
                //         s.withoutApps(|s|
                //             s.withSubst(cargs, nargs, |s|
                //                 traverselist(f.body.clone(), s)
                //             )
                //         )
                //     );
                //     list.push_back(Tree::LetP(PrimStatement { name: *name, typ: f.rtyp.clone(), exp: None }));
                //     list.push_back(Tree::Block(BlockStatement { label, bbody }));
                //     list
                // } else {
                //     TreeContainer::make(root)
                // }
            }
            Tree::LetP(PrimStatement {
                exp: Some(Op::T(ExprTree { op: Phi, args })),
                name: Some(name),
                typ,
            }) => {
                if state.dead(name) {
                    return TreeContainer::new();
                }
                let mut processed = HashSet::new();
                processed.insert(name);
                let mut nargs = vec![args[0].clone()];
                for i in 1..args.len() {
                    match args[i] {
                        Op::V(sym) => {
                            let nsym = state.subst(sym);
                            if !processed.contains(&nsym) && !state.dead(sym) {
                                nargs.push(Operand::V(nsym));
                                processed.insert(nsym);
                            }
                        }
                        _ => panic!("Invalid Phi Node!"),
                    }
                }
                if nargs.len() == 1 {
                    return TreeContainer::new();
                }
                if nargs.len() == 2 {
                    return TreeContainer::make(Tree::LetP(PrimStatement {
                        exp: Some(args[1].clone()),
                        name: Some(name),
                        typ,
                    }));
                }
                TreeContainer::make(Tree::LetP(PrimStatement {
                    exp: Some(Op::T(ExprTree {
                        op: Phi,
                        args: nargs,
                    })),
                    name: Some(name),
                    typ,
                }))
            }
            Tree::LetP(PrimStatement {
                exp: Some(Operand::C(l)),
                name: Some(name),
                typ,
            }) => {
                if state.dead(name) {
                    return TreeContainer::new();
                }
                state.add_const(name, l.clone());
                return TreeContainer::make(Tree::LetP(PrimStatement {
                    exp: Some(Operand::C(l)),
                    name: Some(name),
                    typ,
                }));
            }
            Tree::LetP(PrimStatement {
                exp: Some(Operand::V(l)),
                name: Some(name),
                ..
            }) => {
                if state.dead(name) {
                    return TreeContainer::new();
                }
                let nname = state.subst(l);
                if name != nname {
                    state.add_subst(name, nname)
                }
                return TreeContainer::new();
            }
            Tree::LetP(PrimStatement {
                exp: Some(Operand::A(l)),
                name: Some(name),
                typ,
            }) => {
                if state.dead(name) {
                    return TreeContainer::new();
                }
                TreeContainer::make(Tree::LetP(PrimStatement {
                    exp: Some(substop(Operand::A(l), state)),
                    name: Some(name),
                    typ,
                }))
            }
            Tree::LetP(PrimStatement {
                exp: Some(Operand::T(e)),
                name: Some(name),
                typ,
            }) => TreeContainer::make(Tree::LetP(PrimStatement {
                name: Some(name),
                typ,
                exp: Some(substop(Operand::T(e), state)),
            })),
            Tree::LetP(PrimStatement { name, typ, exp }) => {
                return TreeContainer::make(Tree::LetP(PrimStatement {
                    name,
                    typ,
                    exp: exp.map(|e| substop(e, state)),
                }))
            }
            Tree::Block(b) => {
                if state.applied_once(b.label) {
                    state.addc(b.label)
                }
                let bbody = traverselist(b.bbody, state);
                if state.dead(b.label) {
                    return bbody;
                }
                TreeContainer::make(Tree::Block(BlockStatement {
                    bbody,
                    label: b.label,
                }))
            }
            Tree::Switch(s) => {
                if state.applied_once(s.label) {
                    state.addc(s.label)
                }
                let mut cases = Vec::new();
                for (ops, case) in s.cases {
                    cases.push((ops, traverselist(case, state)))
                }
                let default = traverselist(s.default, state);
                TreeContainer::make(Tree::Switch(SwitchStatement {
                    arg: substop(s.arg, state),
                    label: s.label,
                    cases,
                    default,
                }))
            }
            Tree::Loop(LoopStatement {
                cond,
                label,
                lbody,
                dowhile,
            }) => {
                if state.applied_once(label) {
                    state.addc(label)
                }
                let nbody = traverselist(lbody, state);
                TreeContainer::make(Tree::Loop(LoopStatement {
                    cond: substop(cond, state),
                    label,
                    lbody: nbody,
                    dowhile,
                }))
            }
            Tree::If(IfStatement {
                cond,
                label,
                btrue,
                bfalse,
            }) => {
                if state.applied_once(label) {
                    state.addc(label)
                }
                let ntrue = traverselist(btrue, state);
                let nfalse = traverselist(bfalse, state);
                TreeContainer::make(match cond {
                    Operand::C(Literal::Bool(true)) => Tree::Block(BlockStatement {
                        label,
                        bbody: ntrue,
                    }),
                    Operand::C(Literal::Bool(false)) => Tree::Block(BlockStatement {
                        label,
                        bbody: nfalse,
                    }),
                    _ => Tree::If(IfStatement {
                        cond: substop(cond, state),
                        label,
                        btrue: ntrue,
                        bfalse: nfalse,
                    }),
                })
            }
            Tree::Break(label) => {
                if let Some(cont) = state.l_env.get(&label).cloned() {
                    if cont.len() > 0 {
                        traverselist(cont.clone(), state)
                    } else {
                        TreeContainer::make(Tree::Break(label))
                    }
                } else {
                    TreeContainer::make(Tree::Break(label))
                }
            }
            Tree::Try(_) => todo!(),
            // We wrap functions in a block and break from the block.
            // This is necessary to handle arbitrary control flow.
            Tree::Return(ReturnStatement { val: Some(e) }) if state.inline.is_some() => {
                let inline = state.inline.as_ref().unwrap();
                let mut res = TreeContainer::new();
                res.push_back(Tree::LetP(PrimStatement {
                    name: inline.name,
                    typ: Typ::Void,
                    exp: Some(e),
                }));
                res.push_back(Tree::Break(inline.label));
                res
            }
            Tree::Return(ReturnStatement { val: None }) if state.inline.is_some() => {
                let inline = state.inline.as_ref().unwrap();
                TreeContainer::make(Tree::Break(inline.label))
            }
            Tree::Return(ReturnStatement { val }) => {
                TreeContainer::make(Tree::Return(ReturnStatement {
                    val: val.map(|op| substop(op, state)),
                }))
            }
            Tree::Continue(_) => TreeContainer::make(root),
            Tree::EntryPoint(_) => TreeContainer::make(root),
        }
    }

    fn const_op(op: Operation) -> bool {
        match op {
            Operation::InstanceOf
            | Operation::New
            | Operation::ArrayNew
            | Operation::InvokeVirtual
            | Operation::InvokeStatic
            | Operation::Phi
            | Operation::Access
            | Operation::Index
            | Operation::Assert
            | Operation::Throw => false,
            _ => true,
        }
    }

    fn const_eval(op: Operation, args: &Vec<Literal>) -> Literal {
        match args.as_slice() {
            [a, b, c] => terneval(op, a.clone(), b.clone(), c.clone()),
            [a, b] => bineval(op, a.clone(), b.clone()),
            [a] => uneval(op, a.clone()),
            _ => panic!("Empty arguments list"),
        }
    }

    fn terneval(op: Operation, a: Literal, b: Literal, c: Literal) -> Literal {
        match a {
            Literal::Bool(true) => b,
            Literal::Bool(false) => c,
            _ => panic!("Invalid Ternary"),
        }
    }

    fn bineval(op: Operation, a: Literal, b: Literal) -> Literal {
        if let (Some(ai), Some(bi)) = (a.get_int(), b.get_double()) {
            let ai = ai as f64;
            match op {
                Operation::Eq => return Literal::Bool(ai == bi),
                Operation::Neq => return Literal::Bool(ai != bi),
                Operation::G => return Literal::Bool(ai > bi),
                Operation::L => return Literal::Bool(ai < bi),
                Operation::GEq => return Literal::Bool(ai >= bi),
                Operation::LEq => return Literal::Bool(ai <= bi),
                _ => (),
            }
            let eval = match op {
                Operation::Add => ai + bi,
                Operation::Sub => ai - bi,
                Operation::Mul => ai * bi,
                Operation::Div => ai / bi,
                Operation::Mod => ai % bi,
                _ => panic!("Unknown Integer Operation!"),
            };
            return match b.double_rank() {
                0 => panic!("Invalid Integer Operation"),
                1 => Literal::Float(eval as f32),
                2 => Literal::Double(eval.try_into().unwrap()),
                _ => panic!("Impossible integer type"),
            };
        }
        if let (Some(ai), Some(bi)) = (a.get_double(), b.get_int()) {
            let bi = bi as f64;
            match op {
                Operation::Eq => return Literal::Bool(ai == bi),
                Operation::Neq => return Literal::Bool(ai != bi),
                Operation::G => return Literal::Bool(ai > bi),
                Operation::L => return Literal::Bool(ai < bi),
                Operation::GEq => return Literal::Bool(ai >= bi),
                Operation::LEq => return Literal::Bool(ai <= bi),
                _ => (),
            }
            let eval = match op {
                Operation::Add => ai + bi,
                Operation::Sub => ai - bi,
                Operation::Mul => ai * bi,
                Operation::Div => ai / bi,
                Operation::Mod => ai % bi,
                _ => panic!("Unknown Integer Operation!"),
            };
            return match a.double_rank() {
                0 => panic!("Invalid Integer Operation"),
                1 => Literal::Float(eval as f32),
                2 => Literal::Double(eval.try_into().unwrap()),
                _ => panic!("Impossible integer type"),
            };
        }
        if let (Some(ai), Some(bi)) = (a.get_int(), b.get_int()) {
            match op {
                Operation::Eq => return Literal::Bool(ai == bi),
                Operation::Neq => return Literal::Bool(ai != bi),
                Operation::G => return Literal::Bool(ai > bi),
                Operation::L => return Literal::Bool(ai < bi),
                Operation::GEq => return Literal::Bool(ai >= bi),
                Operation::LEq => return Literal::Bool(ai <= bi),
                _ => (),
            }
            let eval = match op {
                Operation::Add => ai + bi,
                Operation::Sub => ai - bi,
                Operation::Mul => ai * bi,
                Operation::Div => ai / bi,
                Operation::Mod => ai % bi,
                Operation::Shl => ai << bi,
                Operation::Shr => ai >> bi,
                Operation::UShr => ((ai as u64) >> bi) as i64,
                Operation::And => ai & bi,
                Operation::Or => ai | bi,
                Operation::Xor => ai ^ bi,
                _ => panic!("Unknown Integer Operation!"),
            };
            // This is technically wrong in the event you wanted overflow.
            let mx = std::cmp::max(a.int_rank(), b.int_rank());
            return match mx {
                0 => panic!("Invalid Integer Operation"),
                1 => Literal::Byte(eval.try_into().unwrap()),
                2 => Literal::Short(eval.try_into().unwrap()),
                3 => Literal::Int(eval.try_into().unwrap()),
                4 => Literal::Long(eval),
                _ => panic!("Impossible integer type"),
            };
        }
        if let (Some(ai), Some(bi)) = (a.get_double(), b.get_double()) {
            match op {
                Operation::Eq => return Literal::Bool(ai == bi),
                Operation::Neq => return Literal::Bool(ai != bi),
                Operation::G => return Literal::Bool(ai > bi),
                Operation::L => return Literal::Bool(ai < bi),
                Operation::GEq => return Literal::Bool(ai >= bi),
                Operation::LEq => return Literal::Bool(ai <= bi),
                _ => (),
            }
            let eval = match op {
                Operation::Add => ai + bi,
                Operation::Sub => ai - bi,
                Operation::Mul => ai * bi,
                Operation::Div => ai / bi,
                Operation::Mod => ai % bi,
                _ => panic!("Unknown Integer Operation!"),
            };
            // This is technically wrong in the event you wanted overflow.
            let mx = std::cmp::max(a.double_rank(), b.double_rank());
            return match mx {
                0 => panic!("Invalid Integer Operation"),
                1 => Literal::Float(eval as f32),
                2 => Literal::Double(eval.try_into().unwrap()),
                _ => panic!("Impossible Float type"),
            };
        }
        if let (Literal::Bool(ab), Literal::Bool(bb)) = (a, b) {
            return match op {
                Operation::Eq => Literal::Bool(ab == bb),
                Operation::Neq => Literal::Bool(ab != bb),
                Operation::LAnd => Literal::Bool(ab && bb),
                Operation::LOr => Literal::Bool(ab || bb),
                _ => panic!("Invalid Conditional Operation!"),
            };
        }
        panic!("Unknown Binary Operation")
    }

    fn uneval(op: Operation, a: Literal) -> Literal {
        match a {
            Literal::Bool(b) => match op {
                Operation::LNot => Literal::Bool(!b),
                _ => panic!("Unknown"),
            },
            Literal::Byte(b) => match op {
                Operation::Sub => Literal::Byte(-b),
                Operation::Not => Literal::Byte(!b),
                _ => panic!("Unknown"),
            },
            Literal::Short(b) => match op {
                Operation::Sub => Literal::Short(-b),
                Operation::Not => Literal::Short(!b),
                _ => panic!("Unknown"),
            },
            Literal::Int(b) => match op {
                Operation::Sub => Literal::Int(-b),
                Operation::Not => Literal::Int(!b),
                _ => panic!("Unknown"),
            },
            Literal::Long(b) => match op {
                Operation::Sub => Literal::Long(-b),
                Operation::Not => Literal::Long(!b),
                _ => panic!("Unknown"),
            },
            Literal::Float(b) => match op {
                Operation::Sub => Literal::Float(-b),
                _ => panic!("Unknown"),
            },
            Literal::Double(b) => match op {
                Operation::Sub => Literal::Double(-b),
                _ => panic!("Unknown"),
            },
            _ => panic!("Invalid Unary Operation"),
        }
    }

    fn substop(op: Operand, state: &mut State) -> Operand {
        use ArrayExpression as A;
        match op {
            Operand::This(c) => {
                if let Some(sym) = state.inline.as_ref().and_then(|obj| obj.object) {
                    return Operand::V(sym);
                }
                Operand::This(c)
            }
            // Literally wrong, but we don't really support super atm.
            Operand::Super(s) => Operand::Super(s),
            Operand::V(sym) => {
                if let Some(l) = state.const_env.get(&sym) {
                    return Operand::C(l.clone());
                }
                Operand::V(state.subst(sym))
            }
            Operand::T(ExprTree { op, args }) => {
                if const_op(op)
                    && args.iter().all(|a| match a {
                        Operand::C(_) => true,
                        _ => false,
                    })
                {
                    let nargs = args
                        .iter()
                        .map(|a| match a {
                            Operand::C(l) => l.clone(),
                            _ => panic!(),
                        })
                        .collect();
                    return Operand::C(const_eval(op, &nargs));
                }
                let nargs = args.into_iter().map(|a| substop(a, state)).collect();
                Operand::T(ExprTree { op, args: nargs })
            }
            Operand::A(A::Empty(a)) => Operand::A(A::Empty(Box::new(
                a.into_iter().map(|a| substop(a, state)).collect(),
            ))),
            Operand::A(A::Initializer(a)) => {
                Operand::A(A::Initializer(Box::new(substarray(*a, state))))
            }
            other => other,
        }
    }

    fn substarray(a: ArrayInitializer, state: &mut State) -> ArrayInitializer {
        use ElementInitializer as E;
        a.into_iter()
            .map(|e| {
                Box::new(match *e {
                    E::Expr(op) => E::Expr(substop(op, state)),
                    E::ArrayInitializer(c) => E::ArrayInitializer(substarray(c, state)),
                })
            })
            .collect()
    }

    fn impure(op: Operation, lvalue: bool) -> bool {
        match op {
            Operation::InvokeVirtual => true,
            Operation::InvokeStatic => true,
            Operation::Assert => true,
            Operation::Throw => true,
            Operation::Access => lvalue,
            Operation::Index => lvalue,
            _ => false,
        }
    }

    fn unstable(op: Operation, lvalue: bool) -> bool {
        match op {
            Operation::InvokeVirtual => true,
            // It might take in an object, which would make it unstable.
            Operation::InvokeStatic => true,
            Operation::Assert => true,
            Operation::Throw => true,
            Operation::Access => lvalue,
            Operation::Index => lvalue,
            _ => false,
        }
    }
}

pub mod inline {}

pub mod census {
    use crate::ir::*;
    use crate::symbolmanager::Symbol;
    use std::collections::HashMap;
    pub struct Census {
        map: HashMap<Symbol, usize>,
    }
    impl Census {
        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }
        pub fn inc(&mut self, sym: Symbol) {
            if let Some(s) = self.map.get_mut(&sym) {
                *s = *s + 1;
            } else {
                self.map.insert(sym, 1);
            }
        }
    }

    pub fn census(root: &Tree) -> HashMap<Symbol, usize> {
        let mut state = Census::new();
        traverse(root, &mut state);
        state.map
    }

    #[allow(unused)]
    fn traverse(root: &Tree, state: &mut Census) {
        match root {
            Tree::Program(stmts) => stmts.iter().for_each(|s| traverse(s, state)),
            Tree::LetI(ImportDeclaration { path }) => (),
            Tree::LetF(f) => f.body.iter().for_each(|b| traverse(b, state)),
            Tree::LetC(c) => c.methods.iter().for_each(|m| traverse(m, state)),
            Tree::LetE(_) => todo!(),
            Tree::LetP(p) => p.exp.iter().for_each(|e| operand(e, state)),
            Tree::Block(b) => b.bbody.iter().for_each(|s| traverse(s, state)),
            Tree::Switch(SwitchStatement {
                arg,
                label,
                cases,
                default,
            }) => {
                operand(arg, state);
                for (ops, code) in cases {
                    code.iter().for_each(|t| traverse(t, state))
                }
                for d in default {
                    traverse(d, state)
                }
            }
            Tree::Loop(LoopStatement {
                cond,
                label,
                lbody,
                dowhile,
            }) => {
                operand(cond, state);
                for l in lbody {
                    traverse(l, state)
                }
            }
            Tree::If(IfStatement {
                cond,
                label,
                btrue,
                bfalse,
            }) => {
                operand(cond, state);
                for b in btrue {
                    traverse(b, state)
                }
                for b in bfalse {
                    traverse(b, state)
                }
            }
            Tree::Try(_) => todo!(),
            Tree::Return(r) => r.val.iter().for_each(|v| operand(&v, state)),
            Tree::Break(label) => state.inc(*label),
            Tree::EntryPoint(sym) => state.inc(*sym),
            Tree::Continue(label) => (),
        }
    }

    fn operand(op: &Operand, state: &mut Census) {
        use Operation::*;
        match op {
            Operand::This(_) => (),
            Operand::Super(_) => (),
            Operand::C(_) => (),
            Operand::V(sym) => state.inc(*sym),
            Operand::T(ExprTree { op: New, args }) => match args.as_slice() {
                [Operand::V(sym), rest @ ..] => {
                    state.inc(*sym);
                    rest.iter().for_each(|op| operand(op, state));
                }
                _ => assert!(false, "Invalid Call operation!"),
            },
            Operand::T(ExprTree {
                op: InvokeVirtual,
                args,
            }) => match args.as_slice() {
                [_, Operand::V(sym), rest @ ..] => {
                    state.inc(*sym);
                    rest.iter().for_each(|op| operand(op, state));
                }
                _ => assert!(false, "Invalid Call operation!"),
            },
            Operand::T(ExprTree {
                op: InvokeStatic,
                args,
            }) => match args.as_slice() {
                [_, Operand::V(sym), rest @ ..] => {
                    state.inc(*sym);
                    rest.iter().for_each(|op| operand(op, state));
                }
                _ => assert!(false, "Invalid Call operation!"),
            },
            Operand::T(ExprTree { op, args }) => args.iter().for_each(|a| operand(a, state)),
            Operand::A(_) => (), /* todo!() */
            Operand::Tp(_) => (),
        }
    }
}

// Computes size as measured by space complexity.
// Hence, an array initializer takes up a huge amount of space,
// A function call takes up very little, and so on.
pub mod size {
    use crate::ir::*;
    use crate::symbolmanager::Symbol;
    use std::collections::HashMap;
    struct State {
        map: HashMap<Symbol, usize>,
        switch: bool,
    }

    impl State {
        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
                switch: false,
            }
        }
        pub fn add(&mut self, sym: Symbol, size: usize) {
            if self.switch {
                return;
            }
            self.map.insert(sym, size);
        }
        fn switch(&mut self, f: impl FnOnce(&mut State) -> usize) -> usize {
            let stash = self.switch;
            self.switch = true;
            let res = f(self);
            self.switch = stash;
            res
        }
    }

    pub fn size(root: &Tree) -> HashMap<Symbol, usize> {
        let mut state = State::new();
        traverse(root, 0, &mut state);
        state.map
    }

    fn sum(list: &TreeContainer, state: &mut State) -> usize {
        list.iter().rfold(0, |acc, b| traverse(b, acc, state))
    }

    #[allow(unused)]
    fn traverse(root: &Tree, bsize: usize, state: &mut State) -> usize {
        bsize
            + match root {
                Tree::Program(stmts) => sum(stmts, state),
                Tree::LetI(ImportDeclaration { path }) => 0,
                Tree::LetF(f) => sum(&f.body, state),
                Tree::LetC(c) => sum(&c.methods, state),
                Tree::LetE(_) => todo!(),
                Tree::LetP(p) => p.exp.as_ref().map(|e| operand(&e)).unwrap_or(0),
                Tree::Block(b) => {
                    state.add(b.label, bsize);
                    sum(&b.bbody, state)
                }
                Tree::Switch(s) => {
                    state.add(s.label, bsize);
                    state.switch(|sw| {
                        let cscore: usize = s.cases.iter().map(|(op, code)| sum(code, sw)).sum();
                        let dscore = sum(&s.default, sw);
                        cscore + dscore
                    })
                }
                Tree::Loop(l) => {
                    state.add(l.label, bsize);
                    sum(&l.lbody, state) + operand(&l.cond)
                }
                Tree::If(i) => {
                    state.add(i.label, bsize);
                    sum(&i.btrue, state) + sum(&i.bfalse, state)
                }
                Tree::Try(_) => todo!(),
                Tree::Return(r) => 1 + r.val.as_ref().map(|e| operand(&e)).unwrap_or(0),
                Tree::Continue(label) => 1,
                Tree::Break(label) => 1,
                Tree::EntryPoint(sym) => 0,
            }
    }

    fn array_size(a: &ArrayInitializer) -> usize {
        use ElementInitializer as E;
        a.iter().fold(0, |acc, item| match item.as_ref() {
            E::Expr(_) => 1,
            E::ArrayInitializer(child) => array_size(child),
        })
    }

    fn operand(op: &Operand) -> usize {
        use ArrayExpression as A;
        use Operand as O;
        use Operation::*;
        match op {
            O::This(_) => 1,
            O::Super(_) => 1,
            O::C(_) => 1,
            O::V(_) => 1,
            O::A(array) => match array {
                A::Empty(bv) => bv.len(),
                A::Initializer(a) => array_size(a.as_ref()),
            },
            O::T(ExprTree { op: ArrayNew, args }) => match args.as_slice() {
                [O::Tp(Typ::Array(_)), a] => operand(a),
                _ => panic!("Invalid Array Expression!"),
            },
            O::T(ExprTree { op: New, args }) => args.len(),
            O::T(ExprTree {
                op: InvokeVirtual,
                args,
            }) => args.len(),
            O::T(ExprTree {
                op: InvokeStatic,
                args,
            }) => args.len(),
            O::T(ExprTree { op, args }) => args.len(),
            O::Tp(_) => 0,
        }
    }
}
