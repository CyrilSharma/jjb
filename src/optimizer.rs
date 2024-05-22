
use crate::ir::*;
use crate::symbolmaker::SymbolMaker;
pub struct State {

}

pub fn optimize(tree: &Tree, sm: &mut SymbolMaker) -> Box<Tree> {
    todo!()
}

pub mod shrink {
    use std::collections::HashMap;
    use crate::container::*;
    use crate::ir::*;
    use crate::substitution::Substitution;
    use crate::symbolmaker::{Symbol, SymbolMaker};
    use super::census::census;

    #[derive(Clone)]
    struct FunDef {
        name: Symbol,
        body: TreeContainer,
        args: Vec<Symbol>
    }

    pub struct InlineInfo {
        label: Symbol,
        object: Option<Symbol>,
        name: Symbol
    }

    pub struct State<'l> {
        subst: Substitution<Symbol>,
        census: HashMap<Symbol, usize>,
        f_env: HashMap<Symbol, FunDef>,
        l_env: HashMap<Symbol, TreeContainer>,
        next: Option<ContainerIntoIter<Tree>>,
        inline: Option<InlineInfo>,
        sm: &'l mut SymbolMaker
    }

    impl<'l> State<'l> {
        pub fn new(census: HashMap<Symbol, usize>, sm: &'l mut SymbolMaker) -> Self {
            Self {
                census,
                subst: Substitution::new(),
                f_env: HashMap::new(),
                l_env: HashMap::new(),
                inline: None,
                next: None,
                sm
            }
        }

        pub fn applied_once(&self, sym: Symbol) -> bool {
            self.census.get(&sym).map(|c| *c == 1).unwrap_or(false)
        }

        pub fn dead(&self, sym: Symbol) -> bool {
            self.census.get(&sym).map(|c| false).unwrap_or(true)
        }

        pub fn subst(&self, sym: Symbol) -> Symbol {
            self.subst.subst(sym)
        }

        pub fn add_subst(&mut self, a: Symbol, b: Symbol) {
            self.subst.add_subst(a, b);
        }

        pub fn addf(&mut self, sym: Symbol, tc: &TreeContainer) {
            self.f_env.insert(sym, FunDef {
                args: Vec::new(), body: tc.clone()
            });
        }

        pub fn addc(&mut self, sym: Symbol) {
            let mut cont = TreeContainer::new();
            self.next.clone().map(|n| n.for_each(|c| cont.push_back(c.clone())));
            self.l_env.insert(sym, cont);
        }

        pub fn with_inline<T>(&mut self, label: Symbol, name: Symbol,
            object: Option<Symbol>, f: impl FnOnce(&mut State) -> T) -> T {
            let inline = Some(InlineInfo { label, object, name });
            let stash = std::mem::replace(&mut self.inline, inline);
            let res = f(self);
            self.inline = stash;
            res
        }

        pub fn withoutApps<T>(&mut self, f: impl FnOnce(&mut State) -> T) -> T {
            let f_env = std::mem::take(&mut self.f_env);
            let l_env = std::mem::take(&mut self.l_env);
            let res = f(self);
            self.f_env = f_env;
            self.l_env = l_env;
            res
        }

        pub fn withSubst<T>(&mut self, args: Vec<Symbol>, nargs: Vec<Symbol>,
            f: impl FnOnce(&mut State) -> T) -> T {
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

    pub fn shrink(root: Tree, sm: &mut SymbolMaker) -> Box<Tree> {
        let mut state = State::new(census(&root), sm);
        initialize_state(&root, &mut state);
        Box::new(Tree::Program(traverse(root, &mut state)))
    }

    fn initialize_state(root: &Tree, state: &mut State) {
        match root {
            Tree::Program(s) => for stmt in s { initialize_state(stmt, state) },
            Tree::LetC(c) => for method in &c.methods { initialize_state(method, state) }
            Tree::LetF(f) => if state.applied_once(f.name) { state.addf(f.name, &f.body) }
            _ => ()
        }
    }

    fn contdead(root: &Tree, state: &State) -> bool {
        match root {
            Tree::Block(t) => state.dead(t.label),
            Tree::Switch(t) => state.dead(t.label),
            Tree::Loop(t) => state.dead(t.label),
            Tree::If(t) => state.dead(t.label),
            Tree::Return(_) | Tree::Break(_) |Tree::Continue(_) => true,
            _ => false
        }
    }

    // This would probably be cleaner if the next pointer of branchy code
    // Was stored in that code itself. Probably not worth changing.
    fn traverselist(list: TreeContainer, state: &mut State) -> TreeContainer {
        let mut res = TreeContainer::new();
        let mut iter = list.into_iter();
        let mut cur = iter.next();
        let mut next = iter.next();
        while let Some(stmt) = cur {
            state.next = Some(iter.clone());
            let cdead = contdead(&stmt, state);
            res.append(traverse(stmt, state));
            if cdead { break }
            next = iter.next();
            cur = next;
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
                body: traverselist(f.body, state), ..f
            })),
            // The constructor is considered a method + no innner classes / enums.
            Tree::LetC(c) if c.methods.len() == 0 => TreeContainer::new(),
            Tree::LetC(c) => TreeContainer::make(Tree::LetC(ClassDeclaration {
                methods: traverselist(c.methods, state), ..c
            })),
            Tree::LetE(_) => todo!(),
            Tree::LetP(PrimStatement { exp: None, ..}) => TreeContainer::make(root),
            Tree::LetP(PrimStatement { exp: Some(Op::T(ExprTree { ref op, ref args })), name, ..})
                if matches!(op, New | InvokeVirtual | InvokeStatic) => {
                let idx = if matches!(op, New) { 0 } else { 1 };
                let sym = match args[idx] { Op::V(sym) => sym, _ => panic!("Invalid Call") };
                if let Some(f) = state.f_env.get(&sym).cloned() {
                    let cargs = args.iter().map(|s| match s {
                        Op::V(sym) => state.subst(*sym),
                        _ => panic!("Function should only have symbolic arguments!")
                    }).collect();
                    let nargs = f.args.clone();
                    let label = state.sm.refresh(&f.name);
                    let obj = if matches!(op, InvokeVirtual) {
                        Some(match args[1] { Op::V(sym) => sym, _ => panic!("Invalid Call") })
                    } else {
                        None
                    };
                    let bbody = state.with_inline(label, name, obj, |s|
                        state.withoutApps(|s|
                            s.withSubst(cargs, nargs, |s|
                                traverselist(f.body.clone(), s)
                            )
                        )
                    );
                    // We wrap things in a block to handle multiple return statements.
                    TreeContainer::make(Tree::Block(BlockStatement { label, bbody }))
                } else {
                    TreeContainer::make(root)
                }
            },
            Tree::LetP(PrimStatement { exp: Some(op), name, typ }) => TreeContainer::make(
                Tree::LetP(PrimStatement { name, typ, exp: Some(substop(op, state)) })
            ),
            Tree::Block(b) => {
                if state.applied_once(b.label) { state.addc(b.label) }
                let bbody = traverselist(b.bbody, state);
                if state.dead(b.label) { return bbody }
                TreeContainer::make(Tree::Block(BlockStatement {
                    bbody, label: b.label
                }))
            },
            Tree::Switch(s) => {
                if state.applied_once(s.label) { state.addc(s.label) }
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
            },
            Tree::Loop(LoopStatement { cond, label, lbody, dowhile }) => {
                if state.applied_once(label) { state.addc(label) }
                let nbody = traverselist(lbody, state);
                TreeContainer::make(Tree::Loop(LoopStatement {
                    cond: substop(cond, state), label,
                    lbody: nbody, dowhile
                }))
            },
            Tree::If(IfStatement { cond, label, btrue, bfalse }) => {
                if state.applied_once(label) { state.addc(label) }
                let ntrue = traverselist(btrue, state);
                let nfalse = traverselist(bfalse, state);
                TreeContainer::make(Tree::If(IfStatement {
                    cond: substop(cond, state), label,
                    btrue: ntrue, bfalse: nfalse
                }))
            },
            Tree::Break(label) => {
                if let Some(cont) = state.l_env.get(&label).cloned() {
                    traverselist(cont.clone(), state)
                } else {
                    TreeContainer::make(Tree::Break(label))
                }
            },
            Tree::Try(_) => todo!(),
            // We wrap functions in a block and break from the block.
            // This is necessary to handle arbitrary control flow.
            Tree::Return(ReturnStatement { val: Some(e) }) if state.inline.is_some() => {
                let inline = state.inline.unwrap();
                let mut res = TreeContainer::new();
                res.push_back(Tree::LetP(PrimStatement {
                    name: state.sm.fresh("t"),
                    typ: Typ::Void,
                    exp: Some(Op::T(ExprTree { op: Set, args: vec![Op::V(inline.name), e] }))
                }));
                res.push_back(Tree::Break(inline.label));
                res
            },
            Tree::Return(_) => TreeContainer::make(root),
            Tree::Continue(_) => TreeContainer::make(root),
            Tree::EntryPoint(_) => TreeContainer::make(root)
        }
    }

    fn substop(op: Operand, state: &mut State) -> Operand {
        use ArrayExpression as A;
        match op {
            Operand::This(c) => {
                if let Some(sym) = state.inline.as_ref().and_then(|obj| obj.object) {
                    return Operand::V(sym)
                }
                Operand::This(c)
            },
            // Literally wrong, but we don't really support super atm.
            Operand::Super(s) => Operand::Super(s),
            Operand::V(sym) => Operand::V(state.subst(sym)),
            Operand::T(ExprTree { op, args }) => Operand::T(ExprTree {
                op, args: args.into_iter().map(|a| substop(a, state)).collect()
            }),
            Operand::A(A::Empty(a)) => Operand::A(A::Empty(Box::new(
                a.into_iter().map(|a| substop(a, state)).collect()
            ))),
            Operand::A(A::Initializer(a)) => Operand::A(A::Initializer(
                Box::new(substarray(*a, state))
            )),
            other => other
        }
    }

    fn substarray(a: ArrayInitializer, state: &mut State) -> ArrayInitializer {
        use ElementInitializer as E;
        a.into_iter().map(|e| Box::new(match *e {
            E::Expr(op) => E::Expr(substop(op, state)),
            E::ArrayInitializer(c) => E::ArrayInitializer(substarray(c, state))
        })).collect()
    }
}

pub mod inline {
    use std::collections::HashMap;
    use crate::ir::*;
    use crate::symbolmaker::Symbol;
    pub struct State {
        map: HashMap<Symbol, usize>
    }
    impl State {
        pub fn new() -> Self {
            Self { map: HashMap::new() }
        }
        pub fn inc(&mut self, sym: Symbol) {
            if let Some(s) = self.map.get_mut(&sym) {
                *s = *s + 1;
            } else {
                self.map.insert(sym, 1);
            }
        }
    }
    pub fn statement() {

    }

    pub fn operand() {

    }
}

pub mod census {
    use std::collections::HashMap;
    use crate::ir::*;
    use crate::symbolmaker::Symbol;
    pub struct Census {
        map: HashMap<Symbol, usize>
    }
    impl Census {
        pub fn new() -> Self {
            Self { map: HashMap::new() }
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
            Tree::Switch(SwitchStatement { arg, label, cases, default }) => {
                for (ops, code) in cases { code.iter().for_each(|t| traverse(t, state)) }
                for d in default { traverse(d, state) }
            },
            Tree::Loop(LoopStatement { cond, label, lbody, dowhile }) => {
                for l in lbody { traverse(l, state) };
            },
            Tree::If(IfStatement { cond, label, btrue, bfalse }) => {
                for b in btrue { traverse(b, state) }
                for b in bfalse { traverse(b, state) }
            }
            Tree::Try(_) => todo!(),
            Tree::Return(r) => r.val.iter().for_each(|v| operand(&v, state)),
            Tree::Continue(label) => state.inc(*label),
            Tree::Break(label) => state.inc(*label),
            Tree::EntryPoint(sym) => state.inc(*sym)
        }
    }

    fn operand(op: &Operand, state: &mut Census) {
        use Operation::*;
        match op {
            Operand::This(_) => (),
            Operand::Super(_) => (),
            Operand::C(_) => (),
            Operand::V(sym) => (), /* until we have SSA */
            Operand::T(ExprTree { op: New, args }) => match args[0] {
                Operand::V(sym) => state.inc(sym),
                _ => assert!(false, "Invalid Call operation!")
            }
            Operand::T(ExprTree { op: InvokeVirtual, args }) => match args.as_slice() {
                [_, Operand::V(sym), rest @ ..] => state.inc(*sym),
                _ => assert!(false, "Invalid Call operation!"),
            },
            Operand::T(ExprTree { op: InvokeStatic, args }) => match args.as_slice() {
                [_, Operand::V(sym), rest @ ..] => state.inc(*sym),
                _ => assert!(false, "Invalid Call operation!"),
            }
            other => panic!("Invalid Operand: {:?}", other)
        }
    }
}

// Computes size as measured by space complexity.
// Hence, an array initializer takes up a huge amount of space,
// A function call takes up very little, and so on.
pub mod size {
    use std::collections::HashMap;
    use crate::ir::*;
    use crate::symbolmaker::Symbol;
    struct State {
        map: HashMap<Symbol, usize>,
        switch: bool
    }

    impl State {
        pub fn new() -> Self {
            Self { map: HashMap::new(), switch: false }
        }
        pub fn add(&mut self, sym: Symbol, size: usize) {
            if !self.switch { self.add(sym, size) }
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

    pub fn sum(list: &TreeContainer, state: &mut State) -> usize {
        list.iter().rfold(0, |acc, b| traverse(b, acc, state))
    }

    #[allow(unused)]
    fn traverse(root: &Tree, bsize: usize, state: &mut State) -> usize {
        bsize + match root {
            Tree::Program(stmts) => sum(stmts, state),
            Tree::LetI(ImportDeclaration { path }) => 0,
            Tree::LetF(f) => sum(&f.body, state),
            Tree::LetC(c) => sum(&c.methods, state),
            Tree::LetE(_) => todo!(), 
            Tree::LetP(p) => p.exp.as_ref().map(|e| operand(&e)).unwrap_or(0), 
            Tree::Block(b) => {
                state.add(b.label, bsize);
                sum(&b.bbody, state)
            },
            Tree::Switch(s) => {
                state.add(s.label, bsize);
                state.switch(|sw| {
                    let cscore: usize = s.cases.iter().map(|(op, code)| sum(code, sw)).sum();
                    let dscore = sum(&s.default, sw);
                    cscore + dscore
                })
            },
            Tree::Loop(l) => {
                state.add(l.label, bsize);
                sum(&l.lbody, state) + operand(&l.cond)
            },
            Tree::If(i) => {
                state.add(i.label, bsize);
                sum(&i.btrue, state) + sum(&i.bfalse, state)
            }
            Tree::Try(_) => todo!(),
            Tree::Return(r) => 1 + r.val.as_ref().map(|e| operand(&e)).unwrap_or(0), 
            Tree::Continue(label) => 1,
            Tree::Break(label) => 1,
            Tree::EntryPoint(sym) => 0
        }
    }

    fn array_size(a: &ArrayInitializer) -> usize{
        use ElementInitializer as E;
        a.iter().fold(0, |acc, item| match item.as_ref() {
            E::Expr(_) => 1,
            E::ArrayInitializer(child) => array_size(child)
        })
    }

    fn operand(op: &Operand) -> usize {
        use Operation::*;
        use Operand as O;
        use ArrayExpression as A;
        match op {
            O::This(_) => 1,
            O::Super(_) => 1,
            O::C(_) => 1,
            O::V(sym) => 1, /* until we have SSA */
            O::A(array) => match array {
                A::Empty(bv) => bv.len(),
                A::Initializer(a) => array_size(a.as_ref())
            }
            O::T(ExprTree { op: ArrayNew, args }) => match args.as_slice() {
                [O::Tp(Typ::Array(ArrayTyp { eltype, dims })), a] => operand(a),
                _ => panic!("Invalid Array Expression!")
            }
            O::T(ExprTree { op: New, args }) => args.len(),
            O::T(ExprTree { op: InvokeVirtual, args }) => args.len(),
            O::T(ExprTree { op: InvokeStatic, args }) => args.len(),
            O::T(ExprTree { op, args }) => args.len(),
            O::Tp(_) => 0
        }
    }
}