use std::{collections::LinkedList, rc::Rc};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Typ {
    Void,
    Bool,
    Char,
    Byte,
    Int,
    Short,
    Long,
    Float,
    Double,
    Str,
    // If we don't have access to the class,
    // We will invent a symbol for it, so this is always valid.
    Class(Symbol)
}

#[derive(Clone, Debug)]
pub enum Literal {
    Null,
    Bool(bool),
    Char(char),
    Byte(u8),
    Int(u32),
    Short(u16),
    Long(u64),
    Float(f32),
    Double(f64),
    String(String)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Symbol { id: usize }
pub struct SymbolMaker {
    names: Vec<String>
}
impl SymbolMaker {
    pub fn new() -> Self {
        Self { names: Vec::new() }
    }

    pub fn fresh(&mut self, name: &str) -> Symbol {
        self.names.push(name.to_string());
        Symbol { id: self.names.len() - 1 }
    }

    pub fn name(&self, sym: Symbol) -> &str {
        &self.names[sym.id as usize]
    }

    pub fn uname(&self, sym: Symbol) -> String {
        format!("{}{}", self.names[sym.id], sym.id)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Operation {
    // Arithmetic
    Add,
    Sub,
    Negate,
    Mul,
    Div,
    Mod,
    PreInc,
    PreDec,
    PostInc,
    PostDec,

    // Modifying
    Set,
    PSet,
    SSet,
    MSet,
    DSet,
    ModSet,
    AndSet,
    OrSet,
    XorSet,
    ShrSet,
    UshrSet,
    ShlSet,

    // Conditionals
    Eq,
    Neq,
    G,
    L,
    GEq,
    LEq,
    LAnd,
    LOr,
    LNot,

    // Bitwise
    Not,
    Shl,
    Shr,
    UShr,
    And,
    Or,
    Xor,

    // Oddballs
    InstanceOf,
    Ternary,

    // Custom
    Phi,
    Continue,
    Break,
    Assert,
    Access,
    Index
}

/*
 * The purpose of this IR is to be
 * Easy to manipulate,
 * Easy to convert back into Java,
 * Easy to unroll (high-level structures are kept).
 * 
 * Continue, Break, and Import are all considered Primitives.
 */


// Note: there's no need for classes and imports to have body statements.
// Similar to functions, we can just have a top level list of these things in Program.
type TreeRef = Box<Tree>;
pub enum Tree {
    LetI(ImportDeclaration),
    LetF(FunDeclaration),
    LetC(ClassDeclaration),
    LetE(EnumDeclaration),
    LetCont(ContDeclaration),
    LetP(PrimStatement),
    Switch(SwitchStatement),
    Loop(LoopStatement),
    If(IfStatement),
    Try(TryStatement),
    Return(ReturnStatement),
    Continue(Symbol),
    Break(Symbol),
    EntryPoint(Symbol),
    Terminal /* bandaid fix for switches */
}

pub struct ImportDeclaration {
    pub path: String,
    pub body: TreeRef
}

#[derive(Clone, Debug)]
pub enum Operand {
    This,
    C(Literal),
    V(Symbol),
    T(ExprTree)
}

#[derive(Clone, Debug)]
pub struct ExprTree {
    pub op: Operation,
    pub args: Vec<Operand>
}

pub struct PrimStatement {
    pub name: Symbol,
    pub typ: Typ,
    pub label: Option<Symbol>,
    pub exp: Option<Operand>,
    pub body: TreeRef
}

pub struct FunDeclaration {
    pub name: Symbol,
    pub args: Vec<(Symbol, Typ)>,
    pub modifiers: Vec<String>,
    pub throws: Vec<String>,
    pub return_typ: Typ,
    pub body: Option<TreeRef>
}

pub struct ClassDeclaration {
    pub name: Symbol,
    pub members: Vec<(Symbol, Typ)>,
    pub methods: LinkedList<TreeRef>,
    pub extends: Option<Symbol>,
    pub body: TreeRef
}

// We don't have support for more complicated enums, yet
pub struct EnumDeclaration {
    pub name: Symbol,
    pub members: Vec<Symbol>,
    pub values: Option<Vec<Literal>>,
    pub body: TreeRef
}

pub struct ContDeclaration {
    pub name: Symbol,
    pub cbody: TreeRef,
    pub body: TreeRef
}

pub struct SwitchStatement {
    pub arg: Operand,
    pub label: Symbol,
    pub cases: Vec<(Vec<Operand>, Box<Tree>)>,
    pub default: Option<Box<Tree>>,
    pub body: TreeRef
}

// All other loops will be translated into this.
pub struct LoopStatement {
    pub cond: Operand,
    pub label: Symbol,
    pub lbody: Option<TreeRef>,
    pub body: TreeRef
}

pub struct IfStatement {
    pub cond: Operand,
    pub label: Symbol,
    pub btrue: TreeRef,
    pub bfalse: Option<TreeRef>,
    pub body: TreeRef
}

pub struct TryStatement {
    pub main: TreeRef,
    pub label: Symbol,
    // The class symbol and the arg symbol.
    pub exceptions: Vec<(Symbol, Symbol)>,
    pub catches: Vec<TreeRef>,
    pub finally: TreeRef,
    pub body: TreeRef
}

pub struct ReturnStatement {
    pub val: Option<Operand>
}