use std::collections::LinkedList;
use crate::symbolmaker::Symbol;

#[derive(Clone, Debug, PartialEq)]
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
    Array(ArrayTyp),
    // If we don't have access to the class,
    // We will invent a symbol for it, so this is always valid.
    Class(Symbol)
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayTyp {
    pub eltype: Box<Typ>,
    pub len: Option<u32>,
    pub dims: u8
}

#[derive(Clone, Debug)]
pub enum Literal {
    Null,
    Bool(bool),
    Char(char),
    Byte(i8),
    Int(i32),
    Short(i16),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String)
}

#[derive(Copy, Clone, Debug)]
pub enum Operation {
    // Arithmetic
    Add,
    Sub,
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
    New,
    Call,
    Phi,
    Assert,
    Access,
    Index,
    Throw
}

/*
 * The purpose of this IR is to be
 * Easy to manipulate,
 * Easy to convert back into Java,
 * Easy to unroll (high-level structures are kept).
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
    Block(BlockStatement),
    Switch(SwitchStatement),
    Loop(LoopStatement),
    If(IfStatement),
    Try(TryStatement),
    Return(ReturnStatement),
    Continue(Symbol),
    Break(Symbol),
    EntryPoint(Symbol),
    Terminal
}

pub struct BlockStatement {
    pub label: Symbol,
    pub bbody: Option<TreeRef>,
    pub body: TreeRef
}

pub struct ImportDeclaration {
    pub path: String,
    pub body: TreeRef
}

#[derive(Clone, Debug)]
pub enum Operand {
    This,
    Super,
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
    pub return_typ: Option<Typ>,
    pub body: Option<TreeRef>
}

pub struct ClassDeclaration {
    pub name: Symbol,
    pub members: Vec<(Symbol, Typ)>,
    pub methods: LinkedList<TreeRef>,
    pub extends: Option<Symbol>,
    pub body: TreeRef,
    // I never use nested classes, and it's relatively easy to avoid them.
    // If this gets open-sourced, this is something I could work on.
    // pub classes: LinkedList<TreeRef>,
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
    pub body: TreeRef
}

pub struct SwitchStatement {
    pub arg: Operand,
    pub label: Symbol,
    pub cases: Vec<(Vec<Operand>, Box<Tree>)>,
    pub default: Option<Box<Tree>>,
    pub body: TreeRef
}

// TODO: Special flag for do-while to avoid lots of edge cases.
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