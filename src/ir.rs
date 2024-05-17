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
    Class(Symbol)
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayTyp {
    pub eltype: Box<Typ>,
    pub dims: u8
}

#[derive(Clone, Debug)]
pub enum ArrayExpression {
    Empty(Box<ArrayEmpty>),
    Initializer(Box<ArrayInitializer>)
}

#[derive(Clone, Debug)]
pub struct ArrayEmpty {
    pub tp: Typ,
    pub ops: Vec<Operand>,
    pub dims: usize
}

#[derive(Clone, Debug)]
pub enum ElementInitializer {
    Expr(Operand),
    ArrayInitializer(ArrayInitializer)
}

#[derive(Clone, Debug)]
pub struct ArrayInitializer {
    pub tp: Typ,
    pub ops: Vec<Box<ElementInitializer>>,
    pub dims: u32
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
    ArrayNew,
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

pub type TreeContainer = LinkedList<Tree>;
pub enum Tree {
    Program(TreeContainer),
    LetI(ImportDeclaration),
    LetF(FunDeclaration),
    LetC(ClassDeclaration),
    LetE(EnumDeclaration),
    LetP(PrimStatement),
    Block(BlockStatement),
    Switch(SwitchStatement),
    Loop(LoopStatement),
    If(IfStatement),
    Try(TryStatement),
    Return(ReturnStatement),
    Break(Symbol),
    Continue(Symbol),
    EntryPoint(Symbol)
}

pub struct BlockStatement {
    pub label: Symbol,
    pub bbody: TreeContainer
}

pub struct ImportDeclaration {
    pub path: String
}

#[derive(Clone, Debug)]
pub enum Operand {
    This,
    Super,
    C(Literal),
    V(Symbol),
    T(ExprTree),
    A(ArrayExpression)
}

#[derive(Clone, Debug)]
pub struct ExprTree {
    pub op: Operation,
    pub args: Vec<Operand>
}

pub struct PrimStatement {
    pub name: Symbol,
    pub typ: Typ,
    pub exp: Option<Operand>
}

pub struct FunDeclaration {
    pub name: Symbol,
    pub args: Vec<(Symbol, Typ)>,
    pub modifiers: Vec<String>,
    pub throws: Vec<String>,
    pub return_typ: Option<Typ>,
    pub body: TreeContainer
}

pub struct ClassDeclaration {
    pub name: Symbol,
    pub members: Vec<(Symbol, Typ)>,
    pub methods: TreeContainer,
    pub extends: Option<Symbol>
    // I never use nested classes, and it's relatively easy to avoid them.
    // If this gets open-sourced, this is something I could work on.
    // pub classes: LinkedList<TreeRef>,
}

// We don't have support for more complicated enums, yet
pub struct EnumDeclaration {
    pub name: Symbol,
    pub members: Vec<Symbol>,
    pub values: Option<Vec<Literal>>
}

pub struct SwitchStatement {
    pub arg: Operand,
    pub label: Symbol,
    pub cases: Vec<(Vec<Operand>, TreeContainer)>,
    pub default: TreeContainer
}

// TODO: Special flag for do-while to avoid lots of edge cases.
// All other loops will be translated into this.
pub struct LoopStatement {
    pub cond: Operand,
    pub label: Symbol,
    pub lbody: TreeContainer,
    pub dowhile: bool
}

pub struct IfStatement {
    pub cond: Operand,
    pub label: Symbol,
    pub btrue: TreeContainer,
    pub bfalse: TreeContainer
}

pub struct TryStatement {
    pub main: TreeContainer,
    pub label: Symbol,
    // The class symbol and the arg symbol.
    pub exceptions: Vec<(Symbol, Symbol)>,
    pub catches: Vec<TreeContainer>,
    pub finally: TreeContainer
}

pub struct ReturnStatement {
    pub val: Option<Operand>
}