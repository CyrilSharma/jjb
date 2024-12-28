use crate::container::Container;
use crate::symbolmanager::Symbol;
use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Typ {
    Unknown,
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
    Array(Symbol),
    Class(Symbol),
}

pub struct ArrayTyp {
    pub eltype: Typ,
    pub dims: u8,
}

// Unsure if methods should be part of the type.
pub struct ClassTyp {
    pub members: Vec<(Symbol, Typ)>,
}

pub struct EnumTyp {
    pub members: Vec<(Symbol, Typ)>,
}

impl Typ {
    pub fn intrank(&self) -> u8 {
        use Typ as T;
        match self {
            T::Byte => 1,
            T::Char => 2,
            T::Int => 3,
            T::Short => 4,
            T::Long => 5,
            _ => 0,
        }
    }
}

pub type ArrayInitializer = Vec<Box<ElementInitializer>>;

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayExpression {
    Empty(Box<Vec<Operand>>),
    Initializer(Box<ArrayInitializer>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ElementInitializer {
    Expr(Operand),
    ArrayInitializer(ArrayInitializer),
}

#[derive(Clone, Debug, PartialEq)]
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
    // You can replace this with Cow<>
    // Or, you can make another Symbol, or index
    String(String),
}

impl Literal {
    pub fn get_double(&self) -> Option<f64> {
        match self {
            Literal::Float(b) => Some(*b as f64),
            Literal::Double(i) => Some(*i as f64),
            _ => None,
        }
    }

    pub fn double_rank(&self) -> u32 {
        match self {
            Literal::Float(b) => 1,
            Literal::Double(i) => 2,
            _ => 0,
        }
    }

    pub fn get_int(&self) -> Option<i64> {
        match self {
            Literal::Byte(b) => Some(*b as i64),
            Literal::Int(i) => Some(*i as i64),
            Literal::Short(s) => Some(*s as i64),
            Literal::Long(l) => Some(*l),
            _ => None,
        }
    }

    pub fn int_rank(&self) -> u32 {
        match self {
            Literal::Byte(b) => 1,
            Literal::Int(i) => 2,
            Literal::Short(s) => 3,
            Literal::Long(l) => 4,
            _ => 0,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Null => write!(f, "null"),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Char(c) => write!(f, "{}", c),
            Literal::Byte(b) => write!(f, "{}", b),
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Short(s) => write!(f, "{}", s),
            Literal::Long(l) => write!(f, "{}", l),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::Double(d) => write!(f, "{}", d),
            Literal::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
    InvokeVirtual,
    InvokeStatic,
    Phi,
    Pcopy,
    Assert,
    Access,
    Index,
    Throw,
}

/*
 * The purpose of this IR is to be
 * Easy to manipulate,
 * Easy to convert back into Java,
 * Easy to unroll (high-level structures are kept).
 */

pub type TreeContainer = Container<Tree>;

#[derive(Clone)]
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
    EntryPoint(Symbol),
}

#[derive(Clone)]
pub struct BlockStatement {
    pub label: Symbol,
    pub bbody: TreeContainer,
}

#[derive(Clone)]
pub struct ImportDeclaration {
    pub path: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    This(Symbol),
    Super(Symbol),
    C(Literal),
    V(Symbol),
    T(ExprTree),
    A(ArrayExpression),
    Tp(Typ),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprTree {
    pub op: Operation,
    pub args: Vec<Operand>,
}

#[derive(Clone, Debug)]
pub struct PrimStatement {
    pub name: Option<Symbol>,
    pub typ: Typ,
    pub exp: Option<Operand>,
}

#[derive(Clone)]
pub struct FunDeclaration {
    pub name: Symbol,
    pub args: Vec<(Symbol, Typ)>,
    pub modifiers: Vec<String>,
    pub throws: Vec<String>,
    pub return_typ: Typ,
    pub body: TreeContainer,
    pub constructor: bool,
}

#[derive(Clone)]
pub struct ClassDeclaration {
    pub name: Symbol,
    pub members: Vec<(Symbol, Typ)>,
    pub methods: TreeContainer,
    pub extends: Option<Symbol>, // I never use nested classes, and it's relatively easy to avoid them.
                                 // If this gets open-sourced, this is something I could work on.
                                 // pub classes: LinkedList<TreeRef>,
}

// We don't have support for more complicated enums, yet
#[derive(Clone)]
pub struct EnumDeclaration {
    pub name: Symbol,
    pub members: Vec<Symbol>,
    pub values: Option<Vec<Literal>>,
}

#[derive(Clone)]
pub struct SwitchStatement {
    pub arg: Operand,
    pub label: Symbol,
    pub cases: Vec<(Vec<Operand>, TreeContainer)>,
    pub default: TreeContainer,
}

// TODO: Special flag for do-while to avoid lots of edge cases.
// All other loops will be translated into this.
#[derive(Clone)]
pub struct LoopStatement {
    pub cond: Operand,
    pub label: Symbol,
    pub lbody: TreeContainer,
    pub dowhile: bool,
}

#[derive(Clone)]
pub struct IfStatement {
    pub cond: Operand,
    pub label: Symbol,
    pub btrue: TreeContainer,
    pub bfalse: TreeContainer,
}

#[derive(Clone)]
pub struct TryStatement {
    pub main: TreeContainer,
    pub label: Symbol,
    // The class symbol and the arg symbol.
    pub exceptions: Vec<(Symbol, Symbol)>,
    pub catches: Vec<TreeContainer>,
    pub finally: TreeContainer,
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub val: Option<Operand>,
}
