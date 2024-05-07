use std::collections::LinkedList;

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

#[derive(Copy, Clone)]
pub struct Symbol { id: usize }
pub struct SymbolMaker {
    count: u32,
    names: Vec<String>
}
impl SymbolMaker {
    pub fn new() -> Self {
        Self { count: 0, names: Vec::new() }
    }

    pub fn fresh(&mut self, name: String) -> Symbol {
        self.names.push(name);
        Symbol { id: self.names.len() }
    }

    pub fn name(&self, sym: Symbol) -> &str {
        &self.names[sym.id as usize]
    }

    pub fn uname(&self, sym: Symbol) -> String {
        format!("{}{}", self.names[sym.id], sym.id)
    }
}

pub enum Operation {
    // Arithmetic
    Add,
    Sub,
    Negate,
    Mul,
    Div,
    Mod,
    Inc,
    Dec,

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
    Assert
}

/*
 * The purpose of this IR is to be
 * Easy to manipulate,
 * Easy to convert back into Java,
 * Easy to unroll (high-level structures are kept).
 * 
 * Continue, Break, and Import are all considered Primitives.
 */

type TreeRef = Box<Tree>;
pub enum Tree {
    Jump,
    LetI(ImportStatement),
    LetP(PrimStatement),
    LetF(FunDeclaration),
    LetC(ClassDeclaration),
    LetE(EnumDeclaration),
    Switch(SwitchStatement),
    Loop(LoopStatement),
    AppF(AppFStatement),
    If(IfStatement),
    Try(TryStatement),
    Return(ReturnStatement),
    EntryPoint(Symbol)
}

pub struct ImportStatement {
    pub path: String,
    pub body: TreeRef
}

pub enum Operand {
    C(Literal),
    V(Symbol),
    T(Box<ExprTree>)
}

pub struct ExprTree {
    pub op: Operation,
    pub args: Vec<Operand>
}

pub struct PrimStatement {
    pub name: Symbol,
    pub op: Operation,
    pub args: Vec<Operand>,
    pub typ: Typ,
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
    pub extends: Vec<Symbol>,
    pub body: TreeRef
}

// We don't have support for more complicated enums, yet
pub struct EnumDeclaration {
    pub name: Symbol,
    pub members: Vec<Symbol>,
    pub values: Option<Vec<Literal>>,
    pub body: TreeRef
}

pub struct SwitchStatement {
    pub arg: Operand,
    pub cases: Vec<Literal>,
    pub branches: Vec<TreeRef>,
    pub default: TreeRef,
    pub body: TreeRef
}

// All other loops will be translated into this.
pub struct LoopStatement {
    pub cond: Operand,
    pub lbody: Option<TreeRef>,
    pub body: TreeRef
}

pub struct AppFStatement {
    pub fname: Symbol,
    pub args: Vec<Operand>,
    pub body: TreeRef
}

pub struct IfStatement {
    pub cond: Operand,
    pub btrue: TreeRef,
    pub bfalse: Option<TreeRef>,
    pub body: TreeRef
}

pub struct TryStatement {
    pub main: TreeRef,
    // The class symbol and the arg symbol.
    pub exceptions: Vec<(Symbol, Symbol)>,
    pub catches: Vec<TreeRef>,
    pub finally: TreeRef,
    pub body: TreeRef
}

pub struct ReturnStatement {
    pub val: Option<Operand>
}