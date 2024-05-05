use std::collections::LinkedList;

enum Typ {
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

enum Literal {
    Bool(bool),
    Char(char),
    Byte(u8),
    Int(u32),
    Short(u16),
    Long(u64),
    Float(f32),
    Double(f64),
    String(String),
    Import(String)
}

struct Symbol { id: u32 }
struct SymbolMaker {
    count: u32,
    names: Vec<String>
}
impl SymbolMaker {
    fn fresh(name: String) -> Symbol {
        names.push(name);
        Symbol { names.size() }
    }

    fn name(sym: Symbol) -> String {
        names[sym.id]
    }

    fn uname(sym: Symbol) -> String {
        format!("{}{}", names[sym.id], sym.id)
    }
}

enum Operation {
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
    Xor,

    // Oddballs
    InstanceOf,
    Ternary
}

/*
 * The purpose of this IR is to be
 * Easy to manipulate,
 * Easy to convert back into Java,
 * Easy to unroll (high-level structures are kept).
 * 
 * Continue, Break, and Import are all considered Primitives.
 * PHI is just another Operation.
 */

type TreeRef = Box<Tree>;
enum Tree {
    LetL(LitStatement),
    LetP(PrimStatement),
    LetF(FunDeclaration),
    LetC(ClassDeclaration),
    LetE(EnumDeclaration),
    Switch(SwitchStatement),
    Loop(LoopStatement),
    AppF(AppFStatement),
    If(IfStatement),
    Try(TryStatement)
}

struct LitStatement {
    name: Symbol,
    val: Literal,
    typ: Typ,
    body: Option<TreeRef>
}

struct PrimStatement {
    name: Symbol,
    op: Operation,
    args: Vector<Symbol>,
    typ: Typ,
    body: Option<TreeRef>
}

struct FunDeclaration {
    args: Vec<Symbol>,
    types: Vec<Typ>,
    throws: Vec<String>,
    body: Option<TreeRef>
}

struct ClassDeclaration {
    members: Vec<Symbol>,
    methods: LinkedList<TreeRef>,
    body: Option<TreeRef>
}

struct EnumDeclaration {
    members: Vec<Symbol>,
    values: Vec<Literal>,
    body: Option<TreeRef>
}

struct SwitchStatement {
    arg: Symbol,
    cases: Vec<Literal>,
    branches: Vec<TreeRef>,
    body: Option<TreeRef>
}

// All other loops will be translated into this.
struct LoopStatement {
    cond: Operation,
    args: Vec<Symbol>,
    lbody: Option<TreeRef>,
    body: Option<TreeRef>
}

// Functions expect to only receive symbols.
struct AppFStatement {
    fname: Symbol,
    args: Vec<Symbol>,
    body: Option<TreeRef>
}

struct IfStatement {
    cond: Operation,
    args: Vec<Symbol>,
    btrue: TreeRef,
    bfalse: Option<TreeRef>,
    body: Option<TreeRef>
}

struct TryStatement {
    main: TreeRef,
    // The class symbol and the arg symbol.
    exceptions: Vec<(Symbol, Symbol)>,
    catches: Vec<TreeRef>,
    finally: TreeRef,
    body: Option<TreeRef>
}