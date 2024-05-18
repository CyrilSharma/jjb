// https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
// We parse a subset of the above grammar.

use std::collections::HashMap;
use crate::directory::Directory;
use crate::ir::*;
use crate::symbolmaker::{Symbol, SymbolMaker};
use crate::tsretriever::TsRetriever;
use crate::scope::Scope;
use tree_sitter::Node;

struct State<'l> {
    class_sym: Option<Symbol>,
    var_scope: Scope<'l>,
    class_scope: Scope<'l>,
    directory: Directory<'l>,
    type_map: HashMap<Symbol, Typ>,
    tsret: TsRetriever<'l>,
    label_stk: Vec<(&'l str, Symbol)>,
    break_label: Option<Symbol>,
    continue_label: Option<Symbol>,
    label: Option<Symbol>,
    sm: &'l mut SymbolMaker
}

impl<'l> State<'l> {
    pub fn new(source: &'l [u8], sm: &'l mut SymbolMaker) -> Self {
        Self {
            class_sym: None,
            var_scope: Scope::new(),
            class_scope: Scope::new(),
            directory: Directory::new(),
            type_map: HashMap::new(),
            tsret: TsRetriever::new(source),
            label_stk: Vec::new(),
            break_label: None,
            continue_label: None,
            label: None,
            sm
        }
    }

    pub fn push_continue_label(&mut self, label: Symbol) -> Option<Symbol> {
        let res = self.continue_label;
        self.continue_label = Some(label);
        res
    }   

    pub fn restore_continue_label(&mut self, label: Option<Symbol>) {
        self.continue_label = label;
    }

    pub fn push_break_label(&mut self, label: Symbol) -> Option<Symbol> {
        let res = self.break_label;
        self.break_label = Some(label);
        res
    }  

    pub fn restore_break_label(&mut self, label: Option<Symbol>) {
        self.break_label = label;
    }

    pub fn pop_label(&mut self) -> Option<Symbol> {
        let res = self.label;
        self.label = None;
        res
    }

    pub fn find_label(&self, name: &'l str) -> Option<Symbol> {
        for i in (0..self.label_stk.len()).rev() {
            let (cname, csym) = self.label_stk[i];
            if cname == name { return Some(csym) }
        }
        return None
    }

    pub fn get_typ(&self, node: &Node) -> Typ {
        let typ_node = self.tsret.get_field(node, "type");
        self.get_typ_raw(&typ_node)
    }

    pub fn get_typ_raw(&self, typ_node: &Node) -> Typ {
        match typ_node.kind() {
            "void_type" => Typ::Void,
            "integral_type" => match self.tsret.get_text(&typ_node) {
                "byte" => Typ::Byte,
                "short" => Typ::Short,
                "int" => Typ::Int,
                "long" => Typ::Long,
                "char" => Typ::Char,
                other => panic!("integral_type: {}", other)
            }
            "boolean_type" => Typ::Bool,
            "floating_point_type" => match self.tsret.get_text(&typ_node) {
                "float" => Typ::Float,
                "double" => Typ::Double,
                other => panic!("floating_point_type: {}", other)
            }

            "array_type" => {
                let eltyp = self.get_typ_raw(&self.tsret.get_field(&typ_node, "element"));
                let dims = self.tsret.get_dims(
                    &self.tsret.get_field(&typ_node, "dimensions")
                );
                Typ::Array(ArrayTyp { eltype: Box::new(eltyp), dims })
            },

            "type_identifier" => {
                let name = self.tsret.get_text(&typ_node);
                if name == "String" { return Typ::Str }
                Typ::Class(self.class_scope
                    .find(name)
                    .expect(&format!("Unknown Class Type {}", name))
                )
            },
            other => panic!("Unknown Class Type {}", other)
        }
    }

    pub fn scope_in(&mut self) {
        self.var_scope.scope_in();
        self.class_scope.scope_in();
    }

    pub fn scope_out(&mut self) {
        self.var_scope.scope_out();
        self.class_scope.scope_out();
    }
}

pub fn convert(root: Node, source: &[u8], sm: &mut SymbolMaker) -> Box<Tree> {
    assert!(root.kind() == "program");
    let mut state = State::new(source, sm);
    let entry_sym = add_declarations(root, &mut state, "main");
    let mut res = statements(root.child(0).expect("Empty Program!"), &mut state);
    res.push_back(Tree::EntryPoint(entry_sym));
    Box::new(Tree::Program(res))
}

// TODO: support for enums!
fn add_declarations<'l>(root: Node, state: &mut State, entry_name: &'l str) -> Symbol {
    let mut entry_sym = None;
    let mut cursor = root.walk();
    for top in root.named_children(&mut cursor) {
        if top.kind() != "class_declaration" { continue }
        let cname = state.tsret.get_field_text(&top, "name");
        let csym = state.sm.fresh_reserved(cname);
        state.class_scope.insert(cname, csym);

        let mut methods = HashMap::new();
        let mut members = HashMap::new();
        let body = state.tsret.get_field(&top, "body");
        let mut bcursor = body.walk();
        for child in body.named_children(&mut bcursor) { 
            match child.kind() {
                "class_declatation" => panic!("Inner classes are not supported!"),
                "enum_declaration" => panic!("Inner enums are not supported!"),
                "method_declaration" | "constructor_declaration" => {
                    let fname = state.tsret.get_field_text(&child, "name");
                    let fsym = state.sm.fresh_reserved(fname);
                    if fname == entry_name { entry_sym = Some(fsym) }
                    let type_child = child.child_by_field_name("type");
                    let rtyp = type_child.map_or_else(
                        || Typ::Class(csym),
                        |x| state.get_typ_raw(&x)
                    );
                    state.type_map.insert(fsym, rtyp);
                    methods.insert(fname, fsym).map(
                        |_| panic!("method {} appears more then once", fname)
                    );
                }
                "field_declaration" => {
                    let typ = state.get_typ(&child);
                    let declarator = state.tsret.get_field(&child, "declarator");
                    let name = state.tsret.get_field_text(&declarator, "name");
                    let var_sym = state.sm.fresh(name);
                    state.type_map.insert(var_sym, typ);
                    members.insert(name, var_sym).map(
                        |_| panic!("member {} appears more then once", name)
                    );
                },
                other => panic!("Parse Tree uses unknown node {}\n", other)
            }
        }
        state.directory.add_class(csym, methods, members);
    }
    entry_sym.expect("The entry symbol was not found!")
}

fn statements(node: Node, state: &mut State) -> TreeContainer {
    let mut cur = node;
    let mut res = TreeContainer::new();
    loop {
        res.append(statement(cur, state));
        if let Some(nxt) = cur.next_named_sibling() {
            cur = nxt;
            continue;
        }
        break;
    } 
    res
}

fn tail(mut container: TreeContainer, el: Tree) -> TreeContainer {
    use Tree::*;
    if let Some(t) = container.back() {
        match t {
            Return(_) | Break(_) | Continue(_) => return container,
            _ => ()
        }
    }
    container.push_back(el);
    container
}

/* ------ STATEMENTS ------ */
fn statement(node: Node, state: &mut State) -> TreeContainer {
    let empty: TreeContainer = TreeContainer::new();
    match node.kind() {
        /* ------ TOP-LEVEL DECLARATIONS -------- */
        "module_declaration" => panic!("Modules are not supported!"),
        "package_declaration" => empty /* packages are inlined */,
        "import_declaration" => import_declaration(node, state),
        "class_declaration" => class_declaration(node, state),
        "record_declaration" => panic!("Records are not supported!"),
        "interface_declaration" => empty /* interfaces are basically just type asserts */,
        "annotation_type_declaration" => panic!("Annotations are not supported!"),
        "enum_declaration" => enum_declaration(node, state),

        /* ------ Method Statements ----------- */
        "expression_statement" => expression_statement(node, state),
        "labeled_statement" => labeled_statement(node, state),
        "if_statement" => if_statement(node, state),
        "while_statement" => while_statement(node, state, false),
        "for_statement" => for_statement(node, state),
        "enhanced_for_statement" => todo!(),
        "block" => inline_block(node, state),
        ";" => empty,
        "assert_statement" => assert_statement(node, state),
        "do_statement" => while_statement(node, state, true),
        "break_statement" => break_statement(node, state),
        "continue_statement" => continue_statement(node, state),
        "return_statement" => TreeContainer::make(Tree::Return(ReturnStatement { 
            val: node.named_child(0).map(|child| expression(child, state))
        })),
        "yield_statement" => panic!("Yield is unsupported!"),
        "switch_expression" => switch_statement(node, state),
        "synchronized_statement" => panic!("Synchronized is unsupported!"),
        "local_variable_declaration" => local_variable_declaration(node, state),
        "throw_statement" => TreeContainer::make(Tree::LetP(PrimStatement { 
            name: state.sm.fresh("temp"),
            typ: Typ::Void,
            exp: Some(Operand::T(ExprTree {
                op: Operation::Throw,
                args: vec![expression(node.child(1).expect(""), state)]
            })),
        })),
        "try_statement" => todo!(),
        "try_with_resources_statement" => todo!(),
        "line_comment" | "block_comment" => empty,
        other => panic!("Unsupported statement {}!", other)
    }
}

/* ------------ DECLARATIONS ------------ */
fn import_declaration(node: Node, state: &mut State) -> TreeContainer {
    let path = state.tsret.get_text(&node.named_child(0).expect("Invalid Import"));
    node.child(1).map(|x| if x.kind() == "static" { panic!("Static imports are not yet supported.") } );
    return TreeContainer::make(Tree::LetI(ImportDeclaration { path: path.to_string() }));
}

fn class_declaration(node: Node, state: &mut State) -> TreeContainer {
    let cname = state.tsret.get_field_text(&node, "name");
    let csym = state.class_scope.find(cname).expect("Class Symbol was not inserted!");
    let superclass = node.child_by_field_name("superclass").map(|sc| {
        let pname = state.tsret.get_text(&sc.child(1).expect("Missing Super."));
        if let Some(sym) = state.class_scope.find(pname) { sym }
        else { state.sm.fresh_reserved(pname) }
    });

    // Note that we do this here, because in the first pass it's unclear whether the parent
    // Hasn't been added yet, or if it just doesn't exist in the heirarchy.
    superclass.map(|psym| state.directory.add_parent(csym, psym));
    let members = state.directory.members(csym)
        .expect("class symbol was not inserted!")
        .iter()
        .map(|x| (*x, state.type_map.get(x).expect("").clone()))
        .collect::<Vec<(Symbol, Typ)>>();
    let mut methods = TreeContainer::new();
    if let Some(body) = node.child_by_field_name("body") {
        state.scope_in();
        state.class_sym = Some(csym);
        let mut bcursor = body.walk();
        for child in body.named_children(&mut bcursor) { 
            match child.kind() {
                "method_declaration" | "constructor_declaration" => {
                    let text = state.tsret.get_field_text(&child, "name");
                    let name = *state.directory.resolve_method(csym, text).expect(
                        &format!("class {}, function {} wasn't inserted.", cname, text)
                    );
                    methods.push_back(method_declaration(child, state, name));
                },
                _ => ()
            }
        };
        state.class_sym = None;
        state.scope_out();
    }
    return TreeContainer::make(Tree::LetC(ClassDeclaration {
        name: csym,
        members,
        methods,
        extends: superclass
    }));
}

// TODO: Enums!
fn enum_declaration(node: Node, state: &mut State) -> TreeContainer {
    todo!()
}

fn method_declaration(node: Node, state: &mut State, name: Symbol) -> Tree {
    let rtyp = if node.kind() == "constructor_declaration" {
        None
    } else {
        Some(state.type_map.get(&name).expect("Funtype was not inserted!").clone())
    };
    let mut modifiers = Vec::new();
    let mods = node.named_child(0).expect("Method has 0 children.");
    // TODO: All we really care about is whether the method is static.
    if mods.kind() == "modifiers" {
        let mut cursor = mods.walk();
        for child in mods.children(&mut cursor) {
            modifiers.push(state.tsret.get_text(&child).to_string());
        }
    }

    let mut throws: Vec<String> = Vec::new();
    let child_count = node.child_count();
    if let Some(c) = node.child(child_count - 2) {
        if c.kind() == "throws" {
            let mut cursor = c.walk();
            cursor.goto_first_child();
            while cursor.goto_next_sibling() {
                throws.push(state.tsret.get_text(&cursor.node()).to_string())
            }
        }
    }

    let mut args = Vec::new();
    if let Some(params) = node.child_by_field_name("parameters") {
        let mut cursor = params.walk();
        for child in params.named_children(&mut cursor) {
            let argname = state.tsret.get_field_text(&child, "name");
            let argsym = state.sm.fresh(argname);
            args.push((argsym, state.get_typ(&child)));
            state.var_scope.insert(argname, argsym);
        }
    }    

    let body_node = state.tsret.get_field(&node, "body");
    let body = tail(
        inline_block(body_node, state),
        Tree::Return(ReturnStatement { val: None })
    );
    Tree::LetF(FunDeclaration {
        name,
        return_typ: rtyp,
        args,
        throws,
        modifiers,
        body
    })
}

fn break_statement(node: Node, state: &mut State) -> TreeContainer {
    TreeContainer::make(Tree::Break(match state.tsret.get_text(&node.child(1).expect("")) {
        ";" => state.break_label.expect("Loop was not labeled!"),
        ident => state.find_label(ident).expect("Unknown label")
    }))
}

fn continue_statement(node: Node, state: &mut State) -> TreeContainer {
    TreeContainer::make(Tree::Continue(match state.tsret.get_text(&node.child(1).expect("")) {
        ";" => state.continue_label.expect("Loop was not labeled!"),
        ident => state.find_label(ident).expect("Unknown label")
    }))
}

fn expression_statement(node: Node, state: &mut State) -> TreeContainer {
    TreeContainer::make(Tree::LetP(PrimStatement {
        name: state.sm.fresh("expr_stmt"),
        typ: Typ::Void,
        exp: Some(expression(node.child(0).expect("Expression is non-null"), state))
    }))
}

fn labeled_statement(node: Node, state: &mut State) -> TreeContainer {
    let mut ans = TreeContainer::new();
    let label_str = state.tsret.get_text(&node.child(0).expect("Label does not exist!"));
    let label_sym = state.sm.fresh(label_str);
    state.label = Some(label_sym);
    state.label_stk.push((label_str, label_sym));
    state.scope_in();
    let content = node.child(2).expect("Label should have child.");
    if content.kind() == "block" {
        let nchild_res = content.named_child(0);
        if let Some(child) = nchild_res {
            state.pop_label();
            ans.push_back(Tree::Block(BlockStatement {
                label: label_sym,
                bbody: tail(
                    statements(child, state),
                    Tree::Break(label_sym)
                )
            }));
        }
    } else {
        ans.append(statement(content, state))
    };
    state.scope_out();
    state.label_stk.pop();
    state.label = None;
    ans
}

/* ------------ CONDITIONALS --------------- */
fn switch_statement(node: Node, state: &mut State) -> TreeContainer {
    let switch_label = state.pop_label().unwrap_or(state.sm.fresh("_switch_"));
    let bstash = state.push_break_label(switch_label);
    let arg = expression(state.tsret.get_field(&node, "condition"), state);
    let mut cases: Vec<(Vec<Operand>, TreeContainer)> = Vec::new();
    let mut cur_args: Vec<Operand> = Vec::new();
    let mut default: TreeContainer = TreeContainer::new();
    let block = state.tsret.get_field(&node, "body");
    let mut cursor = block.walk();
    cursor.goto_first_child();
    while cursor.goto_next_sibling() {
        let node = cursor.node();
        match node.kind() {
            "switch_block_statement_group" => {
                let (ops, tree) = switch_block_statement_group(node, state);
                if ops.len() != 0 {
                    cur_args.extend(ops);
                    cases.push((cur_args.clone(), tree));
                    cur_args.clear(); 
                } else {
                    default = tree;
                }
            },
            "switch_rule" => panic!("switch_rule is unsupported"),
            _ => break
        }
    }
    state.restore_break_label(bstash);
    TreeContainer::make(Tree::Switch(SwitchStatement {
        arg,
        label: switch_label,
        cases: cases,
        default: default
    }))
}

fn switch_block_statement_group(node: Node, state: &mut State) -> (Vec<Operand>, TreeContainer) {
    let mut ops: Vec<Operand> = Vec::new();
    let mut cur = node.walk();
    cur.goto_first_child();
    loop {
        let child = cur.node();
        match child.kind() {
            "switch_label" => switch_label(child, state).map(|e| ops.push(e)),
            _ => break
        };
        if !cur.goto_next_sibling() { return (ops, TreeContainer::new()) }
        if !cur.goto_next_sibling() { return (ops, TreeContainer::new()) }
    }
    (ops, statements(cur.node(), state))
}

fn switch_label(node: Node, state: &mut State) -> Option<Operand> {
    let child = node.child(0).expect("Empty Switch Label!");
    match state.tsret.get_text(&child) {
        "case" => Some(expression(node.child(1).expect("Empty Case!"), state)),
        "default" => None,
        _ => panic!("Unknown switch label!")
    }
}

fn if_statement(node: Node, state: &mut State) -> TreeContainer {
    let branch_label = state.pop_label().unwrap_or(state.sm.fresh("_if_"));
    let btrue = tail(
        inline_statement(state.tsret.get_field(&node, "consequence"), state),
        Tree::Break(branch_label)
    );
    let mut bfalse = TreeContainer::new();
    node.child_by_field_name("alternative").map(|alt| bfalse = tail(
        inline_statement(alt, state),
        Tree::Break(branch_label)
    ));
    TreeContainer::make(Tree::If(IfStatement {
        cond: expression(state.tsret.get_field(&node, "condition"), state),
        btrue,
        bfalse,
        label: branch_label
    }))
}

fn while_statement(node: Node, state: &mut State, dowhile: bool) -> TreeContainer {
    let loop_label = state.pop_label().unwrap_or(state.sm.fresh("_while_"));
    let bstash = state.push_break_label(loop_label);
    let cstash = state.push_continue_label(loop_label);
    let body_node = state.tsret.get_field(&node, "body");
    let inline_body = tail(
        inline_statement(body_node, state),
        Tree::Continue(loop_label)
    );
    state.restore_break_label(bstash);
    state.restore_continue_label(cstash);
    TreeContainer::make(Tree::Loop(LoopStatement {
        cond: expression(state.tsret.get_field(&node, "condition"), state),
        lbody: inline_body,
        label: loop_label,
        dowhile
    }))
}

fn for_statement(node: Node, state: &mut State) -> TreeContainer {
    let wrap = |state: &mut State, op: Operand| {
        Tree::LetP(PrimStatement {
            name: state.sm.fresh("temp"),
            typ: Typ::Void,
            exp: Some(op)
        })
    };

    state.scope_in();
    let mut res = TreeContainer::new();
    let fchild = state.tsret.get_field(&node, "init");
    if fchild.kind() == "local_variable_declaration" {
        res.append(local_variable_declaration(fchild, state));
    } else {
        let mut cursor = node.walk();
        for child in node.children_by_field_name("init", &mut cursor) {
            let exp = expression(child, state);
            res.push_back(wrap(state, exp));
        }
    }

    let loop_label = state.pop_label().unwrap_or(state.sm.fresh("_for_"));
    let block_label = state.sm.fresh("_for_body_");
    let bstash = state.push_break_label(block_label);
    let cstash = state.push_continue_label(loop_label);
    let inline_body = tail(
        inline_statement(state.tsret.get_field(&node, "body"), state),
        Tree::Break(block_label)
    );
    let mut lbody = TreeContainer::new();
    lbody.push_back(Tree::Block(BlockStatement {
        label: block_label,
        bbody: inline_body
    }));
    state.restore_break_label(bstash);
    state.restore_continue_label(cstash);
    let mut cursor = node.walk();
    for child in node.children_by_field_name("update", &mut cursor) {
        let exp = expression(child, state);
        lbody.push_back(wrap(state, exp));
    }

    lbody = tail(lbody, Tree::Continue(loop_label));
    let cond = expression(state.tsret.get_field(&node, "condition"), state);
    res.push_back(Tree::Loop(LoopStatement { cond, label: loop_label, lbody, dowhile: false }));
    state.scope_out();
    res
}

fn assert_statement(node: Node, state: &mut State) -> TreeContainer {
    TreeContainer::make(Tree::LetP(PrimStatement {
        name: state.sm.fresh("assert"),
        typ: Typ::Void,
        exp: Some(Operand::T(ExprTree {
            op: Operation::Assert,
            args: vec![node.child(1), node.child(2)].into_iter().flatten()
            .map(|x| expression(x, state)).collect()
        }))
    }))
}

fn local_variable_declaration(node: Node, state: &mut State) -> TreeContainer {
    let tp = state.get_typ(&node);
    let mut cur = node.named_child(1).expect("Declaration has 0 declarators!");
    let mut syms = Vec::new();
    let mut exps = Vec::new();
    loop {
        let name_str = state.tsret.get_field_text(&cur, "name");
        let name_sym = state.sm.fresh(name_str);
        let value = cur.child_by_field_name("value");
        let exp = value.map(|x| expression(x, state));
        syms.push(name_sym);
        exps.push(exp);
        state.var_scope.insert(name_str, name_sym);
        let dim_res = cur.child_by_field_name("dimensions");
        let el_tp = if let Some(dim_cur) = dim_res {
            Typ::Array(ArrayTyp {
                eltype: Box::new(tp.clone()),
                dims: state.tsret.get_dims(&dim_cur)
            })
        } else {
            tp.clone()
        };
        state.type_map.insert(name_sym, el_tp);
        if let Some(nbr) = cur.next_named_sibling() {
            cur = nbr;
            continue;
        }
        break;
    };

    let mut res = TreeContainer::new();
    for (name, exp) in syms.into_iter().zip(exps.into_iter()) {
        res.push_back(Tree::LetP(PrimStatement {
            name, typ: tp.clone(), exp
        }));
    }
    res
}

fn inline_block(node: Node, state: &mut State) -> TreeContainer {
    state.scope_in();
    let mut res = TreeContainer::new();
    if let Some(child) = node.named_child(0) {
        res = statements(child, state);
    }
    state.scope_out();
    res
}

// For things like if_statements, for_statements, etc. 
// If the statement is a block, it hoists the contents, otherwise, it uses them directly.
fn inline_statement(node: Node, state: &mut State) -> TreeContainer {
    state.scope_in();
    let mut res = TreeContainer::new();
    if node.kind() == "block" {
        if let Some(child) = node.named_child(0) {
            res = statements(child, state);
        }
    } else {
        res = statement(node, state);
    };
    state.scope_out();
    res
}

fn expression(node: Node, state: &mut State) -> Operand {
    let (op, _) = type_expression(node, state);
    op
}

fn type_coerce(a: Option<Typ>, b: Option<Typ>) -> Option<Typ> {
    match (a, b) {
        (Some(at), Some(bt)) if at == bt => Some(at),
        _ => None
    }
}

fn type_expression(node: Node, state: &mut State) -> (Operand, Option<Typ>) {
    use Operand as O;
    match node.kind() {
        "assignment_expression" => binary_expression(node, state),
        "binary_expression" => binary_expression(node, state),
        "instanceof_expression" => panic!("instanceof is not supported yet!"),
        "lambda_expression" => panic!("lambdas are not supported!"),
        "ternary_expression" => {
            let (cond, cond_typ) = type_expression(state.tsret.get_field(&node, "condition"), state);
            let (cons, cons_typ) = type_expression(state.tsret.get_field(&node, "consequence"), state);
            let (alt, alt_typ) = type_expression(state.tsret.get_field(&node, "alternative"), state);
            let tree = Operand::T(ExprTree { op: Operation::Ternary, args: vec![cond, cons, alt] });
            (tree, type_coerce(cons_typ, alt_typ))
        },
        "update_expression" => {
            use Operand::*;
            use Operation::*;
            let (op, idx) = match node.child(0).expect("Update Expression Has Child").kind() {
                "++" => (PreInc, 1),
                "--" => (PreDec, 1),
                _ => match node.child(1).expect("Update Expression Has Child").kind() {
                    "++" => (PostInc, 0),
                    "--" => (PostDec, 0),
                    _ => panic!("Unknown Updated Expression!")
                }
            };
            let (exp, etyp) = type_expression(node.child(idx).expect(""), state);
            return (T(ExprTree { op, args: vec![exp] }), etyp);
        },
        "cast_expression" => panic!("casts are not supported yet!"),
        "unary_expression" => {
            let (exp, etyp) = type_expression(state.tsret.get_field(&node, "operand"), state);
            (Operand::T(ExprTree { op: state.tsret.get_op(&node), args: vec![exp] }), etyp)
        },
        "switch_expression" => panic!("Switches are not supported as expressions!"),

        //------- PRIMARY EXPRESSIONS ---------//

        // _literal
        "character_literal" => (O::C(state.tsret.get_lit(&node)), Some(Typ::Char)),
        "true" | "false" => (O::C(state.tsret.get_lit(&node)), Some(Typ::Bool)),
        "decimal_integer_literal" | "hex_integer_literal" |
        "octal_integer_literal" | "binary_integer_literal" => (O::C(state.tsret.get_lit(&node)), Some(Typ::Long)),
        "decimal_floating_point_literal" | "hex_floating_point_literal" => (O::C(state.tsret.get_lit(&node)), Some(Typ::Double)),
        "string_literal" => (O::C(state.tsret.get_lit(&node)), Some(Typ::Str)),
        "null_literal" => (O::C(state.tsret.get_lit(&node)), None),

        "class_literal" => panic!("Class Literals are not (yet) supported!"),
        "this" => (O::This, Some(Typ::Class(state.class_sym.expect("Current Class not set!")))),
        "super" => {
            let parent = state.directory.resolve_parent(state.class_sym.expect("Current Class not set!"));
            (O::Super, Some(Typ::Class(parent.expect("Invalid use of super"))))
        }
        "identifier" => {
            // Not actually sure if I should be allowing classes here...
            let iname = state.tsret.get_text(&node);
            let isym = state.var_scope.find(iname).or_else(||
                state.class_scope.find(iname)
            );
            if let Some(sym) = isym {
                (O::V(sym), state.type_map.get(&sym).cloned())   
            } else {
                let res = state.directory.resolve_field(
                    state.class_sym.expect(""), iname
                );
                if let Some(sym) = res {
                    // The symbol belongs to the current class!
                    // Replace it with this.symbol to represent the code better.
                    let tree = O::T(ExprTree {
                        op: Operation::Access,
                        args: vec![Operand::This, Operand::V(*sym)]}
                    );
                    (tree, state.type_map.get(&sym).cloned())
                } else {
                    // The symbol belongs to a super class we don't have.
                    let sym = state.sm.fresh_reserved(iname);
                    (O::V(sym), state.type_map.get(&sym).cloned())   
                }
            }
        }

        "parenthesized_expression" => type_expression(node.child(1).expect("parenthesized_expression"), state),
        "object_creation_expression" => {
            let dne = || panic!("Object child node does not exist");
            node.child(0).map(|x| x.kind() == "new").map_or_else(dne,
                |result| { if !result { panic!("Unsupported Object Creation Expression!"); }
            });
            node.child(1).map(|x| x.kind() == "@").map_or_else(dne,
                |result| { if result { panic!("Annotations are not supported!"); }
            });
            let nchild = node.child_count();
            node.child(nchild - 1).map(|x| x.kind() == "class_body").map_or_else(dne,
                |result| { if result { panic!("Inline classes are not supported!"); }
            });
            let obj = state.tsret.get_field_text(&node, "type");
            let csym = state.class_scope.find(obj).unwrap_or(
                state.sm.fresh(obj)
            );
            // We put the symbol of the actual function call here, rather then the class symbol.
            let res1 = state.directory.resolve_method(csym, obj);
            let fsym = if let Some(sym) = res1 {
                *sym
            } else {
                state.sm.fresh(obj)
            };
            let mut args = vec![O::V(fsym)];
            args.extend(parse_args(node.child_by_field_name("arguments").expect("Missing argument list."), state));
            (O::T(ExprTree { op: Operation::New, args }), Some(Typ::Class(csym)))
        },
        "field_access" => {
            let (obj, obj_typ) = type_expression(state.tsret.get_field(&node, "object"), state);
            let fname = state.tsret.get_field_text(&node, "field");
            let fsym = obj_typ.and_then(|x| {
                if let Typ::Class(csym) = x {
                    state.directory.resolve_field(csym, fname).copied()
                } else { 
                    None
                }
            }).unwrap_or_else(|| state.sm.fresh_reserved(fname));
            let tree = O::T(ExprTree { op: Operation::Access, args: vec![obj, Operand::V(fsym)] });
            (tree, state.type_map.get(&fsym).cloned())
        },
        "array_access" => {
            let (arr, arr_type) = type_expression(state.tsret.get_field(&node, "array"), state);
            let index = expression(state.tsret.get_field(&node, "index"), state);
            let rtyp = arr_type.map(|tp| match tp {
                Typ::Array(ArrayTyp { eltype, dims }) => if dims == 1 { *eltype } else {
                    Typ::Array(ArrayTyp { eltype, dims: dims - 1 })
                },
                other => panic!("Invalid array access on {:?}", other)
            });
            (O::T(ExprTree { op: Operation::Index, args: vec![arr, index] }), rtyp)
        },
        "method_invocation" => {
            node.child_by_field_name("type_arguments").map(|_| panic!("Templates not supported!"));
            // TODO: this is bad, it's a basic feature and you will need this for inlining extended classes.
            node.child(2).map(|x| if x.kind() == "super" { panic!("Super not supported in invocation (yet)!") });
            
            let (obj, obj_type) = type_expression(node.child_by_field_name("object").expect(""), state);
            let fname = state.tsret.get_field_text(&node, "name");
            let fsym = obj_type.and_then(|x| {
                if let Typ::Class(csym) = x {
                    state.directory.resolve_method(csym, fname).copied()
                } else { 
                    None
                }
            }).unwrap_or_else(|| state.sm.fresh_reserved(fname));
            let mut args = vec![obj, Operand::V(fsym)];
            args.extend(parse_args(state.tsret.get_field(&node, "arguments"), state));
            (O::T(ExprTree { op: Operation::InvokeVirtual, args }), state.type_map.get(&fsym).cloned())
        },
        "method_reference" => panic!("Method references are not supported!"),
        "array_creation_expression" => {
            let dne = || panic!("Array child node does not exist");
            node.child(1).map(|x| x.kind() == "@").map_or_else(dne,
                |result| { if result { panic!("Annotations are not supported!"); }
            });

            let mut ndims: u8 = 0;
            let mut args = Vec::new();
            let mut cursor = node.walk();
            for dim_node in node.children_by_field_name("dimensions", &mut cursor) {
                let mut cursor = dim_node.walk();
                for child in dim_node.children(&mut cursor) {
                    match child.kind() {
                        "@" => panic!("Annotations are not supported!"),
                        "[" => (),
                        "]" => ndims += 1,
                        _ => args.push(expression(child, state))
                    }
                }
            }

            println!("ndims: {}, nargs: {}", ndims, args.len());
            
            let eltype = Box::new(state.get_typ(&node));
            let tp = Typ::Array(ArrayTyp { eltype, dims: ndims });
            let value_node = node.child_by_field_name("value");
            let op = if let Some(vnode) = value_node {
                O::A(ArrayExpression::Initializer(
                    tp.clone(), Box::new(parse_array_initializer(vnode, state)
                )))
            } else {
                O::A(ArrayExpression::Empty(
                    tp.clone(), Box::new(ArrayEmpty { ops: args, dims: ndims })
                ))
            };
            return (op, Some(tp))
        },
        "template_expression" => panic!("Template expressions are not supported yet."),

        // _reserved_identifier
        "open" | "module" | "record" | "with" | "yield" | "seal" => panic!("Reserved Identifiers are not supported!"),
        other => panic!("Unknown Expression Type: {}", other)
    }
}

fn parse_array_initializer(node: Node, state: &mut State) -> ArrayInitializer {
    let mut ops = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "{" | "}" | "," => (),
            "array_initializer" => ops.push(Box::new(ElementInitializer::ArrayInitializer(
                parse_array_initializer(child, state))
            )),
            _ => ops.push(Box::new(ElementInitializer::Expr(expression(child, state))))
        }
    }
    return ArrayInitializer { ops };
}

fn parse_args(node: Node, state: &mut State) -> Vec<Operand> {
    let mut res = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        res.push(expression(child, state))
    }
    res
}

// It's ok to be a bit convervative with evaluating types.
// In the worst case, it merely inhibits us from performing some inlining.
fn binary_expression(node: Node, state: &mut State) -> (Operand, Option<Typ>) {
    let (left, ltyp) = type_expression(state.tsret.get_field(&node, "left"), state);
    let (right, rtyp) = type_expression(state.tsret.get_field(&node, "right"), state);
    let tree = ExprTree { op: state.tsret.get_op(&node), args: vec![left, right] };
    (Operand::T(tree), type_coerce(ltyp, rtyp))
}