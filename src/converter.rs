// https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
// We parse a subset of the above grammar.

use std::collections::LinkedList;
use crate::ir::*;
use crate::tsretriever::TsRetriever;
use crate::scope::Scope;
use tree_sitter::Node;

type TailTp<'l> = &'l dyn Fn(&mut State) -> Box<Tree>;

pub struct State<'l> {
    tsretriever: TsRetriever<'l>,
    sm: &'l mut SymbolMaker,
    scope: Scope<'l>,
    entry_name: &'l str,
    entry_sym: Option<Symbol>
}

impl<'l> State<'l> {
    pub fn new(source: &'l [u8], sm: &'l mut SymbolMaker) -> Self {
        Self {
            entry_name: "Main",
            entry_sym: None,
            scope: Scope::new(),
            tsretriever: TsRetriever::new(source),
            sm,
        }
    }
}

pub fn convert(root: Node, source: &[u8], sm: &mut SymbolMaker) -> Box<Tree> {
    assert!(root.kind() == "program");
    let mut state = State::new(source, sm);
    let res = |s: &mut State| Box::new(Tree::EntryPoint(s.sm.fresh("Main")));
    statement(root.child(0).expect("Empty Program!"), &res, &mut state)
}

fn statement(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    match node.kind() {
        /* ------ TOP-LEVEL DECLARATIONS -------- */
        "module_declaration" => panic!("Modules are not supported!"),
        "package_declaration" => next(node, tail, state), /* packages are inlined */
        "import_declaration" => import_declaration(node, tail, state),
        "class_declaration" => class_declaration(node, tail, state),
        "record_declaration" => panic!("Records are not supported!"),
        "interface_declaration" => { println!("Warning: Interfaces are ignored!"); next(node, tail, state) },
        "annotation_type_declaration" => panic!("Annotations are not supported!"),
        "enum_declaration" => enum_declaration(node, state),

        /* ------ Method Statements ----------- */
        "expression_statement" => expression_statement(node, tail, state),
        "labeled_statement" => labeled_statement(node, tail, state),
        "if_statement" => if_statement(node, tail, state),
        "while_statement" => while_statement(node, tail, state),
        "for_statement" => todo!(),
        "enhanced_for_statement" => todo!(),
        "block" => block(node, tail, state),
        ";" => next(node, tail, state),
        "assert_statement" => assert_statement(node, tail, state),
        "do_statement" => todo!(),
        "break_statement" => todo!(), // Box::new(Tree::Break()),
        "continue_statement" => todo!(), // Box::new(Tree::Continue()),
        "return_statement" => Box::new(Tree::Return(ReturnStatement { 
            val: node.named_child(0).map(|child| expression(child, state))
        })),
        "yield_statement" => panic!("Yield is unsupported!"),
        "switch_expression" => switch_statement(node, tail, state),
        "synchronized_statement" => panic!("Synchronized is unsupported!"),
        "local_variable_declaration" => local_variable_declaration(node, tail, state),
        "throw_statement" => todo!(),
        "try_statement" => todo!(),
        "try_with_resources_statement" => todo!(),
        other => panic!("Unsupported statement {}!", other)
    }
}

/*
    * Grabs the next statement, if there is no next statement, tail is inserted.
    */
pub fn next(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    if let Some(sib) = node.next_named_sibling() {
        statement(sib, tail, state)
    } else {
        tail(state)
    }
}

pub fn expression_statement(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    Box::new(Tree::LetP(PrimStatement {
        name: state.sm.fresh("expr_stmt"),
        typ: Typ::Void,
        exp: Some(expression(node.child(0).expect("Expression is non-null"), state)),
        body: next(node, tail, state),
        label: state.tsretriever.get_label(&node).map(|x| state.sm.fresh(x))
    }))
}

pub fn labeled_statement(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    let label_str = state.tsretriever.get_text(&node.child(0).expect("Label does not exist!"));
    let label_sym = state.sm.fresh(label_str);
    // TODO ADDING STUFF ONTO THIS STACK!
    let res = |s: &mut State| { next(node, tail, s) };
    statement(
        node.child(2).expect("labeled statement lacks child!"),
        &res, state
    )
}

fn if_statement(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    let branch_tail = |_: &mut State| { Box::new(Tree::Break(todo!())) };
    Box::new(Tree::If(IfStatement {
        cond: expression(state.tsretriever.get_field(&node, "condition"), state),
        btrue: statement(state.tsretriever.get_field(&node, "consequence"), &branch_tail, state),
        bfalse: node.child_by_field_name("alternative").map(|child| statement(child, &branch_tail, state)),
        label: state.sm.fresh(state.tsretriever.get_label(&node).unwrap_or("_if_")),
        body: next(node, tail, state)
    }))
}

fn while_statement(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    let loop_tail = |_: &mut State| { Box::new(Tree::Continue(todo!())) };
    Box::new(Tree::Loop(LoopStatement {
        cond: expression(state.tsretriever.get_field(&node, "condition"), state),
        lbody: Some(statement(state.tsretriever.get_field(&node, "body"), &loop_tail, state)),
        label: state.sm.fresh(state.tsretriever.get_label(&node).unwrap_or("_loop_")),
        body: next(node, tail, state)
    }))
}

fn assert_statement(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    Box::new(Tree::LetP(PrimStatement {
        name: state.sm.fresh("assert"),
        typ: Typ::Void,
        exp: Some(Operand::T(ExprTree {
            op: Operation::Assert,
            args: vec![node.child(1), node.child(2)].into_iter().flatten()
            .map(|x| expression(x, state)).collect()
        })),
        label: None,
        body: next(node, tail, state)
    }))
}

fn local_variable_declaration(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    let tp = state.tsretriever.get_typ(&node);
    let mut cur = node.named_child(1).expect("Declaration has 0 declarators!");
    let mut syms = Vec::new();
    let mut exps = Vec::new();
    loop {
        let name_str = state.tsretriever.get_field_text(&cur, "name");
        let name_sym = state.sm.fresh(name_str);
        let value = cur.child_by_field_name("value");
        let exp = value.map(|x| Operand::C(state.tsretriever.get_lit(&x)));
        syms.push(name_sym);
        exps.push(exp);
        state.scope.insert(name_str, name_sym);
        if let Some(nbr) = cur.next_named_sibling() {
            cur = nbr;
            continue;
        }
        // TODO dimensions, for when we declare arrays...
        break;
    };
    let mut res = next(node, tail, state);
    for i in (0..syms.len()).rev() {
        res = Box::new(Tree::LetP(PrimStatement {
            name: syms[i],
            label: None,
            typ: tp.clone(),
            exp: exps.remove(i),
            body: res
        }));
    }
    res
}

fn import_declaration(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    let path = state.tsretriever.get_text(&node.named_child(0).expect("Invalid Import"));
    return Box::new(Tree::LetI(ImportDeclaration {
        path: path.to_string(),
        body: next(node, tail, state)
    }));
}

fn enum_declaration(node: Node, state: &mut State) -> Box<Tree> {
    todo!()
}

fn class_declaration(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    let cname = state.tsretriever.get_field_text(&node, "name");
    let csym = state.sm.fresh(cname);

    let mut superclass = None;
    if let Some(sc) = node.child_by_field_name("superclass") {
        let pname = state.tsretriever.get_text(
            &sc.child(1).expect("Superclass should have child.")
        );
        if let Some(sym) = state.scope.find(pname) {
            superclass = Some(sym);
        } else {
            superclass = Some(state.sm.fresh(pname));
        }
    }

    let mut members = Vec::new();
    let mut methods = LinkedList::new();
    if let Some(body) = node.child_by_field_name("body") {
        state.scope.scope_in();
        let mut bcursor = body.walk();
        for child in body.named_children(&mut bcursor) {
            match child.kind() {
                "method_declaration" => methods.push_back(method_declaration(child, state)),
                "field_declaration" => members.push(parse_field(child, state)),
                other => panic!("Parse Tree uses unknown node {}\n", other)
            }
        }
        state.scope.scope_out();
    }
    
    let cname = state.tsretriever.get_field_text(&node, "name");
    return Box::new(Tree::LetC(ClassDeclaration {
        name: csym,
        members,
        methods,
        extends: superclass,
        body: next(node, tail, state)
    }));
}

fn parse_field(node: Node, state: &mut State) -> (Symbol, Typ) {
    let typ = state.tsretriever.get_typ(&node);
    let declarator = state.tsretriever.get_field(&node, "declarator");
    let name = state.tsretriever.get_field_text(&declarator, "name");
    (state.sm.fresh(name), typ)
}

fn switch_statement(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    let arg = expression(state.tsretriever.get_field(&node, "condition"), state);
    let mut cases: Vec<(Vec<Operand>, Box<Tree>)> = Vec::new();
    let mut cur_args: Vec<Operand> = Vec::new();
    let mut default: Option<Box<Tree>> = None;

    let block = state.tsretriever.get_field(&node, "body");
    let mut cursor = block.walk();
    cursor.goto_first_child();
    while cursor.goto_next_sibling() {
        let node = cursor.node();
        // The parser is a little janky in that it specifies
        // That labels can occur multiple times in a block AND
        // Blocks can occur multiple times, creating ambiguity.
        // This handles both cases, even though it usualy just does the former.

        match node.kind() {
            "switch_block_statement_group" => {
                let (ops, tree) = switch_block_statement_group(node, state);
                if ops.len() != 0 {
                    cur_args.extend(ops);
                    if let Some(t) = tree {
                        cases.push((cur_args.clone(), t));
                        cur_args.clear();
                    }; 
                } else {
                    default = Some(tree.expect("Default case doesn't exist!"))
                }
            },
            "switch_rule" => panic!("switch_rule is unsupported"),
            _ => break
        }
    }

    let temp = state.sm.fresh("OOGA");
    Box::new(Tree::Switch(SwitchStatement {
        arg,
        label: temp, // TODO
        cases: cases,
        default: default,
        body: next(node, tail, state)
    }))
}

fn switch_block_statement_group(node: Node, state: &mut State) -> (Vec<Operand>, Option<Box<Tree>>) {
    let mut ops: Vec<Operand> = Vec::new();
    let mut cur = node.walk();
    cur.goto_first_child();
    loop {
        let child = cur.node();
        match child.kind() {
            "switch_label" => switch_label(child, state).map(|e| ops.push(e)),
            _ => break
        };
        if !cur.goto_next_sibling() { return (ops, None) }
        if !cur.goto_next_sibling() { return (ops, None) }
    }
    let tail = |_: &mut State| { Box::new(Tree::Terminal) };
    (ops, Some(statement(cur.node(), &tail, state)))
}

fn switch_label(node: Node, state: &mut State) -> Option<Operand> {
    let child = node.child(0).expect("Empty Switch Label!");
    match state.tsretriever.get_text(&child) {
        "case" => Some(expression(node.child(1).expect("Empty Case!"), state)),
        "default" => None,
        _ => panic!("Unknown switch label!")
    }
}

fn method_declaration(node: Node, state: &mut State) -> Box<Tree> {
    let rtyp = state.tsretriever.get_typ(&node);
    let name = state.tsretriever.get_field_text(&node, "name");
    let name_sym = state.sm.fresh(name);

    let mut modifiers = Vec::new();
    let mods = node.named_child(0).expect("Method has 0 children.");
    if mods.kind() == "modifiers" {
        let mut cursor = mods.walk();
        for child in mods.children(&mut cursor) {
            modifiers.push(state.tsretriever.get_text(&child).to_string());
        }
    }

    let mut throws: Vec<String> = Vec::new();
    let child_count = node.child_count();
    if let Some(c) = node.child(child_count - 2) {
        if c.kind() == "throws" {
            let mut cursor = c.walk();
            cursor.goto_first_child();
            while cursor.goto_next_sibling() {
                throws.push(state.tsretriever.get_text(&cursor.node()).to_string())
            }
        }
    }

    let mut args = Vec::new();
    if let Some(params) = node.child_by_field_name("parameters") {
        let mut cursor = params.walk();
        for child in params.named_children(&mut cursor) {
            let argname = state.tsretriever.get_field_text(&child, "name");
            let argsym = state.sm.fresh(argname);
            args.push((argsym, state.tsretriever.get_typ(&node)));
            state.scope.insert(argname, argsym);
        }
    }
    
    let tail = |_: &mut State| { Box::new(Tree::Return(ReturnStatement { val: None } )) };
    let body = node.child_by_field_name("body").map(|x| block(x, &tail, state));
    Box::new(Tree::LetF(FunDeclaration {
        name: name_sym,
        return_typ: rtyp,
        args,
        throws,
        modifiers,
        body
    }))
}

pub fn block(node: Node, tail: TailTp, state: &mut State) -> Box<Tree> {
    state.scope.scope_in();
    let res = if let Some(child) = node.named_child(0) {
        let child_tail = |s: &mut State| next(node, tail, s);
        statement(child, &child_tail, state)
    } else {
        todo!()
    };
    state.scope.scope_out();
    res
}

pub fn expression(node: Node, state: &mut State) -> Operand {
    use Operand as O;
    match node.kind() {
        "assignment_expression" => binary_expression(node, state),
        "binary_expression" => binary_expression(node, state),
        "instanceof_expression" => todo!(),
        "lambda_expression" => todo!(),
        "ternary_expression" => Operand::T(ExprTree {
            op: Operation::Ternary,
            args: vec![expression(state.tsretriever.get_field(&node, "condition"), state),
                        expression(state.tsretriever.get_field(&node, "consequence"), state),
                        expression(state.tsretriever.get_field(&node, "alternative"), state)]
        }),
        "update_expression" => {
            use Operand::*;
            use Operation::*;
            match node.child(0).expect("Update Expression Has Child").kind() {
                "++" => return T(ExprTree { op: PreInc, args: vec![expression(node.child(1).expect(""), state)] }),
                "--" => return T(ExprTree { op: PreDec, args: vec![expression(node.child(1).expect(""), state)] }),
                _ => ()
            }
            match node.child(1).expect("Update Expression Has Child").kind() {
                "++" => return T(ExprTree { op: PreInc, args: vec![expression(node.child(0).expect(""), state)] }),
                "--" => return T(ExprTree { op: PreDec, args: vec![expression(node.child(0).expect(""), state)] }),
                _ => panic!("Unknown Updated Expression!")
            }
        },
        "cast_expression" => todo!(),
        "unary_expression" => Operand::T(ExprTree {
            op: state.tsretriever.get_op(&node),
            args: vec![expression(state.tsretriever.get_field(&node, "operand"), state)]
        }),
        "switch_expression" => panic!("Switches are not supported as expressions!"),

        //------- PRIMARY EXPRESSIONS ---------//

        // _literal
        "decimal_integer_literal" | "hex_integer_literal" | "octal_integer_literal" |
        "binary_integer_literal" | "decimal_floating_point_literal" | "hex_floating_point_literal" |
        "true" | "false" | "character_literal" | "string_literal" | "null_literal" =>
            O::C(state.tsretriever.get_lit(&node)),

        "class_literal" => panic!("Class Literals are not supported!"),
        "this" => O::This,
        "identifier" => {
            let iname = state.tsretriever.get_text(&node);
            O::V(state.scope.find(iname).expect(
                &format!("Identifier {} not found", iname)))
        }

        "parenthesized_expression" => expression(node.child(1).expect("parenthesized_expression"), state),
        "object_creation_expression" => todo!(),
        "field_access" => O::T(ExprTree {
            op: Operation::Access,
            args: vec![expression(state.tsretriever.get_field(&node, "object"), state),
                        expression(state.tsretriever.get_field(&node, "field"), state)]
        }),
        "array_access" => O::T(ExprTree {
            op: Operation::Index,
            args: vec![expression(state.tsretriever.get_field(&node, "array"), state),
                        expression(state.tsretriever.get_field(&node, "index"), state)]
        }),
        "method_invocation" => todo!(),
        "method_reference" => todo!(),
        "array_creation_expression" => todo!(),
        "template_expression" => todo!(),

        // _reserved_identifier
        "open" | "module" | "record" | "with" | "yield" | "seal" => panic!("Reserved Identifiers are not supported!"),
        other => panic!("Unknown Expression Type: {}", other)
    }
}

pub fn binary_expression(node: Node, state: &mut State) -> Operand {
    Operand::T(ExprTree {
        op: state.tsretriever.get_op(&node),
        args: vec![
            expression(state.tsretriever.get_field(&node, "left"), state),
            expression(state.tsretriever.get_field(&node, "right"), state)
        ]
    })
}


#[cfg(test)]
mod tests {
    use tree_sitter::Parser;
    use crate::printer::Printer;
    use super::*;

    #[test]
    fn tinker() {
        let code = r#"
        import java.util.Scanner;
        class Test extends Object {
            int y;
            static int thing(int x, int z) throws Exception {
                int a, b, c = 3;
                label: a += 2;
                switch (x) {
                    case 0:
                    case 1: return 2;
                    case 2:
                    case 3: return a;
                    case 4: return 3;
                    default: return 2;
                }
                if (x < 2) {
                    return 3;
                } else {
                    return 2;
                }
                return x * 2;
            }
        }
        "#;

        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
        let tree = parser.parse(code, None).unwrap();
        let mut sm = SymbolMaker::new();
        println!("{}", tree.root_node());
        let ast = convert(tree.root_node(), code.as_bytes(), &mut sm);
        let mut printer = Printer::new(&sm);
        printer.print_tree(&ast);
        assert!(false);
    }
}