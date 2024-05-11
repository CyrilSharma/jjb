// https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
// We parse a subset of the above grammar.
use std::collections::LinkedList;

use crate::ir::*;
use crate::labelstack::LabelStack;
use crate::tshelpers as H;
use crate::scope::Scope;
use tree_sitter::Node;

type TailTp = dyn Fn(&mut Converter) -> Box<Tree>;


pub struct Converter<'l> {
    source: &'l [u8],
    sm: &'l mut SymbolMaker,
    scope: Scope<'l>,
    lstack: LabelStack<'l>,
    entry_name: &'l str,
    entry_sym: Option<Symbol>
}

impl<'l> Converter<'l> {
    pub fn new(source: &'l [u8], sm: &'l mut SymbolMaker) -> Self {
        Converter {
            entry_name: "Main",
            entry_sym: None,
            scope: Scope::new(),
            lstack: LabelStack::new(),
            sm,
            source
        }
    }

    pub fn scope_in(&mut self) {
        self.scope.scope_in();

        // self.labels.scope_in();
    }

    pub fn scope_out(&mut self) {
        self.scope.scope_out();
        // self.labels.scope_out();
    }

    pub fn convert(&mut self, root: Node) -> Box<Tree> {
        assert!(root.kind() == "program");
        let res = |this: &mut Self| Box::new(Tree::EntryPoint(this.sm.fresh("Main")));
        self.statement(root.child(0).expect("Empty Program!"), &res)
    }

    pub fn statement(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        match node.kind() {
            /* ------ TOP-LEVEL DECLARATIONS -------- */
            "module_declaration" => panic!("Modules are not supported!"),
            "package_declaration" => self.next(node, tail), /* packages are inlined */
            "import_declaration" => self.import_declaration(node, tail),
            "class_declaration" => self.class_declaration(node, tail),
            "record_declaration" => panic!("Records are not supported!"),
            "interface_declaration" => { println!("Warning: Interfaces are ignored!"); self.next(node, tail) },
            "annotation_type_declaration" => panic!("Annotations are not supported!"),
            "enum_declaration" => self.enum_declaration(node),

            /* ------ Method Statements ----------- */
            "expression_statement" => self.expression_statement(node, tail),
            "labeled_statement" => self.labeled_statement(node, tail),
            "if_statement" => self.if_statement(node, tail),
            "while_statement" => self.while_statement(node, tail),
            "for_statement" => todo!(),
            "enhanced_for_statement" => todo!(),
            "block" => self.block(node, tail),
            ";" => self.next(node, tail),
            "assert_statement" => self.assert_statement(node, tail),
            "do_statement" => todo!(),
            "break_statement" => todo!(), // Box::new(Tree::Break()),
            "continue_statement" => todo!(), // Box::new(Tree::Continue()),
            "return_statement" => Box::new(Tree::Return(ReturnStatement { 
                val: node.named_child(0).map(|child| self.expression(child))
            })),
            "yield_statement" => panic!("Yield is unsupported!"),
            "switch_expression" => self.switch_statement(node, tail),
            "synchronized_statement" => panic!("Synchronized is unsupported!"),
            "local_variable_declaration" => self.local_variable_declaration(node, tail),
            "throw_statement" => todo!(),
            "try_statement" => todo!(),
            "try_with_resources_statement" => todo!(),
            other => panic!("Unsupported statement {}!", other)
        }
    }

    /*
     * Grabs the next statement, if there is no next statement, tail is inserted.
     */
    pub fn next(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        if let Some(sib) = node.next_named_sibling() {
            self.statement(sib, tail)
        } else {
            tail(self)
        }
    }

    pub fn expression_statement(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        Box::new(Tree::LetP(PrimStatement {
            name: self.sm.fresh("expr_stmt"),
            typ: Typ::Void,
            exp: Some(self.expression(node.child(0).expect("Expression is non-null"))),
            body: self.next(node, tail),
            label: H::get_label(&node, self.source).map(|x| self.sm.fresh(x))
        }))
    }

    pub fn labeled_statement(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        let label_str = H::get_text(&node.child(0).expect("Label does not exist!"), self.source);
        let label_sym = self.sm.fresh(label_str);
        self.lstack.push(Some((label_str, label_sym)));
        let res = |this: &mut Self| { this.next(node, tail) };
        self.statement(
            node.child(2).expect("labeled statement lacks child!"),
            &res
        )
    }

    pub fn if_statement(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        let branch_tail = |this: &mut Converter| { Box::new(Tree::Break(todo!())) };
        Box::new(Tree::If(IfStatement {
            cond: self.expression(H::get_field(&node, "condition")),
            btrue: self.statement(H::get_field(&node, "consequence"), &branch_tail),
            bfalse: node.child_by_field_name("alternative").map(|child| self.statement(child, &branch_tail)),
            label: self.sm.fresh(H::get_label(&node, self.source).unwrap_or("_if_")),
            body: self.next(node, tail)
        }))
    }

    pub fn while_statement(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        let loop_tail = |this: &mut Self| { Box::new(Tree::Continue(todo!())) };
        Box::new(Tree::Loop(LoopStatement {
            cond: self.expression(H::get_field(&node, "condition")),
            lbody: Some(self.statement(H::get_field(&node, "body"), &loop_tail)),
            label: self.sm.fresh(H::get_label(&node, self.source).unwrap_or("_loop_")),
            body: self.next(node, tail)
        }))
    }

    pub fn assert_statement(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        Box::new(Tree::LetP(PrimStatement {
            name: self.sm.fresh("assert"),
            typ: Typ::Void,
            exp: Some(Operand::T(ExprTree {
                op: Operation::Assert,
                args: vec![node.child(1), node.child(2)].into_iter().flatten()
                .map(|x| self.expression(x)).collect()
            })),
            label: None,
            body: self.next(node, tail)
        }))
    }

    pub fn local_variable_declaration(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        let tp = H::get_typ(&node, self.source);
        let mut cur = node.named_child(1).expect("Declaration has 0 declarators!");
        let mut syms = Vec::new();
        let mut exps = Vec::new();
        loop {
            let name_str = H::get_field_text(&cur, "name", self.source);
            let name_sym = self.sm.fresh(name_str);
            let value = cur.child_by_field_name("value");
            let exp = value.map(|x| Operand::C(H::get_lit(&x, self.source)));
            syms.push(name_sym);
            exps.push(exp);
            self.scope.insert(name_str, name_sym);
            if let Some(nbr) = cur.next_named_sibling() {
                cur = nbr;
                continue;
            }
            // TODO dimensions, for when we declare arrays...
            break;
        };
        let mut res = self.next(node, tail);
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

    pub fn import_declaration(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        let path = node.named_child(0).expect("Invalid Import")
            .utf8_text(self.source).expect("UTF8 Error");
        return Box::new(Tree::LetI(ImportDeclaration {
            path: path.to_string(),
            body: self.next(node, tail)
        }));
    }

    pub fn enum_declaration(&mut self, node: Node) -> Box<Tree> {
        todo!()
    }

    pub fn class_declaration(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        let cname = H::get_field_text(&node, "name", &self.source);
        let csym = self.sm.fresh(cname);

        let mut superclass = None;
        if let Some(sc) = node.child_by_field_name("superclass") {
            let pname = H::get_text(
                &sc.child(1).expect("Superclass should have child."),
                self.source
            );
            if let Some(sym) = self.scope.find(pname) {
                superclass = Some(sym);
            } else {
                superclass = Some(self.sm.fresh(pname));
            }
        }

        let mut members = Vec::new();
        let mut methods = LinkedList::new();
        if let Some(body) = node.child_by_field_name("body") {
            self.scope_in();
            let mut bcursor = body.walk();
            for child in body.named_children(&mut bcursor) {
                match child.kind() {
                    "method_declaration" => methods.push_back(self.method_declaration(child)),
                    "field_declaration" => members.push(self.parse_field(child)),
                    other => panic!("Parse Tree uses unknown node {}\n", other)
                }
            }
            self.scope_out();
        }
        
        let cname = H::get_field_text(&node, "name", &self.source);
        return Box::new(Tree::LetC(ClassDeclaration {
            name: csym,
            members,
            methods,
            extends: superclass,
            body: self.next(node, tail)
        }));
    }

    fn parse_field(&mut self, node: Node) -> (Symbol, Typ) {
        let typ = H::get_typ(&node, self.source);
        let declarator = H::get_field(&node, "declarator");
        let name = H::get_field_text(&declarator, "name", self.source);
        (self.sm.fresh(name), typ)
    }

    fn switch_statement(&mut self, node: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        let arg = self.expression(H::get_field(&node, "condition"));
        let mut cases: Vec<(Vec<Operand>, Box<Tree>)> = Vec::new();
        let mut cur_args: Vec<Operand> = Vec::new();
        let mut default: Option<Box<Tree>> = None;

        let block = H::get_field(&node, "body");
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
                    let (ops, tree) = self.switch_block_statement_group(node);
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

        let temp = self.sm.fresh("OOGA");
        Box::new(Tree::Switch(SwitchStatement {
            arg,
            label: temp, // TODO
            cases: cases,
            default: default,
            body: self.next(node, tail)
        }))
    }

    fn switch_block_statement_group(&mut self, node: Node) -> (Vec<Operand>, Option<Box<Tree>>) {
        let mut ops: Vec<Operand> = Vec::new();
        let mut cur = node.walk();
        cur.goto_first_child();
        loop {
            let child = cur.node();
            match child.kind() {
                "switch_label" => self.switch_label(child).map(|e| ops.push(e)),
                _ => break
            };
            if !cur.goto_next_sibling() { return (ops, None) }
            if !cur.goto_next_sibling() { return (ops, None) }
        }
        let tail = |_: &mut Self| { Box::new(Tree::Terminal) };
        (ops, Some(self.statement(cur.node(), &tail)))
    }

    fn switch_label(&mut self, node: Node) -> Option<Operand> {
        let child = node.child(0).expect("Empty Switch Label!");
        match H::get_text(&child, self.source) {
            "case" => Some(self.expression(node.child(1).expect("Empty Case!"))),
            "default" => None,
            _ => panic!("Unknown switch label!")
        }
    }

    fn method_declaration(&mut self, node: Node) -> Box<Tree> {
        let rtyp = H::get_typ(&node, self.source);
        let name = H::get_field_text(&node, "name", self.source);
        let name_sym = self.sm.fresh(name);

        let mut modifiers = Vec::new();
        let mods = node.named_child(0).expect("Method has 0 children.");
        if mods.kind() == "modifiers" {
            let mut cursor = mods.walk();
            for child in mods.children(&mut cursor) {
                modifiers.push(H::get_text(&child, self.source).to_string());
            }
        }

        let mut throws: Vec<String> = Vec::new();
        let child_count = node.child_count();
        if let Some(c) = node.child(child_count - 2) {
            if c.kind() == "throws" {
                let mut cursor = c.walk();
                cursor.goto_first_child();
                while cursor.goto_next_sibling() {
                    throws.push(H::get_text(&cursor.node(), self.source).to_string())
                }
            }
        }

        let mut args = Vec::new();
        if let Some(params) = node.child_by_field_name("parameters") {
            let mut cursor = params.walk();
            for child in params.named_children(&mut cursor) {
                let argname = H::get_field_text(&child, "name", self.source);
                let argsym = self.sm.fresh(argname);
                args.push((argsym, H::get_typ(&node, self.source)));
                self.scope.insert(argname, argsym);
            }
        }
        
        let tail = |_: &mut Self| { Box::new(Tree::Return(ReturnStatement { val: None } )) };
        let body = node.child_by_field_name("body").map(|x| self.block(x, &tail));
        Box::new(Tree::LetF(FunDeclaration {
            name: name_sym,
            return_typ: rtyp,
            args,
            throws,
            modifiers,
            body
        }))
    }

    pub fn block(&mut self, block: Node, tail: &dyn Fn(&mut Self) -> Box<Tree>) -> Box<Tree> {
        self.scope_in();
        let res = if let Some(child) = block.named_child(0) {
            let child_tail = |this: &mut Self| {
                this.next(block, tail)
            };
            self.statement(child, &child_tail)
        } else {
            todo!()
        };
        self.scope_out();
        res
    }

    pub fn expression(&mut self, node: Node) -> Operand {
        use Operand as O;
        let source = self.source;
        match node.kind() {
            "assignment_expression" => self.binary_expression(node),
            "binary_expression" => self.binary_expression(node),
            "instanceof_expression" => todo!(),
            "lambda_expression" => todo!(),
            "ternary_expression" => Operand::T(ExprTree {
                op: Operation::Ternary,
                args: vec![self.expression(H::get_field(&node, "condition")),
                           self.expression(H::get_field(&node, "consequence")),
                           self.expression(H::get_field(&node, "alternative"))]
            }),
            "update_expression" => {
                use Operand::*;
                use Operation::*;
                match node.child(0).expect("Update Expression Has Child").kind() {
                    "++" => return T(ExprTree { op: PreInc, args: vec![self.expression(node.child(1).expect(""))] }),
                    "--" => return T(ExprTree { op: PreDec, args: vec![self.expression(node.child(1).expect(""))] }),
                    _ => ()
                }
                match node.child(1).expect("Update Expression Has Child").kind() {
                    "++" => return T(ExprTree { op: PreInc, args: vec![self.expression(node.child(0).expect(""))] }),
                    "--" => return T(ExprTree { op: PreDec, args: vec![self.expression(node.child(0).expect(""))] }),
                    _ => panic!("Unknown Updated Expression!")
                }
            },
            "cast_expression" => todo!(),
            "unary_expression" => Operand::T(ExprTree {
                op: H::get_op(&node, source),
                args: vec![self.expression(H::get_field(&node, "operand"))]
            }),
            "switch_expression" => panic!("Switches are not supported as expressions!"),

            //------- PRIMARY EXPRESSIONS ---------//

            // _literal
            "decimal_integer_literal" | "hex_integer_literal" | "octal_integer_literal" |
            "binary_integer_literal" | "decimal_floating_point_literal" | "hex_floating_point_literal" |
            "true" | "false" | "character_literal" | "string_literal" | "null_literal" =>
                O::C(H::get_lit(&node, source)),

            "class_literal" => panic!("Class Literals are not supported!"),
            "this" => O::This,
            "identifier" => {
                let iname = H::get_text(&node, source);
                O::V(self.scope.find(iname).expect(
                    &format!("Identifier {} not found", iname)))
            }

            "parenthesized_expression" => self.expression(node.child(1).expect("parenthesized_expression")),
            "object_creation_expression" => todo!(),
            "field_access" => O::T(ExprTree {
                op: Operation::Access,
                args: vec![self.expression(H::get_field(&node, "object")),
                           self.expression(H::get_field(&node, "field"))]
            }),
            "array_access" => O::T(ExprTree {
                op: Operation::Index,
                args: vec![self.expression(H::get_field(&node, "array")),
                           self.expression(H::get_field(&node, "index"))]
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

    pub fn binary_expression(&mut self, node: Node) -> Operand {
        Operand::T(ExprTree {
            op: H::get_op(&node, self.source),
            args: vec![
                self.expression(H::get_field(&node, "left")),
                self.expression(H::get_field(&node, "right"))
            ]
        })
    }
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
        let ast = Converter::new(code.as_bytes(), &mut sm).convert(tree.root_node());
        let mut printer = Printer::new(&sm);
        printer.print_tree(&ast);
        assert!(false);
    }
}