// https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
// We parse a subset of the above grammar.
use std::collections::LinkedList;

use crate::ir::*;
use crate::tshelpers as H;
use crate::scope::Scope;
use crate::printer::Printer;
use tree_sitter::Node;
pub struct Converter<'l> {
    source: &'l [u8],
    sm: &'l mut SymbolMaker,
    scope: Scope<'l>,
    entry_name: &'l str,
    entry_sym: Option<Symbol>
}

impl<'l> Converter<'l> {
    pub fn new(source: &'l [u8], sm: &'l mut SymbolMaker) -> Self {
        Converter {
            entry_name: "Main",
            entry_sym: None,
            scope: Scope::new(),
            sm,
            source
        }
    }

    pub fn convert(&mut self, root: Node) -> Tree {
        assert!(root.kind() == "program");
        let mut cursor = root.walk();
        *self.statement(root.child(0).expect("Empty Program!"))
    }

    pub fn statement(&mut self, node: Node) -> Box<Tree> {
        match node.kind() {
            // Top-Level Declarations...
            "module_declaration" => panic!("Modules are not supported!"),
            "package_declaration" => self.next(node), /* packages are inlined */
            "import_declaration" => self.import_declaration(node),
            "class_declaration" => self.class_declaration(node),
            "record_declaration" => panic!("Records are not supported!"),
            "interface_declaration" => todo!(),
            "annotation_type_declaration" => panic!("Annotations are not supported!"),
            "enum_declaration" => self.enum_declaration(node),

            "expression_statement" => todo!(),
            "labeled_statement" => todo!(),
            "if_statement" => Box::new(Tree::If(IfStatement {
                cond: self.expression(H::get_field(&node, "condition")),
                btrue: self.statement(H::get_field(&node, "consequence")),
                bfalse: node.child_by_field_name("alternative").map(|child| self.statement(child)),
                body: self.next(node)
            })),
            "while_statement" => Box::new(Tree::Loop(LoopStatement {
                cond: self.expression(H::get_field(&node, "condition")),
                lbody: Some(self.statement(H::get_field(&node, "body"))),
                body: self.next(node)
            })),
            "for_statement" => todo!(),
            "enhanced_for_statement" => todo!(),
            "block" => todo!(),
            ";" => self.next(node),
            "assert_statement" => Box::new(Tree::LetP(PrimStatement {
                name: self.sm.fresh("assert"),
                typ: Typ::Void,
                exp: Operand::T(ExprTree {
                    op: Operation::Assert,
                    args: vec![node.child(1), node.child(2)].into_iter().flatten()
                    .map(|x| self.expression(x)).collect()
                }),
                body: self.next(node)
            })),
            // requires special handling.
            "do_statement" => todo!(),
            "break_statement" => Box::new(Tree::LetP(PrimStatement {
                name: self.sm.fresh("break"),
                typ: Typ::Void,
                exp: Operand::T(ExprTree {
                    op: Operation::Break,
                    args: vec![]
                }),
                body: self.next(node)
            })),
            "continue_statement" => Box::new(Tree::LetP(PrimStatement {
                name: self.sm.fresh("continue"),
                typ: Typ::Void,
                exp: Operand::T(ExprTree {
                    op: Operation::Continue,
                    args: vec![]
                }),
                body: self.next(node)
            })),
            "return_statement" => Box::new(Tree::Return(ReturnStatement { 
                val: node.named_child(0).map(|child| self.expression(child))
            })),
            "yield_statement" => panic!("Yield is unsupported!"),
            "switch_expression" => self.switch_statement(node),
            "synchronized_statement" => todo!(),
            "local_variable_declaration" => todo!(),
            "throw_statement" => todo!(),
            "try_statement" => todo!(),
            "try_with_resources_statement" => todo!(), 
            _ => panic!("Unsupported statement!")
        }
    }

    /*
     * Grabs the next statement.
     * If you're a top-level node, this inserts the entry-point call.
     * Otherwise, you must be at the end of a function or branch, so a jump is required.
     */
    pub fn next(&mut self, node: Node) -> Box<Tree> {
        if let Some(sib) = node.next_named_sibling() {
            return self.statement(sib)
        }
        if let Some(p) = node.parent() {
            if p.parent().is_none() {
                return Box::new(Tree::EntryPoint(self.sm.fresh(self.entry_name)))
            }
        }
        return Box::new(Tree::Jump)
    }

    pub fn import_declaration(&mut self, node: Node) -> Box<Tree> {
        let path = node.named_child(0).expect("Invalid Import")
            .utf8_text(self.source).expect("UTF8 Error");
        return Box::new(Tree::LetI(ImportStatement {
            path: path.to_string(),
            body: self.next(node)
        }));
    }

    pub fn enum_declaration(&mut self, node: Node) -> Box<Tree> {
        todo!()
    }

    pub fn class_declaration(&mut self, node: Node) -> Box<Tree> {
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
            self.scope.scope_in();
            let mut bcursor = body.walk();
            for child in body.named_children(&mut bcursor) {
                match child.kind() {
                    "method_declaration" => methods.push_back(self.method_declaration(child)),
                    "field_declaration" => members.push(self.parse_field(child)),
                    other => panic!("Parse Tree uses unknown node {}\n", other)
                }
            }
            self.scope.scope_out();
        }
        
        let cname = H::get_field_text(&node, "name", &self.source);
        return Box::new(Tree::LetC(ClassDeclaration {
            name: csym,
            members,
            methods,
            extends: superclass,
            body: self.next(node)
        }));
    }

    fn parse_field(&mut self, node: Node) -> (Symbol, Typ) {
        let typ = H::get_typ(&node, self.source);
        let declarator = H::get_field(&node, "declarator");
        let name = H::get_field_text(&declarator, "name", self.source);
        (self.sm.fresh(name), typ)
    }

    fn switch_statement(&mut self, node: Node) -> Box<Tree> {
        let arg = self.expression(H::get_field(&node, "condition"));
        Box::new(Tree::Switch(SwitchStatement {
            arg,
            cases: todo!(),
            branches: todo!(),
            default: todo!(),
            body: self.next(node)
        }))
    }

    fn method_declaration(&mut self, node: Node) -> Box<Tree> {
        let rtyp = H::get_typ(&node, self.source);
        let name = H::get_field_text(&node, "name", self.source);
        let name_sym = self.sm.fresh(name);

        let mut modifiers = Vec::new();
        let mods = node.named_child(0).expect("Method has 0 children.");
        if mods.kind() == "modifiers" {
            let mut cursor = mods.walk();
            for child in mods.named_children(&mut cursor) {
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
        
        let body = node.child_by_field_name("body").map(|x| self.block(x));
        Box::new(Tree::LetF(FunDeclaration {
            name: name_sym,
            return_typ: rtyp,
            args,
            throws,
            modifiers,
            body
        }))
    }

    pub fn block(&mut self, block: Node) -> Box<Tree> {
        self.scope.scope_in();
        let res = if let Some(child) = block.named_child(0) {
            self.statement(child)
        } else {
            todo!()
        };
        self.scope.scope_out();
        res
    }

    pub fn expression(&mut self, node: Node) -> Operand {
        use Operand as O;
        use Literal as L;
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
            "update_expression" => todo!(),
            "cast_expression" => todo!(),
            "unary_expression" => Operand::T(ExprTree {
                op: H::get_op(&node, source),
                args: vec![self.expression(H::get_field(&node, "operand"))]
            }),
            "switch_expression" => panic!("Switches are not supported as expressions!"),

            //------- PRIMARY EXPRESSIONS ---------//

            // _literal
            "decimal_integer_literal" => O::C(L::Long(H::parse_text(&node, source))),
            "hex_integer_literal" => O::C(L::Long(H::parse_text(&node, source))),
            "octal_integer_literal" => O::C(L::Long(H::parse_text(&node, source))),
            "binary_integer_literal" => O::C(L::Long(H::parse_text(&node, source))),
            "decimal_floating_point_literal" => O::C(L::Double(H::parse_text(&node, source))),
            "hex_floating_point_literal" => O::C(L::Long(H::parse_text(&node, source))),
            "true" => O::C(L::Bool(true)),
            "false" => O::C(L::Bool(false)),
            "character_literal" => O::C(L::Char(H::parse_text(&node, source))),
            "string_literal" => O::C(L::String(H::parse_text(&node, source))),
            "null_literal" => O::C(L::Null),

            "class_literal" => panic!("Class Literals are not supported!"),
            "this" => O::This,
            "identifier" => {
                let iname = H::get_text(&node, source);
                O::V(self.scope.find(iname).expect(
                    &format!("Identifier {} not found", iname)))
            }

            "parenthesized_expression" => self.expression(node.named_child(1).expect("parenthesized_expression")),
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

    use super::*;

    #[test]
    fn tinker() {
        let code = r#"
        import java.util.Scanner;
        class Test extends Object {
            int y;
            static int thing(int x, int z) throws Exception {
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