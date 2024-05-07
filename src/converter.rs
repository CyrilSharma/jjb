// https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
// We parse a subset of the above grammar.
use std::collections::LinkedList;

use crate::ir::*;
use crate::tshelpers as H;
use tree_sitter::Node;
pub struct Converter<'l> {
    source: &'l [u8],
    sm: &'l mut SymbolMaker,
    entry_name: &'l str,
    entry_sym: Option<Symbol>
}

impl<'l> Converter<'l> {
    pub fn new(source: &'l [u8], sm: &'l mut SymbolMaker) -> Self {
        Converter { entry_name: "Main", entry_sym: None, sm, source }
    }

    pub fn convert(&mut self, root: Node) -> Tree {
        assert!(root.kind() == "program");
        let mut cursor = root.walk();
        if !cursor.goto_last_child() { panic!("Empty Program!") }
        let mut tail = Box::new(Tree::EntryPoint(self.sm.fresh("Main".to_string())));
        loop {
            tail = self.top(cursor.node(), tail);
            if !cursor.goto_previous_sibling() { break }
        }
        *tail
    }

    pub fn top(&mut self, root: Node, tail: Box<Tree>) -> Box<Tree> {
        match root.kind() {
            "class_declaration" => self.class_declaration(root, tail),
            "import_declaration" => self.import_declaration(root, tail),
            other => panic!("Parse Tree uses unknown node {}\n", other)
        }
    }

    pub fn import_declaration(&mut self, node: Node, tail: Box<Tree>) -> Box<Tree> {
        let path = node.named_child(0).expect("Invalid Import")
            .utf8_text(self.source).expect("UTF8 Error");
        return Box::new(Tree::LetI(ImportStatement {
            path: path.to_string(),
            body: tail
        }));
    }

    pub fn class_declaration(&mut self, node: Node, tail: Box<Tree>) -> Box<Tree> {
        let mut members = Vec::new();
        let mut methods = LinkedList::new();
        if let Some(body) = node.child_by_field_name("body") {
            let mut bcursor = body.walk();
            for child in body.named_children(&mut bcursor) {
                match child.kind() {
                    "method_declaration" => methods.push_back(self.method_declaration(child)),
                    "field_declaration" => members.push(self.parse_field(child)),
                    other => panic!("Parse Tree uses unknown node {}\n", other)
                }
            }
        }
        
        let cname = H::get_field_text(&node, "name", &self.source);
        return Box::new(Tree::LetC(ClassDeclaration {
            name: self.sm.fresh(cname),
            members,
            methods,
            extends: todo!(),
            body: tail
        }));
    }

    fn parse_field(&mut self, node: Node) -> (Symbol, Typ) {
        let typ = H::get_typ(&node, self.source);
        let declarator = H::get_field(&node, "declarator");
        let name = H::get_field_text(&declarator, "name", self.source);
        (self.sm.fresh(name), typ)
    }

    pub fn method_declaration(&mut self, node: Node) -> Box<Tree> {
        let rtyp = H::get_typ(&node, self.source);
        let name = H::get_field_text(&node, "name", self.source);
        let name_sym = self.sm.fresh(name);

        let mut args = Vec::new();
        if let Some(params) = node.child_by_field_name("parameters") {
            let mut cursor = params.walk();
            for child in params.named_children(&mut cursor) {
                args.push((
                    self.sm.fresh(H::get_field_text(&child, "name", self.source)),
                    H::get_typ(&node, self.source),
                ));
            }
        }

        let mut body = node.child_by_field_name("body").map(|x| self.body(x));
        Box::new(Tree::LetF(FunDeclaration {
            name: name_sym,
            return_typ: rtyp,
            args,
            throws: todo!(),
            modifiers: todo!(),
            body
        }))
    }

    pub fn body(&mut self, node: Node) -> Box<Tree> {
        match node.kind() {
            "block" => self.block(node),
            other => panic!("Unknown body child: {}", other)
        }
    }

    pub fn block(&mut self, block: Node) -> Box<Tree> {
        if let Some(child) = block.named_child(0) {
            self.statement(child)
        } else {
            todo!()
        }
    }

    pub fn statement(&mut self, node: Node) -> Box<Tree> {
        match node.kind() {
            "declaration" => todo!(),
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
                name: self.sm.fresh("assert".to_string()),
                op: Operation::Assert,
                args: vec![node.child(1), node.child(2)].into_iter().flatten()
                    .map(|x| self.expression(x)).collect(),
                typ: Typ::Void,
                body: self.next(node)
            })),
            // requires special handling.
            "do_statement" => todo!(),
            "break_statement" => Box::new(Tree::LetP(PrimStatement {
                name: self.sm.fresh("break".to_string()),
                op: Operation::Break,
                args: Vec::new(),
                typ: Typ::Void,
                body: self.next(node)
            })),
            "continue_statement" => Box::new(Tree::LetP(PrimStatement {
                name: self.sm.fresh("continue".to_string()),
                op: Operation::Continue,
                args: Vec::new(),
                typ: Typ::Void,
                body: self.next(node)
            })),
            "return_statement" => Box::new(Tree::Return(ReturnStatement { 
                val: node.named_child(0).map(|child| self.expression(child))
            })),
            "yield_statement" => panic!("Yield is unsupported!"),
            "switch_expression" => todo!(),
            "synchronized_statement" => todo!(),
            "local_variable_declaration" => todo!(),
            "throw_statement" => todo!(),
            "try_statement" => todo!(),
            "try_with_resources_statement" => todo!(), 
            _ => panic!("Unsupported statement!")
        }
    }

    pub fn next(&mut self, node: Node) -> Box<Tree> {
        if let Some(sib) = node.next_named_sibling() {
            self.statement(sib)
        } else {
            Box::new(Tree::Jump)
        }
    }

    pub fn expression(&mut self, node: Node) -> Operand {
        match node.kind() {
            "assignment_expression" => self.binary_expression(node),
            "binary_expression" => self.binary_expression(node),
            "instanceof_expression" => todo!(),
            "lambda_expression" => todo!(),
            "ternary_expression" => Operand::T(Box::new(ExprTree {
                op: Operation::Ternary,
                args: vec![self.expression(H::get_field(&node, "condition")),
                           self.expression(H::get_field(&node, "consequence")),
                           self.expression(H::get_field(&node, "alternative"))]
            })),
            "update_expression" => todo!(),
            "primary_expression" => todo!(),
            "cast_expression" => todo!(),
            "unary_expression" => Operand::T(Box::new(ExprTree {
                op: H::get_op(&node, self.source),
                args: vec![self.expression(H::get_field(&node, "operand"))]
            })),
            "switch_expression" => todo!(),
            other => panic!("Unknown Expression Type: {}", other)
        }
    }

    pub fn primary_expression(&mut self, node: Node) -> Operand {
        if let Some(l) = H::parse_lit(&node, self.source) { return Operand::C(l) }
        match node.kind() {
            "class_literal" => todo!(),
            "this" => todo!(),
            "identifier" => Operand::V(self.lookup(&H::get_text(&node, self.source))),
            "parenthesized_expression" => todo!(),
            "object_creation_expression" => todo!(),
            "field_access" => todo!(),
            "array_access" => todo!(),
            "method_invocation" => todo!(),
            "method_reference" => todo!(),
            "array_creation_expression" => todo!(),
            "template_expression" => todo!(),
            res if H::is_reserved_identifier(res) => panic!("Reserved Identifiers are not supported!"),
            other => panic!("Unknown Primary Expression {}\n", other)
        }
    }

    pub fn binary_expression(&mut self, node: Node) -> Operand {
        Operand::T(Box::new(ExprTree {
            op: H::get_op(&node, self.source),
            args: vec![
                self.expression(H::get_field(&node, "left")),
                self.expression(H::get_field(&node, "right"))
            ]
        }))
    }

    pub fn switch_expression(&mut self, node: Node) -> Box<Tree> {
        // Note that even if switches appear as expression, we can still turn them into
        // Top-level statements, but pushing whatever expression tree they appear in, down into the branches.
        todo!()
    }

    pub fn lookup(&mut self, name: &str) -> Symbol {
        todo!()
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
            int double(int x, int z) throws Exception {
                return x * 2;
            }
        }
        "#;

        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
        let tree = parser.parse(code, None).unwrap();
        let mut sm = SymbolMaker::new();
        Converter::new(code.as_bytes(), &mut sm).convert(tree.root_node());
        assert!(false)
    }
}