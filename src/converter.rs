// https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
// We parse a subset of the above grammar.
use std::collections::LinkedList;

use crate::ir::*;
use tree_sitter::{Node, Parser, TreeCursor};
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
        
        let cname = self.get_field_text(&node, "name");
        return Box::new(Tree::LetC(ClassDeclaration {
            name: self.sm.fresh(cname),
            members,
            methods,
            extends: todo!(),
            body: tail
        }));
    }

    fn parse_field(&mut self, node: Node) -> (Symbol, Typ) {
        let typ = Converter::convert_tstyp(&self.get_field_text(&node, "type"));
        let declarator = node.child_by_field_name("declarator")
            .expect("Field name does not have declarator!");
        let name = self.get_field_text(&declarator, "name");
        (self.sm.fresh(name), typ)
    }

    pub fn method_declaration(&mut self, node: Node) -> Box<Tree> {
        let rtyp = Converter::convert_tstyp(&self.get_field_text(&node, "type"));
        let name = self.get_field_text(&node, "name");
        let name_sym = self.sm.fresh(name);

        let mut args = Vec::new();
        if let Some(params) = node.child_by_field_name("parameters") {
            let mut cursor = params.walk();
            for child in params.named_children(&mut cursor) {
                let temp = self.get_field_text(&child, "name");
                args.push((
                    self.sm.fresh(temp),
                    Converter::convert_tstyp(&self.get_field_text(&child, "type")),
                ));
            }
        }

        let mut body: Option<Box<Tree>> = None;
        if let Some(child) = node.child_by_field_name("body") {
            body = Some(self.body(child)); 
        };

        Box::new(Tree::LetF(FunDeclaration {
            name: name_sym,
            return_typ: rtyp,
            args,
            throws: todo!(),
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
        let mut cursor = block.walk();
        // In some situations this may be ok, but this is just a quick fix.
        if !cursor.goto_last_child() { panic!("Empty Block!") }
        // If the function is non-void, then this is redundant, DCE will remove it.
        // Otherwise, it is implicit in a void function, and must be added explicitly.
        let mut tail = Box::new(Tree::Return(ReturnStatement{ val: None }));
        loop {
            tail = self.statement(cursor.node(), tail);
            if !cursor.goto_previous_sibling() { break }
        }
        tail
    }

    pub fn statement(&mut self, node: Node, tail: Box<Tree>) -> Box<Tree> {
        match node.kind() {
            "declaration" => todo!(),
            "expression_statement" => todo!(),
            "labeled_statement" => todo!(),
            "if_statement" => todo!(),
            "while_statement" => todo!(),
            "for_statement" => todo!(),
            "enhanced_for_statement" => todo!(),
            "block" => todo!(),
            ";" => tail,
            "assert_statement" => todo!(),
            "do_statement" => todo!(),
            "break_statement" => todo!(),
            "continue_statement" => todo!(),
            "return_statement" => todo!(),
            "yield_statement" => todo!(),
            "switch_expression" => todo!(),
            "synchronized_statement" => todo!(),
            "local_variable_declaration" => todo!(),
            "throw_statement" => todo!(),
            "try_statement" => todo!(),
            "try_with_resources_statement" => todo!(), 
            _ => panic!("Unsupported statement!")
        }
    }


    fn get_field_text(&mut self, node: &Node, field_name: &str) -> String {
        let child = node.child_by_field_name(field_name)
            .unwrap_or_else(|| panic!("Field '{}' is missing", field_name));
        child.utf8_text(self.source)
            .unwrap_or_else(|_| panic!("Field '{}' contains invalid UTF-8", field_name))
            .to_string()
    }

    pub fn convert_tstyp(name: &str) -> Typ {
        match name {
            "int" => Typ::Int,
            other => panic!("Unknown Tree-Sitter Type: {}\n", other)
        }
    }
}


#[cfg(test)]
mod tests {
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