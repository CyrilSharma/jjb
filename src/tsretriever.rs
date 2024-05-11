use std::str::FromStr;

use tree_sitter::Node;
use crate::ir::{Literal, Operation, Typ};

pub struct TsRetriever<'l> { source: &'l[u8] }
impl<'l> TsRetriever<'l> {
    pub fn new(source: &'l[u8]) -> Self {
        Self { source }
    }

    pub fn get_text(&self, node: &Node) -> &'l str {
        node.utf8_text(self.source)
            .unwrap_or_else(|_| panic!("Invalid UTF-8"))
    }
    
    // Define a method that extracts text from a node and parses it into type `T`
    pub fn parse_text<T: FromStr>(&self, node: &Node) -> T {
        let text = node.utf8_text(self.source).expect("Invalid UTF-8");
        if let Ok(res) = text.parse::<T>() { res }
        else { panic!("Unable to parse text into type!") }
    }
    
    pub fn get_field<'tree>(&self, node: &'tree Node<'tree>, field_name: &str) -> Node<'tree> {
        node.child_by_field_name(field_name)
            .unwrap_or_else(|| panic!("Field '{}' is missing", field_name))
    }
    
    pub fn get_field_text(&self, node: &Node, field_name: &str) -> &'l str {
        self.get_field(node, field_name)
            .utf8_text(self.source)
            .unwrap_or_else(|_| panic!("Field '{}' contains invalid UTF-8", field_name))
    }
    
    pub fn get_label(&self, node: &Node) -> Option<&'l str> {
        node.parent().map(|p| if p.kind() == "labeled_statement" {
            Some(self.get_text(&node.child(0).expect("label DNE!")))
        } else {
            None
        }).flatten()
    }
    
    pub fn get_typ(&self, node: &Node) -> Typ {
        match self.get_field_text(node, "type") {
            "byte" => Typ::Byte,
            "short" => Typ::Short,
            "int" => Typ::Int,
            "long" => Typ::Long,
            "char" => Typ::Char,
            "float" => Typ::Float,
            "double" => Typ::Double,
            "boolean" => Typ::Bool,
            other => panic!("Unknown Tree-Sitter Type: {}\n", other)
        }
    }
    
    pub fn get_op(&self, node: &Node) -> Operation {
        use Operation as O;
        match self.get_field_text(node, "operator") {
            ">" => O::G,
            "<" => O::L,
            ">=" => O::GEq,
            "<=" => O::LEq,
            "==" => O::Eq,
            "!=" => O::Neq,
            "&&" => O::LAnd,
            "||" => O::LOr,
    
            "+" => O::Add,
            "-" => O::Sub,
            "*" => O::Mul,
            "/" => O::Div,
            "&" => O::And,
            "|" => O::Or,
            "^" => O::Xor,
            "%" => O::Mod,
            "<<" => O::Shl,
            ">>" => O::Shr,
            ">>>" => O::UShr,
            "!" => O::LNot,
            "~" => O::Not,
    
            "+=" => O::PSet,
            "-=" => O::SSet,
            "*=" => O::MSet,
            "/=" => O::DSet,
            "%=" => O::ModSet,
            "&=" => O::AndSet,
            "|=" => O::OrSet,
            "^=" => O::XorSet,
            ">>=" => O::ShrSet,
            ">>>=" => O::UshrSet,
            "<<=" => O::ShlSet,
    
            other => panic!("Unknown Tree-Sitter Op: {}\n", other)
        }
    }
    
    pub fn get_lit(&self, node: &Node) -> Literal {
        use Literal::*;
        match node.kind() {
            "decimal_integer_literal" => Long(self.parse_text(&node)),
            "hex_integer_literal" => Long(self.parse_text(&node)),
            "octal_integer_literal" => Long(self.parse_text(&node)),
            "binary_integer_literal" => Long(self.parse_text(&node)),
            "decimal_floating_point_literal" => Double(self.parse_text(&node)),
            "hex_floating_point_literal" => Long(self.parse_text(&node)),
            "true" => Bool(true),
            "false" => Bool(false),
            "character_literal" => Char(self.parse_text(&node)),
            "string_literal" => String(self.parse_text(&node)),
            "null_literal" => Null,
            other => panic!("Unknown literal {}", other)
        }
    }
}