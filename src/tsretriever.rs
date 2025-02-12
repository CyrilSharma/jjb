use std::str::FromStr;

use crate::ir::{Literal, Operation, Typ};
use tree_sitter::Node;

pub struct TsRetriever<'l> {
    source: &'l [u8],
}
impl<'l> TsRetriever<'l> {
    pub fn new(source: &'l [u8]) -> Self {
        Self { source }
    }

    pub fn get_text(&self, node: &Node) -> &'l str {
        node.utf8_text(self.source)
            .unwrap_or_else(|_| panic!("Invalid UTF-8"))
    }

    // Define a method that extracts text from a node and parses it into type `T`
    pub fn parse_text<T: FromStr>(&self, node: &Node) -> T {
        let text = node.utf8_text(self.source).expect("Invalid UTF-8");
        if let Ok(res) = text.parse::<T>() {
            res
        } else {
            panic!("Unable to parse text into type!")
        }
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

    pub fn get_dims(&self, dim_node: &Node) -> u8 {
        let mut ndims = 0;
        let mut cursor = dim_node.walk();
        for child in dim_node.children(&mut cursor) {
            match child.kind() {
                "@" => panic!("Annotations are not supported!"),
                "[" => (),
                "]" => ndims += 1,
                other => panic!("Unknown Character {} Found in Dimensions!", other),
            }
        }
        ndims
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

            "=" => O::Set,
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

            other => panic!("Unknown Tree-Sitter Op: {}\n", other),
        }
    }

    pub fn get_lit(&self, node: &Node) -> Literal {
        use Literal::*;
        let parse_int = |var: &str| {
            if var.ends_with("L") {
                return Long(var[0..var.len() - 1].parse().expect(""));
            } else {
                return Int(var[0..var.len()].parse().expect(""));
            }
        };
        let parse_float = |var: &str| {
            if var.ends_with("L") {
                return Double(var[0..var.len() - 1].parse().expect(""));
            } else {
                return Float(var[0..var.len()].parse().expect(""));
            }
        };
        match node.kind() {
            "decimal_integer_literal" => parse_int(&self.get_text(node)),
            "hex_integer_literal" => parse_int(&self.get_text(node)),
            "octal_integer_literal" => parse_int(&self.get_text(node)),
            "binary_integer_literal" => parse_int(&self.get_text(node)),
            "decimal_floating_point_literal" => parse_float(&self.get_text(node)),
            "hex_floating_point_literal" => parse_float(&self.get_text(node)),
            "true" => Bool(true),
            "false" => Bool(false),
            "character_literal" => Char(self.parse_text(&node)),
            "string_literal" => String(self.parse_text(&node)),
            "null_literal" => Null,
            other => panic!("Unknown literal {}", other),
        }
    }
}
