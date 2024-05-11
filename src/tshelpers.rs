use std::str::FromStr;

use tree_sitter::Node;
use crate::ir::{Literal, Operation, Typ};

pub fn get_text<'l>(node: &Node, source: &'l[u8]) -> &'l str {
    node.utf8_text(source)
        .unwrap_or_else(|_| panic!("Invalid UTF-8"))
}

// Define a method that extracts text from a node and parses it into type `T`
pub fn parse_text<T: FromStr>(node: &Node, source: &[u8]) -> T {
    let text = node.utf8_text(source).expect("Invalid UTF-8");
    if let Ok(res) = text.parse::<T>() { res }
    else { panic!("Unable to parse text into type!") }
}

pub fn get_field<'tree>(node: &'tree Node<'tree>, field_name: &str) -> Node<'tree> {
    node.child_by_field_name(field_name)
        .unwrap_or_else(|| panic!("Field '{}' is missing", field_name))
}

pub fn get_field_text<'l>(node: &Node, field_name: &str, source: &'l [u8]) -> &'l str {
    get_field(node, field_name)
        .utf8_text(source)
        .unwrap_or_else(|_| panic!("Field '{}' contains invalid UTF-8", field_name))
}

pub fn get_label<'l>(node: &Node, source: &'l [u8]) -> Option<&'l str> {
    node.parent().map(|p| if p.kind() == "labeled_statement" {
        Some(get_text(&node.child(0).expect("label DNE!"), source))
    } else {
        None
    }).flatten()
}

pub fn get_typ(node: &Node, source: &[u8]) -> Typ {
    match get_field_text(node, "type", source) {
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

pub fn get_op(node: &Node, source: &[u8]) -> Operation {
    use Operation as O;
    match get_field_text(node, "operator", source) {
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

pub fn get_lit(node: &Node, source: &[u8]) -> Literal {
    use Literal::*;
    match node.kind() {
        "decimal_integer_literal" => Long(parse_text(&node, source)),
        "hex_integer_literal" => Long(parse_text(&node, source)),
        "octal_integer_literal" => Long(parse_text(&node, source)),
        "binary_integer_literal" => Long(parse_text(&node, source)),
        "decimal_floating_point_literal" => Double(parse_text(&node, source)),
        "hex_floating_point_literal" => Long(parse_text(&node, source)),
        "true" => Bool(true),
        "false" => Bool(false),
        "character_literal" => Char(parse_text(&node, source)),
        "string_literal" => String(parse_text(&node, source)),
        "null_literal" => Null,
        other => panic!("Unknown literal {}", other)
    }
}