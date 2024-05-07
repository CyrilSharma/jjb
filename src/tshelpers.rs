use tree_sitter::Node;
use crate::ir::{Literal, Operation, Typ};

pub fn get_text(node: &Node, source: &[u8]) -> String {
    node.utf8_text(source)
        .unwrap_or_else(|_| panic!("Invalid UTF-8"))
        .to_string()
}

pub fn get_field<'tree>(node: &'tree Node<'tree>, field_name: &str) -> Node<'tree> {
    node.child_by_field_name(field_name)
        .unwrap_or_else(|| panic!("Field '{}' is missing", field_name))
}

// TODO: change this to return a &str, only if everything else works...
pub fn get_field_text(node: &Node, field_name: &str, source: &[u8]) -> String {
    get_field(node, field_name)
        .utf8_text(source)
        .unwrap_or_else(|_| panic!("Field '{}' contains invalid UTF-8", field_name))
        .to_string()
}

pub fn get_typ(node: &Node, source: &[u8]) -> Typ {
    match get_field_text(node, "type", source).as_str() {
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
    match get_field_text(node, "operator", source).as_str() {
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
        other => panic!("Unknown Tree-Sitter Op: {}\n", other)
    }
}

pub fn parse_lit(node: &Node, source: &[u8]) -> Option<Literal> {
    use Literal as L;
    match node.kind() {
        "decimal_integer_literal" => Some(L::Long(node.utf8_text(source).unwrap().parse().unwrap())),
        "hex_integer_literal" => Some(L::Long(node.utf8_text(source).unwrap().parse().unwrap())),
        "octal_integer_literal" => Some(L::Long(node.utf8_text(source).unwrap().parse().unwrap())),
        "binary_integer_literal" => Some(L::Long(node.utf8_text(source).unwrap().parse().unwrap())),
        "decimal_floating_point_literal" => Some(L::Double(node.utf8_text(source).unwrap().parse().unwrap())),
        "hex_floating_point_literal" => Some(L::Long(node.utf8_text(source).unwrap().parse().unwrap())),
        "true" => Some(L::Bool(true)),
        "false" => Some(L::Bool(false)),
        "character_literal" => Some(L::Char(node.utf8_text(source).unwrap().parse().unwrap())),
        "string_literal" => Some(L::String(node.utf8_text(source).unwrap().parse().unwrap())),
        "null_literal" => Some(L::Null),
        _ => return None
    }
}

pub fn is_reserved_identifier(kind: &str) -> bool {
    match kind {
        "open" => return true,
        "module" => return true,
        "record" => return true,
        "with" => return true,
        "yield" => return true,
        "seal" => return true,
        other => return false
    }
}