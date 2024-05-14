mod ir;
mod converter;
mod tsretriever;
mod scope;
mod printer;
mod symbolmaker;
mod directory;
use converter::convert;
use symbolmaker::SymbolMaker;
use tree_sitter::{Parser, TreeCursor};

fn print_tree(mut cursor: TreeCursor, depth: usize) {
    let node = cursor.node();
    let kind = node.kind();
    let range = node.range();

    println!("{}- {} ({:?})", "  ".repeat(depth), kind, range);

    if cursor.goto_first_child() {
        loop {
            print_tree(cursor.clone(), depth + 1);
            if !cursor.goto_next_sibling() {
                break;
            }
        }
        cursor.goto_parent();
    }
}

fn main() {
    let source = r#"
    import java.util.Scanner;
    class Test {
        int y;
        int double(int x, int z) throws Exception {
            if (x == 2) {
                return 1;
            } else {
                return 2;
            }
            return x * 2;
        }
    }
    "#;

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
    let tree = parser.parse(source, None).unwrap();
    let mut sm = SymbolMaker::new();
    let ast = convert(tree.root_node(), source.as_bytes(), &mut sm);
}
