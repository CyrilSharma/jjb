mod ir;
mod converter;
mod tsretriever;
mod scope;
mod printer;
mod symbolmaker;
mod directory;
mod hoist;
mod container;
mod typeinfer;
mod typetracker;
use typeinfer::typeinfer;
use hoist::hoist;
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
        public static void main(String[] args) {
            int[][][][] a = new int[10][100][][];
        }
    }
    "#;

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
    let tree = parser.parse(source, None).unwrap();
    print_tree(tree.walk(), 0);
    println!("{}", tree.root_node());
    let mut sm = SymbolMaker::new();
    let mut ast = convert(tree.root_node(), source.as_bytes(), &mut sm);
    ast = hoist(ast.as_ref(), &mut sm);
    typeinfer(ast.as_mut(), &mut sm);
}
