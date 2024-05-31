mod ir;
mod converter;
mod tsretriever;
mod scope;
mod printer;
mod symbolmanager;
mod directory;
mod hoist;
mod container;
mod typeinfer;
mod optimizer;
mod substitution;
mod parameters;
mod ssa;
mod flatten;
mod graph;
use typeinfer::typeinfer;
use hoist::hoist;
use converter::convert;
use symbolmanager::SymbolManager;
use tree_sitter::{Parser, TreeCursor};
use parameters::Parameters;
use optimizer::optimize;

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
    let mut sm = SymbolManager::new();
    let params = Parameters { entry_class: "".to_string(), entry_name: "main".to_string() };
    let mut ast = convert(tree.root_node(), source.as_bytes(), &params, &mut sm);
    ast = hoist(ast.as_ref(), &mut sm);
    ast = optimize(ast.as_ref(), &mut sm);
    typeinfer(ast.as_mut(), &mut sm);
}
