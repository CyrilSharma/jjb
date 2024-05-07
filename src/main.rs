mod ir;
mod converter;
mod tshelpers;
use converter::Converter;
use ir::SymbolMaker;
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
    let code = r#"
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

    let tree = parser.parse(code, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
    // println!("Parse tree:");
    // print_tree(tree.root_node().walk(), 0);

    // let root = tree.root_node();
    // let mut rcursor = root.walk();
    // rcursor.goto_first_child();
    // println!("{}", rcursor.node());
    // // println!("{}", rcursor.get().unwrap());
    // // println!("------------------");

    // rcursor.goto_next_sibling();
    // println!("{}", rcursor.node());
    // rcursor.goto_parent();

    // let child = root.named_child(0).unwrap();
    // println!("{}", child.kind());
    // let mut ccursor = child.walk();
    // ccursor.goto_next_sibling();
    // println!("{}", ccursor.node());


    // println!("{}", child.child_by_field_name("name").unwrap());
    // let nchild = child.next_named_sibling().unwrap();

    // println!("HERE -----");
    // println!("{}", nchild);
    // println!("{}", nchild.child_by_field_name("name").unwrap());
}
