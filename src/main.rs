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
    class Test {
        int double(int x) {
            return x * 2;
        }
    }
    "#;

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");

    let tree = parser.parse(code, None).unwrap();
    let cursor = tree.root_node().walk();

    println!("Parse tree:");
    print_tree(cursor, 0);
}
