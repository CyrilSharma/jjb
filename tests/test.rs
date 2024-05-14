use std::fs::{self, write, File};
use std::io::{self, Write};
use std::panic::catch_unwind;
use std::process::Command;
use jjb::printer::print;
use jjb::{converter::convert, ir::Tree, printer::file_print, symbolmaker::SymbolMaker};
use tree_sitter::Parser;

macro_rules! method_test {
    ($func:ident, $str:tt) => {
        #[test]
        fn $func() {
            test_method(stringify!($func), $str);
        }
    };
}

fn run(fname: &str) -> String {
    let output = Command::new("java")
        .arg(fname)
        .output()
        .expect("Failed to execute command");
    String::from_utf8_lossy(&output.stdout).to_string()
}

fn test_equal(source: &str, tree: &Tree, sm: &SymbolMaker, name: &str) {
    let source_path = format!("{}_source_temp.java", name);
    let compile_path = format!("{}_compile_temp.java", name);
    write(source_path.clone(), source).expect("Source Write Failed!");
    let compiled_file = File::create(compile_path.clone()).expect("Compile Write Failed!");
    file_print(tree, sm, compiled_file);
    let res_source = run(&source_path);
    let res_compiled = run(&compile_path);
    if res_source != res_compiled {
        println!("------- SOURCE ------");
        println!("{}", source);
        println!("------- COMPILED ------");
        print(tree, sm);
        println!("----------------------");
        assert_eq!(res_source, res_compiled);
    }
    fs::remove_file(source_path).expect("File removal failed!");
    fs::remove_file(compile_path).expect("File removal failed!");
}

fn test(name: &str, source: &str) {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
    let tree = parser.parse(source, None).unwrap();
    let mut sm = SymbolMaker::new();
    let ast = convert(tree.root_node(), source.as_bytes(), &mut sm);
    test_equal(source, &ast, &sm, name)
    // The other phases will come here...
}

fn test_method(name: &str, source: &str) {
    test(name, &format!(r#"
        public class Test {{
            public static void main(String[] args) {{
                {}
            }}
        }}
    "#, source))
}

#[test]
fn test0() { 
    test("test0", r#"
        import java.util.Scanner;
        class Point {
            int x;
            int y;
        }
        class Test extends Object {
            int y;
            static int thing(int x, int z) throws Exception {
                int a, b, c = 3;
                label: a += 2;
                switch (x) {
                    case 0:
                    case 1: return 2;
                    case 2: break;
                    case 3: return a;
                    case 4: return 3;
                    default: return 2;
                }
                if (x < 2) {
                    return 3;
                } else {
                    return 2;
                }
                for (int i = 0; i < 10; i++) {
                    i += 1;
                }

                Point p = new Point(a, b);
                System.out.println(2);
                
                int w = 0;
                do {
                    w = 2;
                } while (w < 3);
                return x * 2;
            }
        }
        "#);
}



/* -------- LOOPS ---------- */
method_test!(for_1, r#"
    int count = 0;
    for (int i = 0; i < 10; i++) {
        count++;
    }
    System.out.println(count);
"#);

method_test!(for_2, r#"
    int a, b, c, cnt;
    for (a=12, b=23, c=45, cnt=0; cnt < 100; cnt++, a <<= 1, b ^= c, c -= 3) {
        a ^= (b - c);
        b -= (c - ~a);
        c *= (a >> 2);
    }
    System.out.printf(
        "%d %d %d",
        a, b, c
    );
"#);

method_test!(for_3, r#"
    int hash = 0;
    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < i; j++) {
            for (int k = 0; k < j; k++) {
                hash ^= ((i ^ j) * (k | i)) + 1;
            }
        }
    }
    System.out.println(hash);
"#);

method_test!(do_1, r#"
    int count = 0;
    do {
        count++;
    } while (count < 100);
    System.out.println(count);
"#);

method_test!(while_loop_1, r#"
    int count = 0;
    while (count < 420) {
        count++;
    }
    System.out.println(count);
"#);

method_test!(many_loops, r#"
    int count = 0;
    for (int i = 0; i < 3; i++) {
        count += i;
        int j = 0;
        while (j < 2) {
            count += j;
            j++;
            int k = 0;
            do {
                count += k;
                k++;
            } while (k < 2);
        }
    }
    System.out.println(count);
"#);