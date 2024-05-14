use std::fs::{self, write, File};
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
    // Compile the Java file using javac
    let compile_output = Command::new("javac")
        .arg(fname)
        .output()
        .expect("Failed to execute 'javac' command");

    // Check if compilation was successful
    if !compile_output.status.success() {
        return String::from_utf8_lossy(&compile_output.stderr).to_string();
    }

    // Run the compiled Java class using java
    let class_name = fname.trim_end_matches(".java");
    let run_output = Command::new("java")
        .arg(class_name)
        .output()
        .expect("Failed to execute 'java' command");

    // Return the output of running the Java program
    String::from_utf8_lossy(&run_output.stdout).to_string()
}

fn cleanup(name: &str) {
    let _ = fs::remove_file(format!("{}.java", name));
    let _ = fs::remove_file(format!("{}.class", name));
}

fn test_equal(source: &str, tree: &Tree, sm: &SymbolMaker, name: &str) {
    let path = format!("{}.java", name);
    write(path.clone(), source).expect("Source Write Failed!");
    let res_source = run(&path);
    let compiled_file = File::create(path.clone()).expect("Compile Write Failed!");
    file_print(tree, sm, compiled_file);
    let res_compiled = run(&path);
    cleanup(name);
    if res_source != res_compiled {
        println!("------- SOURCE ------");
        println!("{}", source);
        println!("------- COMPILED ------");
        print(tree, sm);
        println!("----------------------");
        assert_eq!(res_source, res_compiled);
    }
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
    let indented_source = source.lines().map(|line| format!("    {}", line)).collect::<Vec<_>>().join("\n");
    test(name, &format!(r#"
public class {} {{
    public static void main(String[] args) {{
        {}
    }}
}}"#, name, indented_source))
}

// #[test]
// fn test0() { 
//     test("test0", r#"
//         import java.util.Scanner;
//         class Point {
//             int x;
//             int y;
//         }
//         class Test extends Object {
//             int y;
//             static int thing(int x, int z) throws Exception {
//                 int a, b, c = 3;
//                 label: a += 2;
//                 switch (x) {
//                     case 0:
//                     case 1: return 2;
//                     case 2: break;
//                     case 3: return a;
//                     case 4: return 3;
//                     default: return 2;
//                 }
//                 if (x < 2) {
//                     return 3;
//                 } else {
//                     return 2;
//                 }
//                 for (int i = 0; i < 10; i++) {
//                     i += 1;
//                 }

//                 Point p = new Point(a, b);
//                 System.out.println(2);
                
//                 int w = 0;
//                 do {
//                     w = 2;
//                 } while (w < 3);
//                 return x * 2;
//             }
//         }
//         "#);
// }



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

/* ----- CONDITIONALS ------- */
method_test!(if_1, r#"
    int i = 10;
    if (i < 12) {
        System.out.println("CORRECT");
    } else {
        System.out.println("INCORRECT");
    }
"#);

method_test!(if_2, r#"
    int i = 10;
    if (i > 12) {
        System.out.println("INCORRECT");
    } else if (i > 6) {
        System.out.println("CORRECT");
    } else {
        System.out.println("INCORRECT");
    }
"#);

method_test!(if_3, r#"
    int x = 129;
    int y = 238;
    int z = 0;
    
    if (x < y) {
        if (x > 15) {
            if (y > 10) {
                z = x - y;
            } else if (y < 5) {
                z = x * y;
            } else {
                z = x + y;
            }
        } else if (x < 5) {
            if (y > 10) {
                z = y << 2;
            } else if (y < 5) {
                z = x / y;
            } else {
                z = x * y;
            }
        } else {
            if (y > 10) {
                z = x ^ y;
            } else if (y < 5) {
                z = x & y;
            } else {
                z = y - x;
            }
        }
    }
    System.out.println(z);
"#);

method_test!(label_1, r#"
    int count = 0;
    label: {
        count += 1;
        if (count == 1) { break label; }
        count += 1;
    }
    System.out.println(count);
"#);

method_test!(label_2, r#"
    int i = 0, j = 0, k = 0;
    label1: while (i++ < 10) {
        label2: while (j++ < 10) {
            label3: while (k++ < 10) {
                System.out.printf("%d %d %d\n", i, j, k);
                if ((i ^ j) < (i ^ k)) {
                    continue label2;
                }
                if (i < 4) { continue label1; }
                break label3;
            }
            if (j > 3) { continue label2; }
            break label1;
        }
    }
"#);
