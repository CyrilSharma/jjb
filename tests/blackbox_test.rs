use std::fs::{self, write};
use std::process::Command;
use jjb::printer::print;
use jjb::{converter::convert, ir::Tree, printer::str_print, symbolmaker::SymbolMaker};
use tree_sitter::Parser;

macro_rules! method_test {
    ($func:ident, $str:tt) => {
        #[test]
        fn $func() {
            test_method(stringify!($func), $str);
        }
    };
}

macro_rules! classes_test {
    ($func:ident, $str:tt) => {
        #[test]
        fn $func() {
            test_classes(stringify!($func), $str);
        }
    };
}

fn stash(fname: &str, content: &str) {
    write(&format!("testfiles/{}", fname), content).expect("Write Failed!");
}

fn compile(fname: &str) {
    let compile_output = Command::new("javac")
        .arg(fname)
        .current_dir("testfiles/")
        .output()
        .expect("Failed to execute 'javac' command");
    let stderr = String::from_utf8_lossy(&compile_output.stderr).to_string();
    assert!(
        stderr.trim().is_empty(),
        "stderr is not empty: {}",
        stderr
    );
}

fn execute(fname: &str) -> String {
     let class_name = fname.trim_end_matches(".java");
     let run_output = Command::new("java")
         .arg(class_name)
         .current_dir("testfiles/")
         .output()
         .expect("Failed to execute 'java' command");
    let stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&run_output.stderr).to_string();
    assert!(
        stderr.trim().is_empty(),
        "stderr is not empty: {}",
        stderr
    );
    stdout
}

fn execute_and_cache(fname: &str) -> String {
    let res = execute(&fname);
    let cache_path = format!("{}_cache.txt", fname);
    stash(&cache_path, &res);
    res
}

fn read_cache(fname: &str) -> Result<String, std::io::Error> {
    let cache_path = format!("testfiles/{}_cache.txt", fname);
    fs::read_to_string(cache_path)
}

fn _run(fname: &str, text: &str) -> String {
    stash(fname, text);
    compile(&fname);
    execute_and_cache(fname)
}

fn run(fname: &str, text: &str) -> String {
    let content = fs::read_to_string(format!("testfiles/{}", fname));
    if let Ok(res) = content {
        if res != text {
            return _run(fname, text);
        } else if let Ok(cache) = read_cache(&fname) {
            return cache;
        }
    }
    _run(fname, text)
}

fn test_equal(source: &str, compile: &str, tree: &Tree, sm: &SymbolMaker, name: &str) {
    let source_path = format!("{}_source.java", name);
    let compile_path = format!("{}_compile.java", name);
    let mut buffer: Vec<u8> = Vec::new();
    buffer.reserve(compile.len());
    str_print(tree, sm, &mut buffer);
    let buffer_str = std::str::from_utf8(&buffer).expect("Invalid UTF-8");
    let res_source = run(&source_path, &source);
    let res_compiled = run(&compile_path, &buffer_str);
    if res_source != res_compiled {
        println!("------- SOURCE ------");
        println!("{}", source);
        println!("------- COMPILED ------");
        print(tree, sm);
        println!("----------------------");
        assert_eq!(res_source, res_compiled);
    }
}

fn test(name: &str, source: &str, compile: &str) {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_java::language()).expect("Error loading Java grammar");
    let tree = parser.parse(compile, None).unwrap();
    let mut sm = SymbolMaker::new();
    let ast = convert(tree.root_node(), compile.as_bytes(), &mut sm);
    test_equal(source, compile, &ast, &sm, name)
    // The other phases will come here...
}

// You can make this better by having test dynamically insert the headers, where the headers
// Just call Test.main() same as class. Then, it will be easy to generalize this for
// Testing all compiler phases simultaneously.
fn test_method(name: &str, source: &str) {
    let indented_source = source.lines().map(|line| format!("    {}", line)).collect::<Vec<_>>().join("\n");
    test(name, &format!(r#"
public class {}_source {{
    public static void main(String[] args) {{
        {}
    }}
}}"#, name, indented_source), &format!(r#"
public class {}_compile {{
    public static void main(String[] args) {{
        {}
    }}
}}"#, name, indented_source),
    );
}

fn test_classes(name: &str, source: &str) {
    let indented_source = source.lines().map(|line| format!("    {}", line)).collect::<Vec<_>>().join("\n");
    test(name, &format!(r#"
public class {}_source {{
    public static void main(String[] args) {{
        Test.main();
    }}
}}
{}"#, name, indented_source), &format!(r#"
public class {}_compile {{
    public static void main(String[] args) {{
        Test.main();
    }}
}}
{}"#, name, indented_source),
    );
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

method_test!(switch_1, r#"
    int i = 5;
    int cnt = 0;
    switch (i) {
        case 0: cnt++;
        case 1: cnt *= 2;
        case 2: cnt ^= 1;
        case 3: cnt <<= 1;
        case 4: cnt *= cnt;
    }
    System.out.println(cnt);
"#);

method_test!(switch_2, r#"
    int cnt = 0;
    while (cnt++ < 50) {
        switch (cnt % 5) {
            case 0: cnt++;
            case 1: break;
            case 2: cnt |= 1;
            case 3: continue;
            case 4: cnt *= cnt;
        }
    }
    System.out.println(cnt);
"#);

method_test!(block_1, r#"
    {
        int x = 0;
        x += 1;
        System.out.println(x);
    }
    {
        int x = 0;
        x += 1;
        System.out.println(x);
    }
"#);

// method_test!(switch_3, r#"
//     int cnt = 0;
//     int i = 0;
//     while (cnt++ < 50) {
//         switch (cnt % 5) {
//             case 0: do { i++; cnt++; } while (i < 5);
//             case 1: do { i++; cnt++; } while (i < 10);
//             case 2: do { i++; cnt++; } while (i < 20);
//             case 3: do { i++; cnt++; } while (i < 25);
//             case 4: do { i++; cnt++; } while (i < 30);
//         }
//     }
//     System.out.println(cnt);
// "#);

classes_test!(obj1, r#"
    class Point {
        int x;
        int y;
        Point(int _x, int _y) {
            x = _x;
            y = _y;
        }
    }
    class Test {
        public static void main() {
            int x = 0, y = 2;
            Point p = new Point(x, y);
            p.x = x;
            p.y = y;
            System.out.printf("%d %d\n", p.x, p.y);
        }
    }
"#);
