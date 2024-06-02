use std::fs::{self, write};
use std::process::Command;
use jjb::optimizer::optimize;
use jjb::parameters::Parameters;
use jjb::printer::print;
use jjb::hoist::hoist;
use jjb::{flatten, ssa};
use jjb::{converter::convert, ir::Tree, printer::str_print, symbolmanager::SymbolManager};
use jjb::typeinfer::typeinfer;
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

struct JavaFileManager { base_dir: String }
impl JavaFileManager {
    /// Create a new JavaFileManager with the specified base directory.
    fn new(base_dir: &str) -> Self {
        JavaFileManager {
            base_dir: base_dir.to_string(),
        }
    }

    /// Writes content to a file in its specified subdirectory.
    fn stash_java(&self, fname: &str, content: &str) {
        let dir = format!("{}/{}", self.base_dir, fname);
        fs::create_dir_all(&dir).expect("Failed to create directory");
        write(&format!("{}/{}.java", dir, fname), content).expect("Write Failed!");
    }

    /// Compiles the Java file located in its respective subdirectory.
    fn compile_java(&self, fname: &str) {
        let dir = format!("{}/{}", self.base_dir, fname);
        let compile_output = Command::new("javac")
            .arg(&format!("{}.java", fname))
            .current_dir(&dir)
            .output()
            .expect("Failed to execute 'javac' command");
        let stderr = String::from_utf8_lossy(&compile_output.stderr).to_string();
        assert!(
            stderr.trim().is_empty(),
            "stderr is not empty: {}",
            stderr
        );
    }

    /// Executes the compiled Java class from its subdirectory.
    fn execute_java(&self, fname: &str) -> String {
        let dir = format!("{}/{}", self.base_dir, fname);
        let run_output = Command::new("timeout")
            .arg("1s")
            .arg("java")
            .arg(fname)
            .current_dir(&dir)
            .output()
            .expect("Failed to execute 'java' command");

        let stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&run_output.stderr).to_string();
        assert!(
            run_output.status.success(),
            "exit code is not 0: {}, stderr: {}",
            run_output.status.code().unwrap_or_default(),
            stderr
        );
        assert!(
            stderr.trim().is_empty(),
            "stderr is not empty: {}",
            stderr
        );
        stdout
    }

    fn cache(&self, fname: &str, content: &str) {
        let cache_path = format!("{}/.{}_cache", self.base_dir, fname);
        write(cache_path, content).expect("Write Failed!");
    }

    /// Executes the Java file and caches the output.
    fn execute_and_cache(&self, fname: &str) -> String {
        let res = self.execute_java(fname);
        self.cache(fname, &res);
        res
    }

    /// Reads the cached output if available.
    fn read_cache(&self, fname: &str) -> Result<String, std::io::Error> {
        let cache_path = format!("{}/.{}_cache", self.base_dir, fname);
        fs::read_to_string(cache_path)
    }

    /// Runs the process: stashes the file, compiles it, executes it, and caches the output.
    fn _run(&self, fname: &str, text: &str) -> String {
        self.stash_java(fname, text);
        self.compile_java(fname);
        self.execute_and_cache(fname)
    }

    /// Public method to run the Java file.
    fn run(&self, fname: &str, text: &str) -> String {
        let base_name = fname;
        let content_path = format!("{}/{}/{}.java", self.base_dir, base_name, fname);
        let content = fs::read_to_string(&content_path);
        if let Ok(res) = content {
            if res != text {
                return self._run(fname, text);
            } else if let Ok(cache) = self.read_cache(fname) {
                return cache;
            }
        }
        self._run(fname, text)
    }
}


fn test_equal(source: &str, compile: &str, tree: &Tree, sm: &SymbolManager, name: &str, params: &Parameters) {
    let source_path = format!("{}_source", name);
    let compile_path = format!("{}_compile", name);
    let buffer_str = str_print(tree, sm);
    let jfm = JavaFileManager::new("testfiles/");
    let res_source = jfm.run(&source_path, &source);
    let res_compiled = jfm.run(&compile_path, &buffer_str);
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
    let mut sm = SymbolManager::new();
    let class_name = format!("{}_compile", name);
    let params = Parameters { entry_class: class_name, entry_name: "main".to_string() };
    let mut ast = convert(tree.root_node(), compile.as_bytes(), &params, &mut sm);
    ast = hoist(ast.as_ref(), &mut sm);
    typeinfer(ast.as_mut(), &mut sm);
    ast = Box::new(ssa::transform(*ast, &mut sm));
    // ast = optimize(ast.as_ref(), &mut sm);
    // ast = Box::new(flatten::flatten(*ast, &mut sm));
    ast = Box::new(ssa::revert(*ast, &mut sm));
    test_equal(source, compile, &ast, &sm, name, &params);
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
        Test.start();
    }}
}}
{}"#, name, indented_source), &format!(r#"
public class {}_compile {{
    public static void main(String[] args) {{
        Test.start();
    }}
}}
{}"#, name, indented_source),
    );
}

method_test!(negative_1, r#"
    int x = 5;
    int y = -7;
    System.out.println(-x + y);
"#);

method_test!(ternary, r#"
    boolean a = false;
    System.out.println(a ? 1 : -1);
    boolean b = true;
    System.out.println(b ? 1 : -1);
"#);

method_test!(undefined, r#"
    int a, b, c, d, e, f;
    a = 2; b = 2; c = 2;
    d = 2; e = 2; f = 2;
    System.out.printf(
        "%d %d %d %d %d %d\n",
        a, b, c, d, e, f
    );
"#);

method_test!(prim_arith, r#"
    int a=1, b=-2, c=3, d=-4, e=5, f=-6, g=7, h=-8;
    for (int it = 0; it < 10; it++) {
        a = b + c;
        b = c - d;
        c = d * e;
        d = e / 3;
        e = f % 7;
        ++f;
        --g;
        h++;
        a--;
    }
    System.out.printf(
        "%d %d %d %d %d %d %d %d\n",
        a, b, c, d, e, f, g, h
    );
"#);

method_test!(prim_arith_simple, r#"
    int a=1, b=-2, c=3, d=-4, e=5;
    for (int it = 0; it < 10; it++) {
        a = b + c;
        b = c - d;
        c = d * e;
        d = e / 3;
    }
    System.out.printf(
        "%d %d %d %d\n",
        a, b, c, d
    );
"#);

method_test!(prim_incr, r#"
    int a=1, b=-2, c=3, d=-4, e=5, f=-6, g=7, h=-8;
    for (int it = 0; it < 10; it++) {
        a = b++ - ++c + d++ - e++ - f++ + f--;
        b = c++ - ++d + e++ - f++ - g++ + h--;
        c = d++ - ++e + f++ - g++ - h++ + a--;
        d = e++ - ++f + g++ - h++ - a++ + b--;
    }
    System.out.printf(
        "%d %d %d %d %d %d %d %d\n",
        a, b, c, d, e, f, g, h
    );
"#);

method_test!(prim_modifying_basic, r#"
    int a=1, b=-2, c=3;
    for (int it = 0; it < 10; it++) {
        a += b;
        b -= c;
    }
    System.out.printf(
        "%d %d %d\n", a, b, c
    );
"#);

method_test!(prim_modifying, r#"
    int a=1, b=-2, c=3, d=-4, e=5, f=-6, g=7, h=-8, i=9, j=-10, k=11;
    for (int it = 0; it < 10; it++) {
        a += b;
        b -= c;
        c *= d;
        d /= 3;
        e %= 4;
        f &= g;
        g |= h;
        h ^= i;
        i >>= j;
        j >>>= k;
        k <<= a;
    }
    System.out.printf(
        "%d %d %d %d %d %d %d %d %d %d %d\n",
        a, b, c, d, e, f, g, h, i, j, k
    );
"#);

method_test!(prim_conditionals, r#"
    boolean a=true, b=false, c=true, d=false, e=true, f=true, g=false, h=true, i=true;
    for (int it = 0; it < 10; it++) {
        a = a == b;
        b = c != d;
        c = (d ? 1 : -1) > (e ? 1 : -1);
        d = (e ? 1 : -1) >= (f ? 1 : -1);
        e = (f ? 1 : -1) <= (g ? 1 : -1);
        f = (g ? 1 : -1) < (h ? 1 : -1);
        g = h && i;
        h = i || a;
        i = !a;
        System.out.print(a ? 1 : 0);
        System.out.print(b ? 1 : 0);
        System.out.print(c ? 1 : 0);
        System.out.print(d ? 1 : 0);
        System.out.print(e ? 1 : 0);
        System.out.print(f ? 1 : 0);
        System.out.print(g ? 1 : 0);
        System.out.print(h ? 1 : 0);
        System.out.print(i ? 1 : 0);
    }
"#);

method_test!(prim_bitwise, r#"
    int a=1, b=-2, c=3, d=-4, e=5, f=-6, g=7;
    for (int it = 0; it < 10; it++) {
        a = ~b;
        b = c << 3;
        c = d >> 1;
        d = e >>> 2;
        e = f & g;
        f = g | a;
        g = a ^ b; 
    }
    System.out.printf(
        "%d %d %d %d %d %d %d\n",
        a, b, c, d, e, f, g
    );
"#);

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

method_test!(while_loop_2, r#"
    int i = 0, j = 0, k = 0;
    while (i++ < 10) {
        while (j++ < 10) {
            while (k++ < 10) {

            }
            if ((j ^ k) < (i ^ k)) break;
        }
        if ((j ^ i) < (k ^ j)) break;
    }
    System.out.printf(
        "%d %d %d",
        i, j, k
    );
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

method_test!(if_no_else, r#"
    int i = 10;
    if (i < 12) {
        System.out.print("0");
    }
    System.out.print("1");
    if (i > 12) {
        System.out.print("2");
    }
    System.out.print("3");
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

method_test!(label_loop, r#"
    int count = 0;
    label: while (++count < 30) {
        count += 1;
        if (count % 2 != 1) { continue label; }
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

method_test!(switch_3, r#"
    int cnt = 0;
    while (cnt++ < 50) {
        switch (cnt % 5) {
            case 0: do { cnt++; } while (cnt < 10);
            case 1: break;
            case 2: do { cnt++; } while (cnt < 1);;
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

method_test!(array_1, r#"
    int[] vars = new int[10];
    for (int i = 0; i < 10; i++) {
        System.out.print(vars[i]);
    }

    int[] array = new int[] { 0, 1, 2, 3, 4, 5 };
    for (int i = 0; i < 6; i++) {
        System.out.print(array[i]);
    }
"#);

method_test!(array_2, r#"
    int[][][][] a = new int[10][100][][];
    int[][] b = new int[][] { 
        { 0, 1, 2 },
        new int[5],
        { 5, 6, 8 }
     };
    int c[][], d[];
"#);

classes_test!(obj1, r#"
    class Point {
        int x;
        int y;
        Point(int _x, int _y) {
            x = _x;
            y = _y;
        }

        public int getx() {
            return x;
        }

        public int gety() {
            return y;
        }
    }
    class Test {
        public static void start() {
            int x = 0, y = 2;
            Point p = new Point(x, y);
            p.x = 4;
            p.y = 5;
            System.out.printf("%d %d\n", p.getx(), p.gety());
        }
    }
"#);

classes_test!(obj2, r#"
    class Item {
        int a;
        Item(int _a) {
            a = _a;
        }
    }
    class Point {
        int x;
        int y;
        Item c;
        Point(int _x, int _y) {
            x = _x;
            y = _y;
            c = new Item(10);
        }

        public int getx() {
            return x;
        }

        public int gety() {
            return y;
        }
    }
    class Test {
        public static void start() {
            int x = 0, y = 2;
            Point p = new Point(x, y);
            System.out.printf("%d\n", p.c.a);
            p.c.a = 20;
            System.out.printf("%d\n", p.c.a);
        }
    }
"#);
