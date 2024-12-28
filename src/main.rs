mod container;
mod converter;
mod cssa;
mod directory;
mod dsu;
mod flatten;
mod graph;
mod hoist;
mod ir;
mod linker;
mod optimizer;
mod parameters;
mod printer;
mod scope;
mod ssa;
mod substitution;
mod symbolmanager;
mod tsretriever;
mod typeinfer;

use converter::convert;
use hoist::hoist;
use optimizer::optimize;
use parameters::Parameters;
use printer::str_print;
use symbolmanager::SymbolManager;
use typeinfer::typeinfer;

use clap::Parser;
use std::fs::File;
use std::io::{self, Read, Write};

fn read_file(path: &str) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

fn write_to_file(file_path: &str, content: &str) -> io::Result<()> {
    let mut file = File::create(file_path)?;
    file.write_all(content.as_bytes())?;
    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// List of files to read
    #[arg(long, required = true)]
    input_files: Vec<String>,

    /// The class which contains the entry method.
    #[arg(long, required = true)]
    entry_class: String,

    /// The name of the entry method.
    #[arg(long, required = true)]
    entry_name: String,

    /// The name of the output file.
    #[arg(long, required = true)]
    output_file: String,
}

fn main() {
    let args = Args::parse();
    let mut java_parser = tree_sitter::Parser::new();
    java_parser
        .set_language(&tree_sitter_java::language())
        .expect("Error loading Java grammar");

    let mut fused_ast: Option<ir::Tree> = None;
    let mut sm = SymbolManager::new();
    let params = Parameters {
        entry_class: args.entry_class,
        entry_name: args.entry_name,
    };
    for file in args.input_files {
        match read_file(&file) {
            Ok(content) => {
                let tree = java_parser
                    .parse(&content, None)
                    .unwrap_or_else(|| panic!("Failed to parse {}", file));
                let ast = convert(tree.root_node(), content.as_bytes(), &params, &mut sm);
                fused_ast = Some(if let Some(existing_ast) = fused_ast {
                    linker::link(existing_ast, *ast)
                } else {
                    *ast
                });
            }
            Err(e) => eprintln!("Error reading file {}: {}", file, e),
        }
    }

    let mut ast = Box::new(fused_ast.expect("No files provided!"));
    // As a general note, things should prefer to consume the tree, and allow
    // The user to clone them if necessary.
    ast = hoist(ast.as_ref(), &mut sm);
    ast = optimize(ast.as_ref(), &mut sm);
    typeinfer(ast.as_mut(), &mut sm);
    let content = str_print(ast.as_ref(), &sm);
    write_to_file(&args.output_file, &content).unwrap();
}
