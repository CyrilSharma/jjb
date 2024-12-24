use crate::ir;
pub fn link(base: ir::Tree, extention: ir::Tree) -> ir::Tree {
    if let (ir::Tree::Program(base_stmts), ir::Tree::Program(extention_stmts)) = (base, extention) {
        let (mut based, mut basec) = declaration_split(base_stmts);
        let (extentiond, extentionc) = declaration_split(extention_stmts);
        based.append(extentiond);
        basec.append(extentionc);
        based.append(basec);
        ir::Tree::Program(based)
    } else {
        panic!("Linker expects two Tree::Programs as input")
    }
}


fn declaration_split(input: ir::TreeContainer) -> (ir::TreeContainer, ir::TreeContainer) {
    let mut declarations = ir::TreeContainer::new();
    let mut content = ir::TreeContainer::new();
    for item in input {
        if matches!(item, ir::Tree::LetI(_)) {
            declarations.push_back(item)
        } else {
            content.push_back(item);
        }
    }
    return ( declarations, content )
}