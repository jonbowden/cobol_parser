use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;
use std::collections::HashSet;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();
    
    let tree = parser.parse(&normalized, None).unwrap();
    let root = tree.root_node();
    
    let mut kinds = HashSet::new();
    
    fn walk(node: tree_sitter::Node, kinds: &mut HashSet<String>) {
        kinds.insert(node.kind().to_string());
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                walk(child, kinds);
            }
        }
    }
    
    walk(root, &mut kinds);
    
    let mut kinds_vec: Vec<_> = kinds.iter().collect();
    kinds_vec.sort();
    println!("Node kinds in parse tree ({} unique):", kinds_vec.len());
    for k in &kinds_vec {
        println!("  {}", k);
    }
}
