use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1A.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        fn find_error_with_parent(node: tree_sitter::Node, source: &str, depth: usize) {
            if node.is_error() || node.is_missing() {
                let start = node.start_position();
                let text = &source[node.start_byte()..node.end_byte().min(node.start_byte() + 50)];
                println!("ERROR at line {}:{}", start.row + 1, start.column);
                println!("  Text: [{}]", text.replace('\n', "\\n"));
                
                // Show parent chain
                let mut parent = node.parent();
                let mut level = 0;
                while let Some(p) = parent {
                    println!("  Parent {}: {} (line {})", level, p.kind(), p.start_position().row + 1);
                    level += 1;
                    if level > 5 { break; }
                    parent = p.parent();
                }
                return;
            }
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    find_error_with_parent(child, source, depth + 1);
                }
            }
        }
        
        find_error_with_parent(tree.root_node(), &normalized, 0);
    }
}
