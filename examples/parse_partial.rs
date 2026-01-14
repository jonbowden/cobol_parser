use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn count_errors(node: &tree_sitter::Node) -> usize {
    let mut count = if node.is_error() || node.is_missing() { 1 } else { 0 };
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            count += count_errors(&child);
        }
    }
    count
}

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1A.cob").unwrap();
    
    // Take first 1050 lines
    let partial: String = source.lines().take(1050).collect::<Vec<_>>().join("\n");
    let normalized = normalize_cobol_source(&partial);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        let errors = count_errors(&tree.root_node());
        println!("First 1050 lines: {} errors", errors);
    }
    
    // Take first 1025 lines (just before the error)
    let partial2: String = source.lines().take(1025).collect::<Vec<_>>().join("\n");
    let normalized2 = normalize_cobol_source(&partial2);

    if let Some(tree) = parser.parse(&normalized2, None) {
        let errors = count_errors(&tree.root_node());
        println!("First 1025 lines: {} errors", errors);
    }
}
