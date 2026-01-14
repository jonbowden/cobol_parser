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
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();

    // Take first 192 lines (just before SELECT)
    let partial: String = source.lines().take(192).collect::<Vec<_>>().join("\n");
    let normalized = normalize_cobol_source(&partial);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        let errors = count_errors(&tree.root_node());
        println!("Lines 1-192: {} errors", errors);
        
        if errors > 0 {
            fn find_error(node: &tree_sitter::Node, source: &str) {
                if node.is_error() || node.is_missing() {
                    let start = node.start_position();
                    let text = &source[node.start_byte()..node.end_byte().min(node.start_byte() + 80)];
                    println!("Error at line {}: [{}]", start.row + 1, text.replace('\n', "\\n"));
                }
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        find_error(&child, source);
                    }
                }
            }
            find_error(&tree.root_node(), &normalized);
        }
    }
}
