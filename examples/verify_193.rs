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

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    for end_line in [192, 193, 194, 195, 200, 210, 220] {
        let partial: String = source.lines().take(end_line).collect::<Vec<_>>().join("\n");
        let normalized = normalize_cobol_source(&partial);
        
        if let Some(tree) = parser.parse(&normalized, None) {
            let errors = count_errors(&tree.root_node());
            println!("Lines 1-{}: {} errors", end_line, errors);
        }
    }
}
