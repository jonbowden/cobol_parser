use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1A.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        // Find ALL errors and sort by line
        fn collect_errors(node: &tree_sitter::Node, source: &str, errors: &mut Vec<(usize, usize, String)>) {
            if node.is_error() || node.is_missing() {
                let start = node.start_position();
                let text = source[node.start_byte()..node.end_byte().min(node.start_byte() + 40)]
                    .replace('\n', "\\n");
                errors.push((start.row + 1, start.column, text));
            }
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    collect_errors(&child, source, errors);
                }
            }
        }
        
        let mut errors = Vec::new();
        collect_errors(&tree.root_node(), &normalized, &mut errors);
        errors.sort_by_key(|(line, col, _)| (*line, *col));
        
        println!("Total errors: {}", errors.len());
        println!("\nFirst 5 errors:");
        for (i, (line, col, text)) in errors.iter().take(5).enumerate() {
            println!("{}. Line {}:{} - [{}]", i+1, line, col, text);
        }
    }
}
