use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1A.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        // Find first error
        fn find_first_error(node: &tree_sitter::Node, source: &str) -> Option<(usize, String)> {
            if node.is_error() || node.is_missing() {
                let start = node.start_position();
                let text = &source[node.start_byte()..node.end_byte().min(node.start_byte() + 80)];
                return Some((start.row + 1, text.to_string()));
            }
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    if let Some(err) = find_first_error(&child, source) {
                        return Some(err);
                    }
                }
            }
            None
        }
        
        if let Some((line, text)) = find_first_error(&tree.root_node(), &normalized) {
            println!("First error at line {}", line);
            println!("Text: [{}]", text.replace('\n', "\\n"));
            
            // Show context
            println!("\nContext (lines {}-{}):", line.saturating_sub(5), line + 3);
            for (i, l) in normalized.lines().enumerate() {
                let ln = i + 1;
                if ln >= line.saturating_sub(5) && ln <= line + 3 {
                    let marker = if ln == line { ">>>" } else { "   " };
                    println!("{} {:4}: {}", marker, ln, l);
                }
            }
        } else {
            println!("No errors found!");
        }
    }
}
