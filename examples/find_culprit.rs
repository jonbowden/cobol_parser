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
    let lines: Vec<&str> = source.lines().collect();
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    // Find minimum lines that include SELECT and still error
    // Start with known good (192) and add SELECT-related lines
    
    let test_ranges = vec![
        (1, 193),    // Up to first SELECT
        (170, 193),  // SPECIAL-NAMES + SELECT
        (172, 193),  // Just around SELECT
        (191, 193),  // Just FILE-CONTROL + SELECT
    ];
    
    for (start, end) in test_ranges {
        // Build minimal program with context
        let partial: String = lines[start-1..end].iter().map(|s| *s).collect::<Vec<_>>().join("\n");
        
        // Add minimal program structure
        let test_source = format!(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n       ENVIRONMENT DIVISION.\n{}\n       DATA DIVISION.\n       PROCEDURE DIVISION.\n           STOP RUN.",
            partial
        );
        
        let normalized = normalize_cobol_source(&test_source);
        
        if let Some(tree) = parser.parse(&normalized, None) {
            let errors = count_errors(&tree.root_node());
            println!("Lines {}-{}: {} errors", start, end, errors);
        }
    }
}
