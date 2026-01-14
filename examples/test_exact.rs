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
    // Exact formatting from TRFVTD1
    let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT TFSSTPL ASSIGN TO TFSSTPL
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
       DATA DIVISION.
       PROCEDURE DIVISION.
           STOP RUN.
"#;
    let normalized = normalize_cobol_source(source);
    
    println!("Normalized:");
    for (i, line) in normalized.lines().enumerate() {
        println!("{:3}: {}", i+1, line);
    }
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        let errors = count_errors(&tree.root_node());
        println!("\nErrors: {}", errors);
    }
}
