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
    let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT TFSSTPL ASSIGN TO TFSSTPL
              ORGANIZATION IS INDEXED.
       DATA DIVISION.
       PROCEDURE DIVISION.
           STOP RUN.
"#;
    let normalized = normalize_cobol_source(source);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        let errors = count_errors(&tree.root_node());
        println!("Errors: {}", errors);
        
        if errors > 0 {
            fn find_error(node: &tree_sitter::Node, source: &str) {
                if node.is_error() || node.is_missing() {
                    let start = node.start_position();
                    let text = &source[node.start_byte()..node.end_byte().min(node.start_byte() + 60)];
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
