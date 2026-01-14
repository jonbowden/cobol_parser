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
    // Test with SPECIAL-NAMES
    let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA
              I-O-FEEDBACK IS I-O-FEEDBACK-AREA
              UPSI-0 IS UPSI-SWITCH-0
              ON STATUS IS U0-ON
              OFF STATUS IS U0-OFF.
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
