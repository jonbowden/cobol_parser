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
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TAG57-BIC PIC X(10).
       01 TFSSTPL-SWFTMGTY PIC X(3).
       01 WS-ACBNKID PIC X(10).
       PROCEDURE DIVISION.
       A159-INITIALIZE-SGX-FLDS-EX.
             EXIT.

       A200-MOVE-TAG-VALUES.
            IF  TAG57-BIC IS NOT = SPACES
            AND TFSSTPL-SWFTMGTY = "200"
                MOVE TAG57-BIC        TO     WS-ACBNKID
            END-IF.
"#;
    let normalized = normalize_cobol_source(source);
    
    println!("Normalized source:");
    for (i, line) in normalized.lines().enumerate() {
        println!("{:3}: |{}|", i+1, line);
    }
    println!();

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        let errors = count_errors(&tree.root_node());
        println!("Error count: {}", errors);

        // Find error nodes
        fn find_errors(node: &tree_sitter::Node, source: &str) {
            if node.is_error() || node.is_missing() {
                let start = node.start_position();
                let end = node.end_position();
                let text = &source[node.start_byte()..node.end_byte().min(node.start_byte() + 50)];
                println!("ERROR at line {}: kind={} text=[{}]", start.row + 1, node.kind(), text);
            }
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    find_errors(&child, source);
                }
            }
        }
        find_errors(&tree.root_node(), &normalized);
    }
}
