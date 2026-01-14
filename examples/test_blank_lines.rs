use tree_sitter::Parser;

fn main() {
    // Test with blank lines
    let source_with_blanks = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  PATH-P1                            PIC X(20) VALUE "Y".
"#;

    // Test without blank lines
    let source_no_blanks = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".
       01  PATH-P1                            PIC X(20) VALUE "Y".
"#;

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    fn count_errors(node: &tree_sitter::Node) -> usize {
        let mut count = if node.is_error() || node.is_missing() { 1 } else { 0 };
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                count += count_errors(&child);
            }
        }
        count
    }

    println!("With blank lines:");
    if let Some(tree) = parser.parse(source_with_blanks, None) {
        println!("  Errors: {}", count_errors(&tree.root_node()));
    }

    println!("Without blank lines:");
    if let Some(tree) = parser.parse(source_no_blanks, None) {
        println!("  Errors: {}", count_errors(&tree.root_node()));
    }
}
