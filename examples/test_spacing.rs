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
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    // Test different numbers of spaces after PATH-P1
    for spaces in [1, 2, 5, 10, 15, 20, 25, 28, 30] {
        let sp = " ".repeat(spaces);
        let source = format!(r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1{}PIC X(20) VALUE "NXYXXXXNNYXXXXXXXXX".
"#, sp);

        let line_len = format!("       01  PATH-P1{}PIC X(20) VALUE \"NXYXXXXNNYXXXXXXXXX\".", sp).len();

        if let Some(tree) = parser.parse(&source, None) {
            let errors = count_errors(&tree.root_node());
            let status = if errors > 0 { "FAIL" } else { "OK" };
            println!("{} spaces (line len {}): {} (errors: {})", spaces, line_len, status, errors);
        }
    }
}
