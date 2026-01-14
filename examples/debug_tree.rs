use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    // A minimal test case
    let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES    PIC X VALUE "X".
       01  PATH-P1                     PIC X(20) VALUE "Y".
"#;

    println!("Source:\n{}", source);
    println!("\nParsing...");

    let normalized = normalize_cobol_source(source);
    println!("Normalized:\n{}", normalized);

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&normalized, None) {
        let root = tree.root_node();
        println!("\nTree S-expression:");
        println!("{}", root.to_sexp());

        // Look for errors
        fn walk_errors(node: &tree_sitter::Node, source: &str, depth: usize) {
            let indent = "  ".repeat(depth);
            if node.is_error() || node.is_missing() {
                let kind = if node.is_error() { "ERROR" } else { "MISSING" };
                let text: String = node.utf8_text(source.as_bytes())
                    .unwrap_or("")
                    .chars()
                    .take(50)
                    .collect();
                println!("{}[{}] at {}:{} - {:?}",
                    indent, kind,
                    node.start_position().row + 1,
                    node.start_position().column,
                    text);
            }

            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    walk_errors(&child, source, depth + 1);
                }
            }
        }

        println!("\nErrors:");
        walk_errors(&root, &normalized, 0);
    }
}
