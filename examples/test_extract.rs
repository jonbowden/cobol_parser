use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1B.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    let lines: Vec<&str> = normalized.lines().collect();

    // Create a minimal test program with the problematic section
    // Lines 348-500 from the file (working storage)
    let mut test_source = String::new();
    test_source.push_str("       IDENTIFICATION DIVISION.\n");
    test_source.push_str("       PROGRAM-ID. TEST.\n");
    test_source.push_str("       DATA DIVISION.\n");

    // Add lines around the problematic area (473-485 from original)
    test_source.push_str("       WORKING-STORAGE SECTION.\n");
    for i in 472..485.min(lines.len()) {
        test_source.push_str(lines[i]);
        test_source.push('\n');
    }

    println!("Test source ({} chars):\n---", test_source.len());
    for (i, line) in test_source.lines().enumerate() {
        println!("{}: |{}|", i + 1, line);
    }
    println!("---");

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    if let Some(tree) = parser.parse(&test_source, None) {
        let root = tree.root_node();

        // Count errors
        fn count_errors(node: &tree_sitter::Node) -> usize {
            let mut count = if node.is_error() || node.is_missing() { 1 } else { 0 };
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    count += count_errors(&child);
                }
            }
            count
        }

        println!("Errors: {}", count_errors(&root));

        // Show first few errors
        fn show_errors(node: &tree_sitter::Node, source: &str, count: &mut usize) {
            if node.is_error() || node.is_missing() {
                *count += 1;
                if *count <= 5 {
                    let line = node.start_position().row;
                    let kind = if node.is_error() { "ERROR" } else { "MISSING" };
                    let text: String = node.utf8_text(source.as_bytes())
                        .unwrap_or("")
                        .lines()
                        .next()
                        .unwrap_or("")
                        .chars()
                        .take(60)
                        .collect();
                    println!("{} line {}: {:?}", kind, line + 1, text);
                }
            }

            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    show_errors(&child, source, count);
                }
            }
        }

        let mut count = 0;
        show_errors(&root, &test_source, &mut count);
    }
}
