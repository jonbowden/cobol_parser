use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1B.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    let lines: Vec<&str> = normalized.lines().collect();

    // Extract context around line 479 (0-indexed = 478)
    let start = 468;
    let end = 485;

    println!("Lines {}..{}:", start + 1, end);
    for i in start..end.min(lines.len()) {
        let marker = if i + 1 == 479 { ">>>" } else { "   " };
        println!("{} {}: |{}|", marker, i + 1, lines[i]);
    }

    println!("\nParsing extract...");

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    // Parse just the extracted portion as if it were a complete program
    // This won't work, we need to parse in context

    // Instead, let's look at the tree for the whole file
    if let Some(tree) = parser.parse(&normalized, None) {
        let root = tree.root_node();

        // Find error nodes
        fn find_errors(node: &tree_sitter::Node, source: &str, lines: &[&str]) {
            if node.is_error() || node.is_missing() {
                let line = node.start_position().row;
                let col = node.start_position().column;
                let kind = if node.is_error() { "ERROR" } else { "MISSING" };
                let text: String = node.utf8_text(source.as_bytes())
                    .unwrap_or("")
                    .lines()
                    .next()
                    .unwrap_or("")
                    .chars()
                    .take(60)
                    .collect();

                // Only show errors near line 479
                if line >= 470 && line <= 490 {
                    println!("{} at line {}, col {}: {:?}", kind, line + 1, col, text);
                    println!("  Parent: {:?}", node.parent().map(|p| p.kind()));
                    println!("  Line content: |{}|", lines.get(line).unwrap_or(&""));
                }
            }

            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    find_errors(&child, source, lines);
                }
            }
        }

        find_errors(&root, &normalized, &lines);
    }
}
