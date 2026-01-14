use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = args.get(1).map(|s| s.as_str()).unwrap_or("sources/pgm/TRFVTF1B.cob");

    let source = std::fs::read_to_string(filename).unwrap();
    let normalized = normalize_cobol_source(&source);

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    let tree = parser.parse(&normalized, None).unwrap();
    let root = tree.root_node();

    let lines: Vec<&str> = normalized.lines().collect();

    println!("=== {} ({} lines) ===\n", filename, lines.len());

    fn walk_errors(node: &tree_sitter::Node, normalized: &str, lines: &[&str]) {
        if node.is_error() || node.is_missing() {
            let line_num = node.start_position().row;
            let kind = if node.is_error() { "ERROR" } else { "MISSING" };
            let node_text: String = node.utf8_text(normalized.as_bytes())
                .unwrap_or("")
                .chars()
                .take(60)
                .collect();

            println!("{} at line {}: {:?}", kind, line_num + 1, node_text);
            println!("Context:");
            let start = line_num.saturating_sub(3);
            let end = (line_num + 4).min(lines.len());
            for i in start..end {
                let marker = if i == line_num { ">>>" } else { "   " };
                println!("{} {:4}: |{}|", marker, i + 1, lines.get(i).unwrap_or(&"<missing>"));
            }
            println!();
        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                walk_errors(&child, normalized, lines);
            }
        }
    }

    walk_errors(&root, &normalized, &lines);
}
