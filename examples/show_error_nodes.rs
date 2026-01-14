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

    println!("=== {} ===", filename);
    println!("Total nodes with errors:");

    let lines: Vec<&str> = normalized.lines().collect();

    fn walk_errors(node: &tree_sitter::Node, lines: &[&str], depth: usize) {
        if node.is_error() || node.is_missing() {
            let line_num = node.start_position().row;
            let line_text = lines.get(line_num).unwrap_or(&"<no line>");
            let kind = if node.is_error() { "ERROR" } else { "MISSING" };
            let node_text: String = node.utf8_text(lines.join("\n").as_bytes())
                .unwrap_or("")
                .chars()
                .take(50)
                .collect();
            println!("\n{} at line {}: {:?}", kind, line_num + 1, node_text);
            println!("  Full line: |{}|", line_text);
            println!("  Parent: {}", node.parent().map(|p| p.kind()).unwrap_or("none"));
        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                walk_errors(&child, lines, depth + 1);
            }
        }
    }

    walk_errors(&root, &lines, 0);

    // Count total errors
    fn count_errors(node: &tree_sitter::Node) -> usize {
        let mut count = if node.is_error() || node.is_missing() { 1 } else { 0 };
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                count += count_errors(&child);
            }
        }
        count
    }

    println!("\n\nTotal error/missing nodes: {}", count_errors(&root));
}
