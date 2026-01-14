use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;
use std::fs;

fn main() {
    let files = fs::read_dir("sources/pgm")
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "cob"));

    for file in files {
        let path = file.path();
        let source = fs::read_to_string(&path).unwrap();
        let normalized = normalize_cobol_source(&source);
        let lines: Vec<&str> = normalized.lines().collect();

        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_cobol::language()).unwrap();

        if let Some(tree) = parser.parse(&normalized, None) {
            let root = tree.root_node();
            let mut has_errors = false;

            fn walk_errors(node: &tree_sitter::Node, normalized: &str, lines: &[&str], filename: &str, has_errors: &mut bool) {
                if node.is_error() || node.is_missing() {
                    if !*has_errors {
                        println!("\n=== {} ===", filename);
                        *has_errors = true;
                    }
                    let line_num = node.start_position().row;
                    let kind = if node.is_error() { "ERROR" } else { "MISSING" };
                    let node_text: String = node.utf8_text(normalized.as_bytes())
                        .unwrap_or("")
                        .chars()
                        .take(60)
                        .collect();
                    println!("{} line {}: {:?}", kind, line_num + 1, node_text);
                    if line_num < lines.len() {
                        println!("  |{}|", lines[line_num]);
                    }
                }

                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        walk_errors(&child, normalized, lines, filename, has_errors);
                    }
                }
            }

            walk_errors(&root, &normalized, &lines, path.file_name().unwrap().to_str().unwrap(), &mut has_errors);
        }
    }
}
