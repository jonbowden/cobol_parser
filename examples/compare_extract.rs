use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;
use std::collections::HashSet;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = args.get(1).unwrap_or(&"sources/pgm/TRFVTD1.cob".to_string()).clone();

    let source = std::fs::read_to_string(&path).expect("Failed to read file");
    let normalized = normalize_cobol_source(&source);

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    let tree = parser.parse(&normalized, None).unwrap();
    let root = tree.root_node();

    let mut calls = HashSet::new();
    let mut paragraphs = HashSet::new();
    let mut sections = HashSet::new();
    let mut copybooks = HashSet::new();
    let mut files = HashSet::new();
    let mut errors = 0;

    fn walk(node: tree_sitter::Node, source: &str,
            calls: &mut HashSet<String>,
            paragraphs: &mut HashSet<String>,
            sections: &mut HashSet<String>,
            copybooks: &mut HashSet<String>,
            files: &mut HashSet<String>,
            errors: &mut i32) {

        let kind = node.kind();

        if node.is_error() || node.is_missing() {
            *errors += 1;
        }

        match kind {
            "call_statement" => {
                // Extract called program name from string child
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        if child.kind() == "string" {
                            if let Ok(text) = child.utf8_text(source.as_bytes()) {
                                let cleaned: String = text.chars()
                                    .filter(|c| c.is_alphanumeric() || *c == '-' || *c == '_')
                                    .collect();
                                if !cleaned.is_empty() {
                                    calls.insert(cleaned.to_uppercase());
                                }
                            }
                        }
                    }
                }
            }
            "paragraph_header" => {
                // Paragraph name is in the node text itself (e.g., "A100-MOVE-TAG-VALUES.")
                if let Ok(text) = node.utf8_text(source.as_bytes()) {
                    let cleaned = text.trim().replace(".", "").to_uppercase();
                    // Filter out very short names or "MAIN-MODULE" type entries
                    if !cleaned.is_empty() && cleaned.len() > 3 {
                        paragraphs.insert(cleaned);
                    }
                }
            }
            "section_header" => {
                // Section name is in the node text itself
                if let Ok(text) = node.utf8_text(source.as_bytes()) {
                    let cleaned = text.trim().replace(".", "").replace(" SECTION", "").to_uppercase();
                    if !cleaned.is_empty() {
                        sections.insert(cleaned);
                    }
                }
            }
            "copy_statement" => {
                if let Ok(text) = node.utf8_text(source.as_bytes()) {
                    // Parse "COPY name" or "COPY name OF lib"
                    let upper = text.to_uppercase();
                    let parts: Vec<&str> = upper.split_whitespace().collect();
                    if parts.len() >= 2 {
                        let name = parts[1].trim_matches(|c| c == '.' || c == '\'' || c == '"');
                        copybooks.insert(name.to_string());
                    }
                }
            }
            "select_statement" => {
                if let Ok(text) = node.utf8_text(source.as_bytes()) {
                    let upper = text.to_uppercase();
                    let parts: Vec<&str> = upper.split_whitespace().collect();
                    if parts.len() >= 2 && parts[0] == "SELECT" {
                        files.insert(parts[1].to_string());
                    }
                }
            }
            _ => {}
        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                walk(child, source, calls, paragraphs, sections, copybooks, files, errors);
            }
        }
    }

    walk(root, &normalized, &mut calls, &mut paragraphs, &mut sections, &mut copybooks, &mut files, &mut errors);

    println!("=== cobol_parser (tree-sitter) Output ===");
    println!("Parse Errors: {}", errors);

    println!("\nFiles: {}", files.len());
    let mut files_vec: Vec<_> = files.iter().collect();
    files_vec.sort();
    for f in &files_vec {
        println!("  - {}", f);
    }

    println!("\nCalls (unique): {}", calls.len());
    let mut calls_vec: Vec<_> = calls.iter().collect();
    calls_vec.sort();
    for c in &calls_vec {
        println!("  - {}", c);
    }

    println!("\nParagraphs: {}", paragraphs.len());
    let mut para_vec: Vec<_> = paragraphs.iter().collect();
    para_vec.sort();
    for p in &para_vec {
        println!("  - {}", p);
    }

    println!("\nSections: {}", sections.len());
    let mut sec_vec: Vec<_> = sections.iter().collect();
    sec_vec.sort();
    for s in &sec_vec {
        println!("  - {}", s);
    }

    println!("\nCopybooks: {}", copybooks.len());
    let mut cb_vec: Vec<_> = copybooks.iter().collect();
    cb_vec.sort();
    for c in &cb_vec {
        println!("  - {}", c);
    }
}
