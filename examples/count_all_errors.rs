use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;
use std::fs;

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
    let files = fs::read_dir("sources/pgm")
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "cob"));

    let mut total_errors = 0;
    let mut files_with_errors = 0;
    let mut clean_files = 0;

    for file in files {
        let path = file.path();
        let source = fs::read_to_string(&path).unwrap();
        let normalized = normalize_cobol_source(&source);

        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_cobol::language()).unwrap();

        if let Some(tree) = parser.parse(&normalized, None) {
            let errors = count_errors(&tree.root_node());
            if errors > 0 {
                println!("{}: {} errors", path.file_name().unwrap().to_string_lossy(), errors);
                total_errors += errors;
                files_with_errors += 1;
            } else {
                clean_files += 1;
            }
        }
    }

    println!("\n=== Summary ===");
    println!("Clean files: {}", clean_files);
    println!("Files with errors: {}", files_with_errors);
    println!("Total error nodes: {}", total_errors);
}
