use cobol_parser::parsers::normalize_cobol_source;
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
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let total_lines = source.lines().count();
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    // Binary search
    let mut low = 1;
    let mut high = total_lines;
    
    while low < high {
        let mid = (low + high) / 2;
        let partial: String = source.lines().take(mid).collect::<Vec<_>>().join("\n");
        let normalized = normalize_cobol_source(&partial);
        
        if let Some(tree) = parser.parse(&normalized, None) {
            let errors = count_errors(&tree.root_node());
            if errors > 0 {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
    }
    
    println!("First error at line: {}", low);
    
    // Show original source around that line
    let lines: Vec<&str> = source.lines().collect();
    println!("\nOriginal source:");
    for i in (low.saturating_sub(3))..=(low + 2).min(total_lines - 1) {
        let marker = if i + 1 == low { ">>>" } else { "   " };
        println!("{} {:4}: {}", marker, i + 1, lines[i]);
    }
}
