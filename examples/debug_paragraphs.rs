use cobol_parser::parsers::normalize_cobol_source;
use tree_sitter::Parser;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();
    
    let tree = parser.parse(&normalized, None).unwrap();
    let root = tree.root_node();
    
    fn walk(node: tree_sitter::Node, source: &str, depth: usize) {
        let kind = node.kind();
        
        if kind == "paragraph_header" || kind == "section_header" || kind == "label" {
            let text = node.utf8_text(source.as_bytes()).unwrap_or("");
            let preview: String = text.chars().take(60).collect();
            println!("{}{}: {:?}", "  ".repeat(depth), kind, preview);
            
            // Show children
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    let child_text = child.utf8_text(source.as_bytes()).unwrap_or("");
                    let child_preview: String = child_text.chars().take(40).collect();
                    println!("{}  child[{}] {}: {:?}", "  ".repeat(depth), i, child.kind(), child_preview);
                }
            }
        }
        
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                walk(child, source, depth + 1);
            }
        }
    }
    
    walk(root, &normalized, 0);
}
