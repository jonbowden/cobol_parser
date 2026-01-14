use cobol_parser::preprocessor::preprocess;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1B.cob").unwrap();
    let result = preprocess(&source);
    
    println!("Fixes applied to TRFVTF1B.cob: {}", result.fixes.len());
    for fix in &result.fixes {
        println!("  Line {}: {:?} - {}", fix.line, fix.fix_type, fix.description);
    }
}
