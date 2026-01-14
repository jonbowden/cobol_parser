use cobol_parser::preprocessor::preprocess;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/GHOINSSTPL.cob").unwrap();
    let result = preprocess(&source);
    
    println!("=== First 15 lines after preprocessing ===");
    for (i, line) in result.source.lines().take(15).enumerate() {
        println!("Line {:2}: {:?}", i+1, line);
    }
    
    println!("\n=== Fixes applied ({} total) ===", result.fixes.len());
    for fix in result.fixes.iter().take(10) {
        println!("  Line {}: {:?} - {}", fix.line, fix.fix_type, fix.description);
    }
    if result.fixes.len() > 10 {
        println!("  ... and {} more fixes", result.fixes.len() - 10);
    }
}
