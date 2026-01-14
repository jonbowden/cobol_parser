use cobol_parser::preprocessor::preprocess;
use cobol_parser::{parse_source, ParserType, ParserConfig};

fn main() {
    // Test GHOINSSTPL.cob which had sequence number issues
    let source = std::fs::read_to_string("sources/pgm/GHOINSSTPL.cob").unwrap();
    let preprocessed = preprocess(&source);
    
    println!("=== Preprocessing Stats ===");
    println!("Fixes applied: {}", preprocessed.fixes.len());
    
    // Parse the preprocessed source
    let config = ParserConfig::default();
    let result = parse_source(&preprocessed.source, "GHOINSSTPL.cob", ParserType::TreeSitter, &config);
    
    match result {
        Ok(r) => {
            println!("\n=== Parse Results ===");
            println!("Program ID: {}", r.program.base.name);
            println!("Procedures: {}", r.procedures.len());
            println!("Errors: {}", r.errors.len());
            for err in &r.errors {
                println!("  - Line {:?}: {}", err.line, &err.message[..err.message.len().min(60)]);
            }
            println!("Warnings: {}", r.warnings.len());
        }
        Err(e) => {
            println!("Parse failed: {:?}", e);
        }
    }
}
