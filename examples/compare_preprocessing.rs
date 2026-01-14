use cobol_parser::preprocessor::preprocess;
use cobol_parser::{parse_source, ParserType, ParserConfig};
use std::fs;

fn main() {
    let config = ParserConfig::default();
    
    println!("{:<20} {:>10} {:>10} {:>10}", "File", "Before", "After", "Diff");
    println!("{}", "-".repeat(55));
    
    let mut total_before = 0;
    let mut total_after = 0;
    
    for entry in fs::read_dir("sources/pgm").unwrap() {
        let path = entry.unwrap().path();
        if path.extension().map_or(false, |e| e == "cob") {
            let filename = path.file_name().unwrap().to_str().unwrap();
            let source = fs::read_to_string(&path).unwrap();
            
            // Parse without preprocessing
            let before = parse_source(&source, filename, ParserType::TreeSitter, &config)
                .map(|r| r.errors.len())
                .unwrap_or(999);
            
            // Parse with preprocessing
            let preprocessed = preprocess(&source);
            let after = parse_source(&preprocessed.source, filename, ParserType::TreeSitter, &config)
                .map(|r| r.errors.len())
                .unwrap_or(999);
            
            let diff = after as i32 - before as i32;
            let diff_str = if diff < 0 { format!("{}", diff) } else if diff > 0 { format!("+{}", diff) } else { "=".to_string() };
            
            println!("{:<20} {:>10} {:>10} {:>10}", filename, before, after, diff_str);
            
            total_before += before;
            total_after += after;
        }
    }
    
    println!("{}", "-".repeat(55));
    let total_diff = total_after as i32 - total_before as i32;
    println!("{:<20} {:>10} {:>10} {:>10}", "TOTAL", total_before, total_after, 
             if total_diff < 0 { format!("{}", total_diff) } else { format!("+{}", total_diff) });
}
