use cobol_parser::preprocessor::preprocess;
use cobol_parser::{parse_source, ParserType, ParserConfig};
use std::collections::HashMap;

fn main() {
    let mut error_counts: HashMap<String, usize> = HashMap::new();
    let mut total_errors = 0;
    let mut files_with_errors = 0;

    for entry in std::fs::read_dir("sources/pgm").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().map_or(false, |e| e == "cob") {
            let source = std::fs::read_to_string(&path).unwrap();
            let preprocessed = preprocess(&source);
            let config = ParserConfig::default();

            if let Ok(result) = parse_source(&preprocessed.source, path.file_name().unwrap().to_str().unwrap(), ParserType::TreeSitter, &config) {
                if !result.errors.is_empty() {
                    files_with_errors += 1;
                    println!("\n=== {} ({} errors) ===", path.file_name().unwrap().to_str().unwrap(), result.errors.len());
                    for err in &result.errors {
                        total_errors += 1;
                        // Extract key phrase from error
                        // Simpler categorization - just look at what the primary error is about
                        let upper_msg = err.message.to_uppercase();
                        let key = if upper_msg.contains("EXEC SQL") || upper_msg.contains("END-EXEC") {
                            "EXEC SQL"
                        } else if upper_msg.contains("LINKAGE SECTION") {
                            "LINKAGE SECTION"
                        } else if upper_msg.contains("EJECT") {
                            "EJECT"
                        } else {
                            "Parse error"
                        };
                        *error_counts.entry(key.to_string()).or_insert(0) += 1;
                        println!("  Line {:?}: {}", err.line, &err.message[..err.message.len().min(80)]);
                    }
                }
            }
        }
    }

    println!("\n\n========== SUMMARY ==========");
    println!("Total errors: {}", total_errors);
    println!("Files with errors: {}", files_with_errors);
    println!("\nError categories:");
    let mut counts: Vec<_> = error_counts.iter().collect();
    counts.sort_by(|a, b| b.1.cmp(a.1));
    for (category, count) in counts {
        println!("  {}: {}", category, count);
    }
}
