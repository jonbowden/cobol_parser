use cobol_parser::preprocessor::preprocess;
use cobol_parser::{parse_source, ParserType, ParserConfig};

fn main() {
    let mut clean = 0;
    let mut with_errors = 0;

    for entry in std::fs::read_dir("sources/pgm").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().map_or(false, |e| e == "cob") {
            let source = std::fs::read_to_string(&path).unwrap();
            let preprocessed = preprocess(&source);
            let config = ParserConfig::default();

            if let Ok(result) = parse_source(&preprocessed.source, path.file_name().unwrap().to_str().unwrap(), ParserType::TreeSitter, &config) {
                if result.errors.is_empty() {
                    clean += 1;
                    println!("CLEAN: {}", path.file_name().unwrap().to_str().unwrap());
                } else {
                    with_errors += 1;
                }
            }
        }
    }

    println!("\n=== Summary ===");
    println!("Files parsing cleanly: {}", clean);
    println!("Files with errors: {}", with_errors);
}
