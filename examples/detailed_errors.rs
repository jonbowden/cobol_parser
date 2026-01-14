use cobol_parser::preprocessor::preprocess;
use cobol_parser::{parse_source, ParserType, ParserConfig};

fn main() {
    for entry in std::fs::read_dir("sources/pgm").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().map_or(false, |e| e == "cob") {
            let source = std::fs::read_to_string(&path).unwrap();
            let preprocessed = preprocess(&source);
            let config = ParserConfig::default();

            if let Ok(result) = parse_source(&preprocessed.source, path.file_name().unwrap().to_str().unwrap(), ParserType::TreeSitter, &config) {
                if !result.errors.is_empty() {
                    println!("\n============================================================");
                    println!("FILE: {}", path.file_name().unwrap().to_str().unwrap());
                    println!("============================================================");

                    let lines: Vec<&str> = preprocessed.source.lines().collect();

                    for err in &result.errors {
                        if let Some(line_num) = err.line {
                            println!("\n--- Error at line {} ---", line_num);
                            // Show context: 2 lines before and after
                            let start = if line_num > 3 { line_num - 3 } else { 0 };
                            let end = (line_num + 2).min(lines.len());
                            for i in start..end {
                                let marker = if i + 1 == line_num { ">>>" } else { "   " };
                                println!("{} {:4}: {}", marker, i + 1, lines.get(i).unwrap_or(&""));
                            }
                        }
                    }
                }
            }
        }
    }
}
