use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    println!("Lines 210-220:");
    for (i, line) in normalized.lines().enumerate() {
        let ln = i + 1;
        if ln >= 210 && ln <= 220 {
            println!("{:4}: {}", ln, line);
        }
    }
}
