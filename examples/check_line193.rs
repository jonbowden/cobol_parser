use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    println!("Lines 191-198:");
    for (i, line) in normalized.lines().enumerate() {
        let ln = i + 1;
        if ln >= 191 && ln <= 198 {
            println!("{:4}: |{}|", ln, line);
        }
    }
}
