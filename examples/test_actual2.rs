use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1A.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    println!("Lines 1019-1035:");
    for (i, line) in normalized.lines().enumerate() {
        let ln = i + 1;
        if ln >= 1019 && ln <= 1035 {
            let chars: Vec<char> = line.chars().collect();
            let cols_1_6: String = chars.iter().take(6).collect();
            let col_7 = chars.get(6).copied().unwrap_or('?');
            let code: String = chars.iter().skip(7).collect();
            println!("{:4}: [{}][{}][{}]", ln, cols_1_6, col_7, &code[..code.len().min(50)]);
        }
    }
}
