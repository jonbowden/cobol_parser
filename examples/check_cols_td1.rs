use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    println!("Column analysis for lines 191-200:");
    for (i, line) in normalized.lines().enumerate() {
        let ln = i + 1;
        if ln >= 191 && ln <= 200 {
            let chars: Vec<char> = line.chars().collect();
            let col7 = chars.get(6).copied().unwrap_or(' ');
            let code_start = chars.iter().skip(7).position(|&c| c != ' ').unwrap_or(0);
            println!("{:4}: col7=[{}] code_col={} |{}|", ln, col7, code_start + 8, &line[7..line.len().min(60)]);
        }
    }
}
