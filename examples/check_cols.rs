use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1A.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    
    println!("Checking column structure around line 1027-1028:");
    for (i, line) in normalized.lines().enumerate() {
        let ln = i + 1;
        if ln >= 1025 && ln <= 1030 {
            println!("Line {}: len={}", ln, line.len());
            print!("  Chars: ");
            for (j, c) in line.chars().enumerate() {
                if j < 15 {
                    if c == ' ' {
                        print!("_");
                    } else {
                        print!("{}", c);
                    }
                }
            }
            println!("...");
            println!("  Col 1-6: [{}]", &line[..6.min(line.len())]);
            println!("  Col 7: [{}]", if line.len() > 6 { &line[6..7] } else { "?" });
            println!("  Code: [{}]", if line.len() > 7 { &line[7..40.min(line.len())] } else { "" });
        }
    }
}
