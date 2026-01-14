use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    // Check TRFVTE1.cob lines around 835 (split AND)
    println!("=== TRFVTE1.cob lines 833-837 ===");
    let source = std::fs::read_to_string("sources/pgm/TRFVTE1.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    let lines: Vec<&str> = normalized.lines().collect();
    for i in 832..837.min(lines.len()) {
        println!("{:4}: |{}|", i + 1, lines.get(i).unwrap_or(&""));
    }

    // Check TRFVTE1.cob lines around 1327 (499 - continuation)
    println!("\n=== TRFVTE1.cob lines 1325-1330 ===");
    for i in 1324..1330.min(lines.len()) {
        println!("{:4}: |{}|", i + 1, lines.get(i).unwrap_or(&""));
    }

    // Check TRFVTF1B.cob lines 537-541
    println!("\n=== TRFVTF1B.cob lines 537-541 ===");
    let source2 = std::fs::read_to_string("sources/pgm/TRFVTF1B.cob").unwrap();
    let normalized2 = normalize_cobol_source(&source2);
    let lines2: Vec<&str> = normalized2.lines().collect();
    for i in 536..541.min(lines2.len()) {
        println!("{:4}: |{}|", i + 1, lines2.get(i).unwrap_or(&""));
    }

    // Check TRFVTD2.cob lines 143-147
    println!("\n=== TRFVTD2.cob lines 143-147 ===");
    let source3 = std::fs::read_to_string("sources/pgm/TRFVTD2.cob").unwrap();
    let normalized3 = normalize_cobol_source(&source3);
    let lines3: Vec<&str> = normalized3.lines().collect();
    for i in 142..147.min(lines3.len()) {
        println!("{:4}: |{}|", i + 1, lines3.get(i).unwrap_or(&""));
    }

    // Check TRFVTB3.cob lines 328-333 (pipe character issue)
    println!("\n=== TRFVTB3.cob lines 328-333 ===");
    let source4 = std::fs::read_to_string("sources/pgm/TRFVTB3.cob").unwrap();
    let normalized4 = normalize_cobol_source(&source4);
    let lines4: Vec<&str> = normalized4.lines().collect();
    for i in 327..333.min(lines4.len()) {
        println!("{:4}: |{}|", i + 1, lines4.get(i).unwrap_or(&""));
    }
}
