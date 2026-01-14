use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    // TRFVTF1B.cob line 539
    println!("=== TRFVTF1B.cob lines 537-541 ===");
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1B.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    for (i, line) in normalized.lines().enumerate() {
        if i >= 536 && i < 541 {
            println!("{:4}: |{}|", i + 1, line);
        }
    }

    // TRFXPARA.cob line 52
    println!("\n=== TRFXPARA.cob lines 50-55 ===");
    let source2 = std::fs::read_to_string("sources/pgm/TRFXPARA.cob").unwrap();
    let normalized2 = normalize_cobol_source(&source2);
    for (i, line) in normalized2.lines().enumerate() {
        if i >= 49 && i < 55 {
            println!("{:4}: |{}|", i + 1, line);
        }
    }

    // TRFVTD1.cob line 216
    println!("\n=== TRFVTD1.cob lines 214-218 ===");
    let source3 = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let normalized3 = normalize_cobol_source(&source3);
    for (i, line) in normalized3.lines().enumerate() {
        if i >= 213 && i < 218 {
            println!("{:4}: |{}|", i + 1, line);
        }
    }
}
