use cobol_parser::preprocessor::preprocess;
use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTF1B.cob").unwrap();
    let preprocessed = preprocess(&source);
    let normalized = normalize_cobol_source(&preprocessed.source);

    let lines: Vec<&str> = normalized.lines().collect();

    println!("=== Lines 535-545 of TRFVTF1B.cob after full normalization ===");
    for i in 534..545.min(lines.len()) {
        println!("{:4}: |{}|", i + 1, lines.get(i).unwrap_or(&""));
    }

    // Check TRFXPARA.cob line 75
    println!("\n=== Lines 73-78 of TRFXPARA.cob after normalization ===");
    let source_xp = std::fs::read_to_string("sources/pgm/TRFXPARA.cob").unwrap();
    let normalized_xp = normalize_cobol_source(&source_xp);
    let lines_xp: Vec<&str> = normalized_xp.lines().collect();
    for i in 72..78.min(lines_xp.len()) {
        println!("{:4}: |{}|", i + 1, lines_xp.get(i).unwrap_or(&""));
    }

    // Check TRFVTE1.cob line 149 (double period)
    println!("\n=== Lines 147-151 of TRFVTE1.cob after normalization ===");
    let source_te1 = std::fs::read_to_string("sources/pgm/TRFVTE1.cob").unwrap();
    let normalized_te1 = normalize_cobol_source(&source_te1);
    let lines_te1: Vec<&str> = normalized_te1.lines().collect();
    for i in 146..151.min(lines_te1.len()) {
        println!("{:4}: |{}|", i + 1, lines_te1.get(i).unwrap_or(&""));
    }

    // Also check TRFVTEl.cob which starts at column 1
    println!("\n=== First 5 lines of TRFVTEl.cob after normalization ===");
    let source2 = std::fs::read_to_string("sources/pgm/TRFVTEl.cob").unwrap();
    let preprocessed2 = preprocess(&source2);
    let normalized2 = normalize_cobol_source(&preprocessed2.source);
    let lines2: Vec<&str> = normalized2.lines().collect();
    for i in 0..5.min(lines2.len()) {
        println!("{:4}: |{}|", i + 1, lines2.get(i).unwrap_or(&""));
    }

    // Check GHOINSSTPL.cob EXEC SQL
    println!("\n=== Lines 63-70 of GHOINSSTPL.cob after normalization ===");
    let source3 = std::fs::read_to_string("sources/pgm/GHOINSSTPL.cob").unwrap();
    let preprocessed3 = preprocess(&source3);
    let normalized3 = normalize_cobol_source(&preprocessed3.source);
    let lines3: Vec<&str> = normalized3.lines().collect();
    for i in 62..70.min(lines3.len()) {
        println!("{:4}: |{}|", i + 1, lines3.get(i).unwrap_or(&""));
    }
}
