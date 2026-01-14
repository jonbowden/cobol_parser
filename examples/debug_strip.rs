use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    // Test the problematic line exactly as it appears in the file
    let line = "                 MOVE WK-VTE1-PARALNO    TO TFSSTPL-PARALNO.";
    println!("Input line: |{}|", line);
    println!("Input len: {}", line.len());
    println!("Chars 40-60: |{}|", &line[40..60.min(line.len())]);

    // Full source with line ending
    let source = format!("{}\n", line);

    // Step through normalization manually
    // 1. After preprocess
    use cobol_parser::preprocessor;
    let preprocessed = preprocessor::preprocess(&source);
    println!("\n1. After preprocess:");
    for (i, pline) in preprocessed.source.lines().enumerate() {
        println!("   Line {}: |{}|", i, pline);
        println!("   Len: {}", pline.len());
        if pline.len() >= 60 {
            println!("   Chars 40-60: |{}|", &pline[40..60]);
        }
    }

    // 2. Full normalize
    let normalized = normalize_cobol_source(&source);
    println!("\n2. After normalize:");
    for (i, nline) in normalized.lines().enumerate() {
        println!("   Line {}: |{}|", i, nline);
        if nline.len() >= 60 {
            println!("   Chars 40-60: |{}|", &nline[40..60]);
        }
    }
}
