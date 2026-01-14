use cobol_parser::parsers::normalize_cobol_source;

fn show_lines(filename: &str, center_line: usize, context: usize) {
    let path = format!("sources/pgm/{}", filename);
    let source = std::fs::read_to_string(&path).unwrap();
    let normalized = normalize_cobol_source(&source);
    let lines: Vec<&str> = normalized.lines().collect();

    println!("\n=== {} around line {} (of {} total) ===", filename, center_line, lines.len());
    let start = center_line.saturating_sub(context + 1);
    let end = (center_line + context).min(lines.len());

    for i in start..end {
        let marker = if i + 1 == center_line { ">>>" } else { "   " };
        println!("{} {:4}: |{}|", marker, i + 1, lines.get(i).unwrap_or(&""));
    }
}

fn main() {
    // TRFVTF1B.cob line 479 - 01 PATH-P1 after 05
    show_lines("TRFVTF1B.cob", 479, 2);

    // TRFXPARA.cob line 52 - WORKING-STORAGE SECTION
    show_lines("TRFXPARA.cob", 52, 2);

    // TRFVTB2.cob line 175 - FD with change marker
    show_lines("TRFVTB2.cob", 175, 2);

    // TRFVTE1.cob line 224 - level indentation
    show_lines("TRFVTE1.cob", 224, 2);

    // TRFVTE1.cob line 477 - MOVE statement
    show_lines("TRFVTE1.cob", 477, 2);

    // TRFVTE1.cob line 666 - change marker + MOVE
    show_lines("TRFVTE1.cob", 666, 2);
}
