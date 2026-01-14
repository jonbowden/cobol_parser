use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTEl.cob").unwrap();
    let normalized = normalize_cobol_source(&source);
    let lines: Vec<&str> = normalized.lines().collect();

    println!("=== TRFVTEl.cob lines 99-105 ===");
    for i in 98..105.min(lines.len()) {
        println!("{:4}: |{}|", i + 1, lines.get(i).unwrap_or(&""));
    }
}
