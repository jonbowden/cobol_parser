use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = "       05  TAB-VL2  OCCURS 20 TIMES      PIC X  VALUE \"X\".\n";
    println!("Input:  |{}|", source.trim());

    let normalized = normalize_cobol_source(source);
    println!("Output: |{}|", normalized.trim());

    // Check the character positions
    let chars: Vec<char> = normalized.chars().collect();
    println!("\nFirst 15 chars:");
    for (i, c) in chars.iter().take(15).enumerate() {
        let display = if *c == ' ' { "SPC".to_string() } else { c.to_string() };
        println!("  [{}] = '{}'", i, display);
    }
}
