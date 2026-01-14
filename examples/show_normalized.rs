use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = args.get(1).map(|s| s.as_str()).unwrap_or("sources/pgm/TRFVTE1.cob");
    let line_num: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(727);
    let context: usize = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(5);

    let source = std::fs::read_to_string(filename).unwrap();
    let normalized = normalize_cobol_source(&source);
    let lines: Vec<&str> = normalized.lines().collect();

    println!("=== {} (total {} lines) lines {}-{} ===\n", filename, lines.len(), line_num.saturating_sub(context), line_num + context);

    let start = line_num.saturating_sub(context + 1);
    let end = (line_num + context).min(lines.len());

    eprintln!("DEBUG: start={}, end={}, lines.len={}", start, end, lines.len());

    for i in start..end {
        let marker = if i + 1 == line_num { ">>>" } else { "   " };
        let line_content = lines.get(i).map(|s| s.to_string()).unwrap_or_else(|| "<missing>".to_string());
        println!("{} {:4}: |{}|", marker, i + 1, line_content);
    }
}
