use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    let source = std::fs::read_to_string("sources/pgm/TRFVTD1.cob").unwrap();
    let partial: String = source.lines().take(193).collect::<Vec<_>>().join("\n");
    let normalized = normalize_cobol_source(&partial);
    
    println!("First 20 normalized lines:");
    for (i, line) in normalized.lines().enumerate().take(20) {
        println!("{:4}: |{}|", i+1, line);
    }
}
