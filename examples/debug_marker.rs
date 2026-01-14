use cobol_parser::parsers::normalize_cobol_source;

fn main() {
    // Test lines with change markers that weren't stripped
    let test_lines = [
        "      GP3A00        MOVE \"N\"           TO  WK-C-TAG56-SW",
        "ID1VKE FD  TFSBNKET.",
        "       01  PATH-P1                            PIC X(20) VALUE \"NXYXXXXNNYXXXXXXXXX\".",
    ];

    for line in &test_lines {
        println!("Input:  |{}|", line);
        let source = format!("{}\n", line);
        let normalized = normalize_cobol_source(&source);
        println!("Output (raw): |{}|", normalized.lines().next().unwrap_or(""));
        println!("Output len: {}", normalized.lines().next().unwrap_or("").len());
        let line_out = normalized.lines().next().unwrap_or("");
        let first_nonspace = line_out.chars().position(|c| c != ' ').unwrap_or(0);
        println!("First non-space at: {}", first_nonspace);
        println!("Content starts with: {}", &line_out[first_nonspace..].chars().take(10).collect::<String>());
        println!();
    }
}
