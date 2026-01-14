use cobol_parser::parsers::normalize_cobol_source;
use std::fs;

fn main() {
    let files = fs::read_dir("sources/pgm")
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "cob"));

    for file in files {
        let path = file.path();
        let source = fs::read_to_string(&path).unwrap();
        let orig_lines = source.lines().count();
        let normalized = normalize_cobol_source(&source);
        let norm_lines = normalized.lines().count();
        let diff = orig_lines as i32 - norm_lines as i32;
        println!("{}: {} -> {} (diff: {})",
            path.file_name().unwrap().to_string_lossy(),
            orig_lines, norm_lines, diff);
    }
}
