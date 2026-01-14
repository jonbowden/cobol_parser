use cobol_parser::preprocessor::{preprocess, FixType};
use std::fs;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let dry_run = args.contains(&"--dry-run".to_string());
    
    if dry_run {
        println!("DRY RUN - no files will be modified\n");
    }
    
    let mut total_fixes = 0;
    let mut files_modified = 0;
    
    for entry in fs::read_dir("sources/pgm").unwrap() {
        let path = entry.unwrap().path();
        if !path.extension().map_or(false, |e| e == "cob") {
            continue;
        }
        
        let filename = path.file_name().unwrap().to_str().unwrap();
        let source = fs::read_to_string(&path).unwrap();
        let result = preprocess(&source);
        
        if result.fixes.is_empty() {
            continue;
        }
        
        // Count fix types
        let seq_fixes = result.fixes.iter().filter(|f| f.fix_type == FixType::MovedSequenceNumber).count();
        let cont_fixes = result.fixes.iter().filter(|f| f.fix_type == FixType::ContinuationToComment).count();
        let comment_fixes = result.fixes.iter().filter(|f| f.fix_type == FixType::FixedCommentAlignment).count();
        
        println!("{}: {} fixes (seq:{}, cont:{}, comment:{})", 
                 filename, result.fixes.len(), seq_fixes, cont_fixes, comment_fixes);
        
        if !dry_run {
            fs::write(&path, &result.source).unwrap();
            files_modified += 1;
        }
        
        total_fixes += result.fixes.len();
    }
    
    println!("\n=== Summary ===");
    println!("Total fixes: {}", total_fixes);
    if dry_run {
        println!("Files that would be modified: (dry run)");
    } else {
        println!("Files modified: {}", files_modified);
    }
}
