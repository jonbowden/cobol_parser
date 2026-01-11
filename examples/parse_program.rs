//! Example: Parse a COBOL program and output the results
//!
//! Run with: cargo run --example parse_program

use cobol_parser::{compare_parsers, ParserConfig};
use std::path::Path;

fn main() -> anyhow::Result<()> {
    // Use the sample COBOL program
    let cobol_file = Path::new("sources/pgm/TRFVBAC.cob");

    if !cobol_file.exists() {
        eprintln!("Sample file not found: {}", cobol_file.display());
        eprintln!("Make sure you're running from the cobol_parser directory");
        std::process::exit(1);
    }

    println!("Parsing: {}", cobol_file.display());
    println!("=========================================\n");

    let config = ParserConfig::default();

    // Compare both parsers
    let comparison = compare_parsers(cobol_file, &config)?;

    // Print the comparison report
    println!("{}", comparison.report());

    // Additional details
    println!("\n## Tree-sitter Details\n");
    println!("Data definitions found: {}", comparison.tree_sitter.program.data_definitions.len());
    println!("Divisions found: {:?}",
        comparison.tree_sitter.program.divisions.iter().map(|d| &d.name).collect::<Vec<_>>()
    );

    println!("\n## Cobolparser Details\n");
    println!("Data definitions found: {}", comparison.aleph.program.data_definitions.len());
    println!("Divisions found: {:?}",
        comparison.aleph.program.divisions.iter().map(|d| &d.name).collect::<Vec<_>>()
    );

    Ok(())
}
