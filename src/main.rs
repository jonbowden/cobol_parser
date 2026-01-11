//! COBOL Parser CLI
//!
//! Command-line interface for parsing COBOL files and comparing parsers.

use cobol_parser::{
    compare_parsers, compare_parsers_with_copybooks, parse_file, parse_file_with_copybooks,
    CopybookConfig, ParserConfig, ParserType,
};
use std::path::PathBuf;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 || args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
        print_usage(&args[0]);
        if args.len() < 2 {
            std::process::exit(1);
        }
        std::process::exit(0);
    }

    let file_path = PathBuf::from(&args[1]);
    if !file_path.exists() {
        eprintln!("Error: File not found: {}", file_path.display());
        std::process::exit(1);
    }

    // Parse command line options
    let mut parser_choice = "compare";
    let mut output_json = false;
    let mut resolve_copybooks = false;
    let mut copybook_paths: Vec<PathBuf> = vec![PathBuf::from("sources/cpy")];

    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "--parser" | "-p" => {
                if i + 1 < args.len() {
                    parser_choice = &args[i + 1];
                    i += 1;
                }
            }
            "--json" | "-j" => {
                output_json = true;
            }
            "--resolve-copybooks" | "-r" => {
                resolve_copybooks = true;
            }
            "--copybook-path" | "-c" => {
                if i + 1 < args.len() {
                    copybook_paths.push(PathBuf::from(&args[i + 1]));
                    i += 1;
                }
            }
            "--help" | "-h" => {
                print_usage(&args[0]);
                std::process::exit(0);
            }
            _ => {}
        }
        i += 1;
    }

    let parser_config = ParserConfig::default();
    let mut copybook_config = CopybookConfig::default();
    copybook_config.search_paths = copybook_paths;

    match parser_choice {
        "tree-sitter" | "ts" => {
            let result = if resolve_copybooks {
                parse_file_with_copybooks(
                    &file_path,
                    ParserType::TreeSitter,
                    &parser_config,
                    &copybook_config,
                )?
            } else {
                parse_file(&file_path, ParserType::TreeSitter, &parser_config)?
            };

            if output_json {
                println!("{}", serde_json::to_string_pretty(&result)?);
            } else {
                print_result(&result, resolve_copybooks);
            }
        }
        "aleph" | "a" => {
            let result = if resolve_copybooks {
                parse_file_with_copybooks(
                    &file_path,
                    ParserType::Aleph,
                    &parser_config,
                    &copybook_config,
                )?
            } else {
                parse_file(&file_path, ParserType::Aleph, &parser_config)?
            };

            if output_json {
                println!("{}", serde_json::to_string_pretty(&result)?);
            } else {
                print_result(&result, resolve_copybooks);
            }
        }
        "compare" | _ => {
            let comparison = if resolve_copybooks {
                compare_parsers_with_copybooks(&file_path, &parser_config, &copybook_config)?
            } else {
                compare_parsers(&file_path, &parser_config)?
            };

            if output_json {
                println!("{{");
                println!(
                    "  \"copybooks_resolved\": {},",
                    comparison.copybooks_resolved
                );
                println!(
                    "  \"tree_sitter\": {},",
                    serde_json::to_string_pretty(&comparison.tree_sitter)?
                );
                println!(
                    "  \"aleph\": {}",
                    serde_json::to_string_pretty(&comparison.aleph)?
                );
                println!("}}");
            } else {
                println!("{}", comparison.report());
            }
        }
    }

    Ok(())
}

fn print_usage(program: &str) {
    eprintln!("COBOL Parser - Compare tree-sitter vs cobolparser approaches");
    eprintln!();
    eprintln!("Usage: {} <cobol_file> [options]", program);
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -p, --parser <name>       Parser to use: tree-sitter, aleph, compare (default: compare)");
    eprintln!("  -j, --json                Output results as JSON");
    eprintln!("  -r, --resolve-copybooks   Inline COPY statements before parsing");
    eprintln!("  -c, --copybook-path <dir> Add copybook search directory (can be repeated)");
    eprintln!("  -h, --help                Show this help message");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  {} sources/pgm/TRFVBAC.cob", program);
    eprintln!("  {} sources/pgm/TRFVBAC.cob --parser tree-sitter", program);
    eprintln!("  {} sources/pgm/TRFVBAC.cob --resolve-copybooks", program);
    eprintln!(
        "  {} sources/pgm/TRFVBAC.cob -r -c sources/cpy -c /other/copybooks",
        program
    );
    eprintln!("  {} sources/pgm/TRFVBAC.cob --json", program);
}

fn print_result(result: &cobol_parser::ParseResult, copybooks_resolved: bool) {
    println!("Parser: {}", result.parser_name);
    println!("Parse time: {} ms", result.parse_time_ms);
    if copybooks_resolved {
        println!("Copybooks: RESOLVED (inlined before parsing)");
    }
    println!();
    println!("Program ID: {}", result.program.base.name);
    println!("URN: {}", result.program.base.urn);
    println!();
    println!("Divisions: {}", result.program.divisions.len());
    for div in &result.program.divisions {
        println!("  - {} (line {})", div.name, div.start_line);
    }
    println!();
    println!("Procedures: {}", result.procedures.len());
    for proc in &result.procedures {
        println!(
            "  - {} (lines {:?}-{:?})",
            proc.base.name, proc.source_start_line, proc.source_end_line
        );
    }
    println!();
    println!("Calls: {}", result.program.calls.join(", "));
    println!("Copybooks: {}", result.program.copybooks.join(", "));
    println!("Reads: {}", result.program.reads_entities.join(", "));
    println!("Writes: {}", result.program.writes_entities.join(", "));
    println!();
    println!(
        "Data Definitions: {}",
        result.program.data_definitions.len()
    );
    for def in result.program.data_definitions.iter().take(10) {
        println!("  - {:02} {} {:?}", def.level, def.name, def.picture);
    }
    if result.program.data_definitions.len() > 10 {
        println!(
            "  ... and {} more",
            result.program.data_definitions.len() - 10
        );
    }
    println!();
    if !result.errors.is_empty() {
        println!("Errors: {}", result.errors.len());
        for err in result.errors.iter().take(5) {
            println!("  - Line {:?}: {}", err.line, err.message);
        }
    }
    if !result.warnings.is_empty() {
        println!("Warnings: {}", result.warnings.len());
        for warn in &result.warnings {
            println!("  - {}", warn);
        }
    }
}
