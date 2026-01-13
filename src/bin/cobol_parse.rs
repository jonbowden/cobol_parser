//! COBOL Parser CLI
//!
//! Parses COBOL source files and outputs JSON for integration with IR_analyser.
//!
//! Usage:
//!   cobol_parse <file.cob> [--copybook-path <path>]...
//!
//! Output is JSON to stdout with the ParseResult structure.

use cobol_parser::{parse_file, parse_file_with_copybooks, ParserType, ParserConfig};
use cobol_parser::copybook::CopybookConfig;
use std::env;
use std::path::{Path, PathBuf};
use std::process;

fn print_usage() {
    eprintln!("Usage: cobol_parse <file.cob> [--copybook-path <path>]...");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --copybook-path <path>  Add copybook search path (can be repeated)");
    eprintln!("  --no-copybooks          Don't resolve copybooks");
    eprintln!("  --pretty                Pretty-print JSON output");
    eprintln!("  --help                  Show this help");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    let mut file_path: Option<PathBuf> = None;
    let mut copybook_paths: Vec<PathBuf> = Vec::new();
    let mut resolve_copybooks = true;
    let mut pretty = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--help" | "-h" => {
                print_usage();
                process::exit(0);
            }
            "--copybook-path" => {
                if i + 1 >= args.len() {
                    eprintln!("Error: --copybook-path requires a path argument");
                    process::exit(1);
                }
                copybook_paths.push(PathBuf::from(&args[i + 1]));
                i += 2;
            }
            "--no-copybooks" => {
                resolve_copybooks = false;
                i += 1;
            }
            "--pretty" => {
                pretty = true;
                i += 1;
            }
            arg if arg.starts_with('-') => {
                eprintln!("Error: Unknown option: {}", arg);
                process::exit(1);
            }
            _ => {
                if file_path.is_some() {
                    eprintln!("Error: Multiple input files not supported");
                    process::exit(1);
                }
                file_path = Some(PathBuf::from(&args[i]));
                i += 1;
            }
        }
    }

    let file_path = match file_path {
        Some(p) => p,
        None => {
            eprintln!("Error: No input file specified");
            print_usage();
            process::exit(1);
        }
    };

    if !file_path.exists() {
        eprintln!("Error: File not found: {}", file_path.display());
        process::exit(1);
    }

    let config = ParserConfig::default();

    let result = if resolve_copybooks {
        // Build copybook config
        let mut copybook_config = CopybookConfig::default();

        // Add file's directory as search path
        if let Some(parent) = file_path.parent() {
            copybook_config.search_paths.push(parent.to_path_buf());
        }

        // Add explicit copybook paths
        for path in copybook_paths {
            copybook_config.search_paths.push(path);
        }

        // Add common relative paths
        copybook_config.search_paths.push(PathBuf::from("sources/cpy"));
        copybook_config.search_paths.push(PathBuf::from("sources/copy"));

        parse_file_with_copybooks(&file_path, ParserType::TreeSitter, &config, &copybook_config)
    } else {
        parse_file(&file_path, ParserType::TreeSitter, &config)
    };

    match result {
        Ok(parse_result) => {
            let json = if pretty {
                serde_json::to_string_pretty(&parse_result)
            } else {
                serde_json::to_string(&parse_result)
            };

            match json {
                Ok(s) => println!("{}", s),
                Err(e) => {
                    eprintln!("Error: Failed to serialize result: {}", e);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("Error: Parse failed: {}", e);
            process::exit(1);
        }
    }
}
