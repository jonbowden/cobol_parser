//! COBOL Parser Library
//!
//! A Rust library for parsing COBOL source code, providing two parser implementations
//! for comparison:
//!
//! 1. **Tree-sitter**: Uses the tree-sitter-cobol grammar for robust, incremental parsing
//! 2. **Cobolparser/AlephTree**: Uses the cobolparser crate with AlephTree AST
//!
//! This library generates an Intermediate Representation (IR) compatible with the
//! IR_analyser Python implementation.
//!
//! # Example
//!
//! ```no_run
//! use cobol_parser::{parse_file, ParserType, ParserConfig};
//! use std::path::Path;
//!
//! let config = ParserConfig::default();
//! let result = parse_file(Path::new("program.cob"), ParserType::TreeSitter, &config).unwrap();
//!
//! println!("Program: {}", result.program.base.name);
//! println!("Procedures: {:?}", result.program.procedures);
//! ```
//!
//! # Copybook Resolution
//!
//! ```no_run
//! use cobol_parser::{parse_file_with_copybooks, ParserType, ParserConfig};
//! use cobol_parser::copybook::CopybookConfig;
//! use std::path::Path;
//!
//! let parser_config = ParserConfig::default();
//! let copybook_config = CopybookConfig::default();
//!
//! let result = parse_file_with_copybooks(
//!     Path::new("program.cob"),
//!     ParserType::TreeSitter,
//!     &parser_config,
//!     &copybook_config,
//! ).unwrap();
//! ```

pub mod copybook;
pub mod model;
pub mod parsers;

pub use copybook::{CopybookConfig, CopybookResolver, ResolvedSource};
pub use model::*;
pub use parsers::{CobolParser, ParserConfig, ParserError};

use std::path::Path;

/// Available parser types for comparison
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserType {
    TreeSitter,
    Aleph,
}

impl ParserType {
    pub fn name(&self) -> &str {
        match self {
            ParserType::TreeSitter => "tree-sitter-cobol",
            ParserType::Aleph => "cobolparser-aleph",
        }
    }
}

/// Parse a COBOL file using the specified parser (without copybook resolution)
pub fn parse_file(
    path: &Path,
    parser_type: ParserType,
    config: &ParserConfig,
) -> Result<ParseResult, ParserError> {
    match parser_type {
        ParserType::TreeSitter => {
            let parser = parsers::create_tree_sitter_parser()?;
            parser.parse_file(path, config)
        }
        ParserType::Aleph => {
            let parser = parsers::create_aleph_parser()?;
            parser.parse_file(path, config)
        }
    }
}

/// Parse a COBOL file with copybook resolution
///
/// This reads the source file, inlines all COPY statements, then parses the result.
pub fn parse_file_with_copybooks(
    path: &Path,
    parser_type: ParserType,
    parser_config: &ParserConfig,
    copybook_config: &CopybookConfig,
) -> Result<ParseResult, ParserError> {
    // Read the source file
    let source = std::fs::read_to_string(path)?;
    let filename = path.to_string_lossy().to_string();

    // Resolve copybooks
    let mut resolver = CopybookResolver::new(copybook_config.clone());

    // Add the source file's directory as a search path
    if let Some(parent) = path.parent() {
        resolver.add_search_path(parent.to_path_buf());
    }

    let resolved = resolver.resolve(&source).map_err(|e| {
        ParserError::Parse(format!("Copybook resolution failed: {}", e))
    })?;

    // Parse the resolved source
    let mut result = parse_source(&resolved.source, &filename, parser_type, parser_config)?;

    // Add resolved copybook info to the result
    for cb in &resolved.copybooks {
        if !result.program.copybooks.contains(&cb.name) {
            result.program.copybooks.push(cb.name.clone());
        }
    }

    Ok(result)
}

/// Parse COBOL source code from a string using the specified parser
pub fn parse_source(
    source: &str,
    filename: &str,
    parser_type: ParserType,
    config: &ParserConfig,
) -> Result<ParseResult, ParserError> {
    match parser_type {
        ParserType::TreeSitter => {
            let parser = parsers::create_tree_sitter_parser()?;
            parser.parse_source(source, filename, config)
        }
        ParserType::Aleph => {
            let parser = parsers::create_aleph_parser()?;
            parser.parse_source(source, filename, config)
        }
    }
}

/// Parse COBOL source with copybook resolution from a string
pub fn parse_source_with_copybooks(
    source: &str,
    filename: &str,
    parser_type: ParserType,
    parser_config: &ParserConfig,
    copybook_config: &CopybookConfig,
) -> Result<ParseResult, ParserError> {
    let mut resolver = CopybookResolver::new(copybook_config.clone());

    let resolved = resolver.resolve(source).map_err(|e| {
        ParserError::Parse(format!("Copybook resolution failed: {}", e))
    })?;

    let mut result = parse_source(&resolved.source, filename, parser_type, parser_config)?;

    for cb in &resolved.copybooks {
        if !result.program.copybooks.contains(&cb.name) {
            result.program.copybooks.push(cb.name.clone());
        }
    }

    Ok(result)
}

/// Compare results from both parsers (without copybook resolution)
pub fn compare_parsers(
    path: &Path,
    config: &ParserConfig,
) -> Result<ParserComparison, ParserError> {
    let ts_result = parse_file(path, ParserType::TreeSitter, config)?;
    let aleph_result = parse_file(path, ParserType::Aleph, config)?;

    Ok(ParserComparison {
        tree_sitter: ts_result,
        aleph: aleph_result,
        copybooks_resolved: false,
    })
}

/// Compare results from both parsers with copybook resolution
pub fn compare_parsers_with_copybooks(
    path: &Path,
    parser_config: &ParserConfig,
    copybook_config: &CopybookConfig,
) -> Result<ParserComparison, ParserError> {
    let ts_result = parse_file_with_copybooks(
        path,
        ParserType::TreeSitter,
        parser_config,
        copybook_config,
    )?;
    let aleph_result = parse_file_with_copybooks(
        path,
        ParserType::Aleph,
        parser_config,
        copybook_config,
    )?;

    Ok(ParserComparison {
        tree_sitter: ts_result,
        aleph: aleph_result,
        copybooks_resolved: true,
    })
}

/// Results from comparing both parsers
#[derive(Debug)]
pub struct ParserComparison {
    pub tree_sitter: ParseResult,
    pub aleph: ParseResult,
    pub copybooks_resolved: bool,
}

impl ParserComparison {
    /// Generate a comparison report
    pub fn report(&self) -> String {
        let mut report = String::new();

        report.push_str("# Parser Comparison Report\n\n");

        if self.copybooks_resolved {
            report.push_str("**Copybooks: RESOLVED (inlined before parsing)**\n\n");
        } else {
            report.push_str("**Copybooks: NOT resolved**\n\n");
        }

        // Performance
        report.push_str("## Performance\n");
        report.push_str("| Parser | Parse Time (ms) |\n|--------|----------------|\n");
        report.push_str(&format!(
            "| Tree-sitter | {} |\n",
            self.tree_sitter.parse_time_ms
        ));
        report.push_str(&format!(
            "| Cobolparser | {} |\n\n",
            self.aleph.parse_time_ms
        ));

        // Program ID
        report.push_str("## Program ID\n");
        report.push_str("| Parser | Program ID |\n|--------|------------|\n");
        report.push_str(&format!(
            "| Tree-sitter | {} |\n",
            self.tree_sitter.program.base.name
        ));
        report.push_str(&format!(
            "| Cobolparser | {} |\n\n",
            self.aleph.program.base.name
        ));

        // Procedures
        report.push_str("## Procedures Found\n");
        report.push_str("| Parser | Count | Names |\n|--------|-------|-------|\n");
        report.push_str(&format!(
            "| Tree-sitter | {} | {} |\n",
            self.tree_sitter.procedures.len(),
            self.tree_sitter
                .procedures
                .iter()
                .map(|p| p.base.name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ));
        report.push_str(&format!(
            "| Cobolparser | {} | {} |\n\n",
            self.aleph.procedures.len(),
            self.aleph
                .procedures
                .iter()
                .map(|p| p.base.name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ));

        // Data Definitions
        report.push_str("## Data Definitions\n");
        report.push_str("| Parser | Count |\n|--------|-------|\n");
        report.push_str(&format!(
            "| Tree-sitter | {} |\n",
            self.tree_sitter.program.data_definitions.len()
        ));
        report.push_str(&format!(
            "| Cobolparser | {} |\n\n",
            self.aleph.program.data_definitions.len()
        ));

        // Calls
        report.push_str("## External Calls\n");
        report.push_str("| Parser | Count | Targets |\n|--------|-------|--------|\n");
        report.push_str(&format!(
            "| Tree-sitter | {} | {} |\n",
            self.tree_sitter.program.calls.len(),
            self.tree_sitter.program.calls.join(", ")
        ));
        report.push_str(&format!(
            "| Cobolparser | {} | {} |\n\n",
            self.aleph.program.calls.len(),
            self.aleph.program.calls.join(", ")
        ));

        // Copybooks
        report.push_str("## Copybooks\n");
        report.push_str("| Parser | Count | Names |\n|--------|-------|-------|\n");
        report.push_str(&format!(
            "| Tree-sitter | {} | {} |\n",
            self.tree_sitter.program.copybooks.len(),
            self.tree_sitter.program.copybooks.join(", ")
        ));
        report.push_str(&format!(
            "| Cobolparser | {} | {} |\n\n",
            self.aleph.program.copybooks.len(),
            self.aleph.program.copybooks.join(", ")
        ));

        // Errors
        report.push_str("## Parse Errors\n");
        report.push_str("| Parser | Error Count | Warning Count |\n|--------|-------------|---------------|\n");
        report.push_str(&format!(
            "| Tree-sitter | {} | {} |\n",
            self.tree_sitter.errors.len(),
            self.tree_sitter.warnings.len()
        ));
        report.push_str(&format!(
            "| Cobolparser | {} | {} |\n",
            self.aleph.errors.len(),
            self.aleph.warnings.len()
        ));

        report
    }
}
