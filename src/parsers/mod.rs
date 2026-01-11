//! COBOL Parser implementations
//!
//! This module provides two parser implementations for comparison:
//! - Tree-sitter based parser (tree_sitter.rs)
//! - AlephTree/cobolparser based parser (aleph.rs)

pub mod tree_sitter_parser;
pub mod aleph_parser;

use crate::model::{ParseResult, UrnFormat};
use std::path::Path;
use thiserror::Error;

/// Normalize COBOL source code for parsing.
///
/// This function cleans up fixed-format COBOL source by:
/// - Blanking out columns 1-6 (sequence number area) to remove change markers
/// - Preserving column 7 (indicator area: *, /, -, D, space)
/// - Preserving columns 8-72 (Area A and Area B)
/// - Optionally truncating or blanking columns 73-80 (identification area)
///
/// This helps parsers handle source files with non-standard sequence numbers
/// like change tracking markers (HOJE01, GP3M00, VASA01, etc.)
pub fn normalize_cobol_source(source: &str) -> String {
    let mut result = String::with_capacity(source.len());

    for line in source.lines() {
        let chars: Vec<char> = line.chars().collect();
        let len = chars.len();

        if len == 0 {
            result.push('\n');
            continue;
        }

        // Build normalized line
        let mut normalized = String::with_capacity(len.max(80));

        // Columns 1-6: blank out sequence number area
        normalized.push_str("      ");

        // Column 7: indicator area (preserve if exists)
        if len > 6 {
            normalized.push(chars[6]);
        } else {
            normalized.push(' ');
        }

        // Columns 8-72: code area (preserve)
        if len > 7 {
            let end = len.min(72);
            for i in 7..end {
                normalized.push(chars[i]);
            }
        }

        // Columns 73-80: identification area (truncate/ignore for parsing)
        // We don't include these as they can cause issues

        result.push_str(&normalized);
        result.push('\n');
    }

    result
}

/// Parser error types
#[derive(Debug, Error)]
pub enum ParserError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Grammar loading error: {0}")]
    Grammar(String),

    #[error("Unsupported feature: {0}")]
    Unsupported(String),
}

/// Configuration for COBOL parsing
#[derive(Debug, Clone)]
pub struct ParserConfig {
    pub app: String,
    pub urn_format: UrnFormat,
    pub include_paths: Vec<String>,
    pub resolve_copybooks: bool,
    pub copybook_dirs: Vec<String>,
    pub strict_columns: bool,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            app: "rem".to_string(),
            urn_format: UrnFormat::Platform,
            include_paths: vec!["**/*".to_string()],
            resolve_copybooks: true,
            copybook_dirs: Vec::new(),
            strict_columns: false,
        }
    }
}

/// Trait for COBOL parsers - both implementations must satisfy this contract
pub trait CobolParser: Send + Sync {
    /// Get parser name for identification
    fn name(&self) -> &str;

    /// Parse a COBOL source file
    fn parse_file(&self, path: &Path, config: &ParserConfig) -> Result<ParseResult, ParserError>;

    /// Parse COBOL source code from a string
    fn parse_source(&self, source: &str, filename: &str, config: &ParserConfig) -> Result<ParseResult, ParserError>;

    /// Check if parser supports a specific feature
    fn supports_feature(&self, feature: &str) -> bool;
}

/// Parser capability features for comparison
pub mod features {
    pub const PROGRAM_ID: &str = "program_id";
    pub const PROCEDURES: &str = "procedures";
    pub const CALLS: &str = "calls";
    pub const FILES: &str = "files";
    pub const TABLES: &str = "tables";
    pub const COPYBOOKS: &str = "copybooks";
    pub const DIVISIONS: &str = "divisions";
    pub const DATA_DEFINITIONS: &str = "data_definitions";
    pub const VARIABLES: &str = "variables";
    pub const SOURCE_CHUNKS: &str = "source_chunks";
    pub const SQL_STATEMENTS: &str = "sql_statements";
    pub const CICS_COMMANDS: &str = "cics_commands";
}

/// Create a tree-sitter based parser
pub fn create_tree_sitter_parser() -> Result<impl CobolParser, ParserError> {
    tree_sitter_parser::TreeSitterCobolParser::new()
}

/// Create an AlephTree based parser
pub fn create_aleph_parser() -> Result<impl CobolParser, ParserError> {
    aleph_parser::AlephCobolParser::new()
}
