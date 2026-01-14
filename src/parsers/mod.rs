//! COBOL Parser implementations
//!
//! This module provides two parser implementations for comparison:
//! - Tree-sitter based parser (tree_sitter.rs)
//! - AlephTree/cobolparser based parser (aleph.rs)

pub mod tree_sitter_parser;
pub mod aleph_parser;

use crate::model::{ParseResult, UrnFormat};
use crate::preprocessor;
use std::path::Path;
use thiserror::Error;

/// Normalize COBOL source code for parsing.
///
/// This function cleans up fixed-format COBOL source by:
/// 1. Running the preprocessor to fix formatting issues:
///    - Move inline sequence numbers to columns 1-6
///    - Convert continuation indicators to comments
///    - Fix misaligned comment indicators
/// 2. Blanking out columns 1-6 (sequence number area)
/// 3. Preserving column 7 (indicator area: *, /, -, D, space)
/// 4. Removing inline change markers from the code area (column 8+)
///
/// This helps parsers handle source files with non-standard formatting.
pub fn normalize_cobol_source(source: &str) -> String {
    // First, run the preprocessor to fix structural issues
    let preprocessed = preprocessor::preprocess(source);
    let source = &preprocessed.source;

    // Check if this is a free-format file (code starts at column 1)
    let is_free_format = detect_free_format(source);

    let mut result = String::with_capacity(source.len());

    for line in source.lines() {
        // Expand tabs to spaces (8-column tab stops)
        let line = expand_tabs(line);
        let chars: Vec<char> = line.chars().collect();
        let len = chars.len();

        if len == 0 {
            result.push('\n');
            continue;
        }

        if is_free_format {
            // For free-format, just pass through with proper formatting
            let mut normalized = normalize_free_format_line(&line);
            // Also collapse spaces in free-format lines
            normalized = collapse_multiple_spaces(&normalized);
            result.push_str(&normalized);
            result.push('\n');
            continue;
        }

        // Build normalized line for fixed-format
        let mut normalized = String::with_capacity(len.max(80));

        // Check if there's a level number in columns 1-6 that should be preserved
        // (some sources have level numbers starting in column 3-4 instead of column 8)
        let level_info = extract_level_from_seq_area(&chars);

        // Columns 1-6: blank out sequence number area
        normalized.push_str("      ");

        // Column 7: indicator area (preserve if exists, unless it's part of data name)
        let (indicator, include_col7) = match level_info {
            Some((_, true)) => (' ', true),   // Col 7 is part of data name
            _ => (if len > 6 { chars[6] } else { ' ' }, false),
        };
        normalized.push(indicator);

        // If we found a level number in the sequence area, prepend it to the code
        if let Some((level, _)) = level_info {
            // Add indentation based on level
            if level >= 2 && level <= 49 {
                normalized.push_str("    ");  // Area B indent
            }
            normalized.push_str(&format!("{:02}  ", level));
        }

        // Columns 8+: code area - strip inline change markers
        // If include_col7 is set, we also need to include column 7 in the code
        let code_start = if include_col7 { 6 } else { 7 };
        if len > code_start {
            let code_area: String = chars[code_start..].iter().collect();
            // Trim trailing whitespace to reduce line length
            // (columns 73-80 often have spaces or change markers)
            let code_area = code_area.trim_end().to_string();
            let trimmed_code = code_area.trim_start();

            // Check if code area starts with asterisks (decoration line) - treat as comment
            if trimmed_code.starts_with("****") || trimmed_code.starts_with("*---") ||
               trimmed_code.starts_with("*===") || trimmed_code.starts_with("*+++") {
                // Convert to proper comment line
                normalized.pop(); // Remove the indicator we added
                normalized.push('*');
                normalized.push_str(&code_area);
            } else if trimmed_code.starts_with('|') {
                // Pipe character at start of code area - treat as comment
                normalized.pop(); // Remove the indicator we added
                normalized.push('*');
                // Replace leading pipe with space
                let code_without_pipe = format!(" {}", &trimmed_code[1..]);
                let leading = code_area.len() - trimmed_code.len();
                normalized.push_str(&" ".repeat(leading));
                normalized.push_str(&code_without_pipe);
            } else {
                // Strip inline change markers if this is not a comment line
                let cleaned_code = if indicator != '*' && indicator != '/' {
                    let stripped = strip_inline_change_markers(&code_area);
                    // Check if the stripped result indicates this should be a comment
                    if stripped.starts_with('*') {
                        // Convert to comment line
                        normalized.pop(); // Remove current indicator
                        normalized.push('*');
                        stripped[1..].to_string()
                    } else {
                        stripped
                    }
                } else {
                    code_area
                };

                normalized.push_str(&cleaned_code);
            }
        }

        // Clean up double periods and other artifacts
        let mut cleaned = normalized
            .replace(". .", ".")
            .replace("..", ".");

        // Check if line is just "/" (page eject after change marker stripped) - convert to comment
        if cleaned.len() > 7 {
            let code_part = cleaned[7..].trim();
            if code_part == "/" || code_part == "/." {
                // Convert to comment line
                cleaned = format!("      *{}", &cleaned[7..]);
            }
        }

        // Fix split COBOL keywords (e.g., "AN      D" -> "AND")
        cleaned = fix_split_keywords(&cleaned);

        // Fix subordinate level indentation (02-49 should be in Area B)
        cleaned = fix_level_indentation(&cleaned);

        // Collapse multiple spaces to reduce line length
        cleaned = collapse_multiple_spaces(&cleaned);

        result.push_str(&cleaned);
        result.push('\n');
    }

    result
}

/// Expand tab characters to spaces using 8-column tab stops.
/// COBOL sources sometimes use tabs, which need to be expanded
/// to ensure correct column-based processing.
fn expand_tabs(line: &str) -> String {
    if !line.contains('\t') {
        return line.to_string();
    }

    let mut result = String::with_capacity(line.len() * 2);
    let mut col = 0;

    for ch in line.chars() {
        if ch == '\t' {
            // Tab to next 8-column boundary
            let spaces = 8 - (col % 8);
            for _ in 0..spaces {
                result.push(' ');
            }
            col += spaces;
        } else {
            result.push(ch);
            col += 1;
        }
    }

    result
}

/// Extract a level number from the sequence area (columns 1-6) if present.
/// Some COBOL sources have level numbers starting at column 3-4 instead of column 8.
/// Returns (level, include_col7) where include_col7 indicates if column 7 should be
/// included in the code area (when it's part of a data name, not an indicator).
fn extract_level_from_seq_area(chars: &[char]) -> Option<(u8, bool)> {
    if chars.len() < 7 {
        return None;
    }

    // Look for pattern: spaces + 2-digit number + space in columns 1-6
    // e.g., "   05 " or "  01  " in the sequence area
    let seq_area: String = chars[..6].iter().collect();
    let trimmed = seq_area.trim();

    // Must be just a 2-digit number (or 1 digit)
    if !trimmed.is_empty()
        && trimmed.len() <= 2
        && trimmed.chars().all(|c| c.is_ascii_digit())
    {
        if let Ok(level) = trimmed.parse::<u8>() {
            // Valid COBOL level numbers
            if matches!(level, 1..=49 | 66 | 77 | 88) {
                let col7 = chars[6];
                // If column 7 is a valid indicator, use normal processing
                if col7.is_whitespace() {
                    return Some((level, false));
                }
                // If column 7 is alphanumeric, it might be part of the data name
                // (e.g., "  01  TABLE-ARRAY" where T is at column 7)
                if col7.is_ascii_alphanumeric() {
                    return Some((level, true));
                }
            }
        }
    }

    None
}

/// Fix indentation for subordinate level numbers.
/// Level numbers 02-49 should be indented (Area B), not at column 8 (Area A).
fn fix_level_indentation(line: &str) -> String {
    if line.len() <= 7 {
        return line.to_string();
    }

    let code_part = &line[7..];
    let trimmed = code_part.trim_start();

    // Check if line starts with a subordinate level number (02-49)
    // Level numbers must be followed by whitespace, not more digits or hyphens
    // (to distinguish from paragraph names like "299-MOVE-TAG-VALUES-EX")
    if trimmed.len() >= 3 {
        let first_two: String = trimmed.chars().take(2).collect();
        let third_char = trimmed.chars().nth(2).unwrap_or(' ');

        // Level numbers are followed by whitespace, not more digits or hyphens
        if third_char.is_whitespace() {
            if let Ok(level) = first_two.parse::<u32>() {
                if level >= 2 && level <= 49 {
                    // Check if the level number is already indented
                    let current_indent = code_part.len() - trimmed.len();
                    if current_indent < 4 {
                        // Not indented enough, add 4 spaces
                        return format!("{}    {}", &line[..7], trimmed);
                    }
                }
            }
        }
    }

    line.to_string()
}

/// Collapse multiple consecutive spaces into single spaces in the code area.
/// This reduces line length for lines with excessive whitespace alignment.
/// Preserves columns 1-7 (sequence area + indicator) and the leading indent in column 8+.
fn collapse_multiple_spaces(line: &str) -> String {
    if line.len() <= 7 {
        return line.to_string();
    }

    let chars: Vec<char> = line.chars().collect();

    // Don't collapse spaces in comment lines
    let indicator = chars.get(6).copied().unwrap_or(' ');
    if indicator == '*' || indicator == '/' {
        return line.to_string();
    }

    // Build prefix from first 7 characters
    let prefix: String = chars[..7].iter().collect();
    let code_part: String = chars[7..].iter().collect();

    // Preserve leading whitespace (indentation)
    let leading_spaces = code_part.len() - code_part.trim_start().len();
    let indent = &code_part[..leading_spaces];
    let content = &code_part[leading_spaces..];

    // Collapse multiple spaces to single space in the content
    // But be careful not to collapse spaces inside string literals
    let mut result = String::with_capacity(content.len());
    let mut in_string = false;
    let mut prev_space = false;

    for ch in content.chars() {
        if ch == '"' {
            in_string = !in_string;
            result.push(ch);
            prev_space = false;
        } else if ch == ' ' && !in_string {
            if !prev_space {
                result.push(ch);
            }
            prev_space = true;
        } else {
            result.push(ch);
            prev_space = false;
        }
    }

    format!("{}{}{}", prefix, indent, result)
}

/// Fix COBOL keywords that have been split with spaces in the middle.
/// Uses word boundary checking to avoid false matches inside identifiers.
fn fix_split_keywords(line: &str) -> String {
    let mut result = line.to_string();

    // Common split patterns we've seen in source files
    // Format: (pattern, replacement)
    let patterns = [
        ("AN      D", "AND"),
        ("AN     D", "AND"),
        ("AN    D", "AND"),
        ("AN   D", "AND"),
        ("AN  D", "AND"),
        ("O      R", "OR"),
        ("O     R", "OR"),
        ("O    R", "OR"),
        ("O   R", "OR"),
        ("NO      T", "NOT"),
        ("NO     T", "NOT"),
        ("NO    T", "NOT"),
        ("NO   T", "NOT"),
        ("NO  T", "NOT"),
    ];

    for (pattern, replacement) in patterns {
        // Only replace if the pattern is at a word boundary
        // (preceded and followed by non-alphanumeric or start/end of string)
        result = replace_at_word_boundary(&result, pattern, replacement);
    }

    result
}

/// Replace pattern with replacement only at word boundaries
fn replace_at_word_boundary(text: &str, pattern: &str, replacement: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        // Check if pattern matches at this position
        let remaining = chars.len() - i;
        if remaining >= pattern_chars.len() {
            let slice: String = chars[i..i + pattern_chars.len()].iter().collect();
            if slice == pattern {
                // Check word boundaries
                let before_ok = i == 0 || !chars[i - 1].is_alphanumeric();
                let after_ok = i + pattern_chars.len() >= chars.len()
                    || !chars[i + pattern_chars.len()].is_alphanumeric();

                if before_ok && after_ok {
                    result.push_str(replacement);
                    i += pattern_chars.len();
                    continue;
                }
            }
        }
        result.push(chars[i]);
        i += 1;
    }

    result
}

/// Detect if a COBOL source file is in free format (code starts at column 1)
fn detect_free_format(source: &str) -> bool {
    for line in source.lines().take(10) {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        // Check if line starts with COBOL division/section keywords at column 1
        let upper = line.to_uppercase();
        if upper.starts_with("IDENTIFICATION")
            || upper.starts_with("PROGRAM-ID")
            || upper.starts_with("ENVIRONMENT")
            || upper.starts_with("DATA")
            || upper.starts_with("PROCEDURE")
        {
            return true;
        }

        // If we find a line with proper column 7 indicator, it's fixed format
        if line.len() > 6 {
            let col7 = line.chars().nth(6).unwrap_or(' ');
            if col7 == ' ' || col7 == '*' || col7 == '/' || col7 == '-' {
                // Check if columns 1-6 look like sequence numbers or spaces
                let cols_1_6: String = line.chars().take(6).collect();
                if cols_1_6.trim().is_empty() || cols_1_6.chars().all(|c| c.is_ascii_digit() || c == ' ') {
                    return false; // Looks like fixed format
                }
            }
        }
    }
    false
}

/// Normalize a free-format COBOL line
fn normalize_free_format_line(line: &str) -> String {
    let trimmed = line.trim();

    // Handle comment lines (starting with *)
    if trimmed.starts_with('*') {
        return format!("      *{}", &trimmed[1..]);
    }

    // Handle empty lines
    if trimmed.is_empty() {
        return "      ".to_string();
    }

    // Strip change markers from free format lines too
    let stripped = strip_inline_change_markers(trimmed);
    let code = stripped.trim();

    // Clean up double periods
    let cleaned = code
        .replace(". .", ".")
        .replace("..", ".");

    // Add proper column structure: 6 spaces + space indicator + code
    format!("       {}", cleaned)
}

/// Strip inline change markers from the code area.
///
/// Change markers are typically 5-7 alphanumeric characters at the start of
/// the code area, followed by a space or asterisk. Examples: GP3M00, 5Q1ARV, HOJE01, CMP3A3
fn strip_inline_change_markers(code: &str) -> String {
    // Check if line starts with a change marker pattern
    // Pattern: alphanumeric (5-7 chars) followed by space or special char, at start of line
    let trimmed = code.trim_start();

    if trimmed.is_empty() {
        return code.to_string();
    }

    // Find the end of potential change marker
    // Change markers are followed by whitespace or asterisk, not by hyphen or other chars
    let marker_end = trimmed
        .char_indices()
        .find(|(_, c)| !c.is_ascii_alphanumeric())
        .map(|(i, _)| i)
        .unwrap_or(trimmed.len());

    // Check what character follows the potential marker
    let following_char = trimmed.chars().nth(marker_end).unwrap_or(' ');

    // If followed by hyphen, this is likely a variable name like TAG57-BIC, not a change marker
    if following_char == '-' {
        return code.to_string();
    }

    let first_word = &trimmed[..marker_end];

    // Check if it matches change marker pattern:
    // - 5-7 characters
    // - Alphanumeric only
    // - Contains at least one digit OR is all uppercase letters followed by statement keyword
    // - Not a COBOL reserved word
    let has_digit = first_word.chars().any(|c| c.is_ascii_digit());
    let is_all_upper = first_word.chars().all(|c| c.is_ascii_uppercase());
    let remaining = &trimmed[marker_end..];
    let next_word_raw = remaining.trim_start().split_whitespace().next().unwrap_or("");
    // Remove trailing period or other punctuation for keyword check
    let next_word = next_word_raw.trim_end_matches(|c: char| !c.is_ascii_alphanumeric());
    let followed_by_keyword = is_cobol_keyword(next_word);

    // Match if: has digit, OR (all uppercase + followed by COBOL keyword)
    let is_change_marker = first_word.len() >= 5
        && first_word.len() <= 7
        && first_word.chars().all(|c| c.is_ascii_alphanumeric())
        && (has_digit || (is_all_upper && followed_by_keyword))
        && !is_cobol_keyword(first_word);

    if is_change_marker {
        let remaining = &trimmed[marker_end..];
        let leading_spaces = code.len() - code.trim_start().len();

        // Check if remaining starts with asterisk (comment marker after change marker)
        if remaining.starts_with('*') {
            // This line is effectively a comment - mark it as such by returning comment format
            // The caller will handle this
            return format!("*{}", remaining);
        }

        // Check for nested change markers (e.g., "GP4D00*CMP3F1*")
        if remaining.starts_with('*') || remaining.trim_start().starts_with('*') {
            return format!("*{}", remaining);
        }

        // Strip the change marker, preserve spacing
        let result = format!("{}{}", " ".repeat(leading_spaces + first_word.len()), remaining);

        // Recursively strip additional change markers
        strip_inline_change_markers(&result)
    } else {
        code.to_string()
    }
}

/// Check if a word is a COBOL keyword that should not be stripped
fn is_cobol_keyword(word: &str) -> bool {
    let upper = word.to_uppercase();
    matches!(upper.as_str(),
        "ACCEPT" | "ADD" | "ALTER" | "CALL" | "CANCEL" | "CLOSE" | "COMPUTE" |
        "CONTINUE" | "DELETE" | "DISPLAY" | "DIVIDE" | "ENTRY" | "EVALUATE" |
        "EXIT" | "GOBACK" | "GENERATE" | "IF" | "INITIALIZE" | "INSPECT" |
        "INVOKE" | "MERGE" | "MOVE" | "MULTIPLY" | "OPEN" | "PERFORM" |
        "READ" | "RELEASE" | "RETURN" | "REWRITE" | "SEARCH" | "SET" |
        "SORT" | "START" | "STOP" | "STRING" | "SUBTRACT" | "UNSTRING" |
        "WRITE" | "COPY" | "REPLACE" | "EXEC" | "SELECT" | "ASSIGN" |
        "RECORD" | "LABEL" | "VALUE" | "OCCURS" | "REDEFINES" | "USAGE" |
        "WORKING" | "LINKAGE" | "PROCEDURE" | "DATA" | "FILE" | "PROGRAM" |
        "AUTHOR" | "DATE" | "ENVIRONMENT" | "CONFIGURATION" | "INPUT" |
        "OUTPUT" | "SPECIAL" | "SOURCE" | "OBJECT" | "FD" | "SD" | "RD" |
        "SECTION" | "DIVISION"
    )
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
