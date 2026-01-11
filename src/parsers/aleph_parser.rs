//! AlephTree/cobolparser based COBOL parser
//!
//! Uses the cobolparser crate which generates an AlephTree AST.
//! This approach may be stricter about COBOL column formatting.

use super::{features, normalize_cobol_source, CobolParser, ParserConfig, ParserError};
use crate::model::*;
use aleph_syntax_tree::syntax::AlephTree;
use std::path::Path;
use std::time::Instant;

/// COBOL reserved words and scope terminators that should not be treated as paragraph names
const COBOL_RESERVED_WORDS: &[&str] = &[
    // Scope terminators
    "END-IF",
    "END-READ",
    "END-WRITE",
    "END-REWRITE",
    "END-DELETE",
    "END-START",
    "END-EVALUATE",
    "END-PERFORM",
    "END-SEARCH",
    "END-CALL",
    "END-COMPUTE",
    "END-DIVIDE",
    "END-MULTIPLY",
    "END-ADD",
    "END-SUBTRACT",
    "END-STRING",
    "END-UNSTRING",
    "END-ACCEPT",
    "END-DISPLAY",
    "END-RETURN",
    "END-EXEC",
    "END-PROGRAM",
    // Control statements
    "EXIT",
    "STOP",
    "END",
    "GOBACK",
    "CONTINUE",
    // Compiler directives
    "EJECT",
    "SKIP1",
    "SKIP2",
    "SKIP3",
    "COPY",
    "REPLACE",
    // Other reserved words that might appear alone on a line
    "ELSE",
    "WHEN",
    "OTHER",
    "THEN",
];

/// Check if a name is a COBOL reserved word that shouldn't be a paragraph
fn is_cobol_reserved_word(name: &str) -> bool {
    COBOL_RESERVED_WORDS.contains(&name.to_uppercase().as_str())
}

/// AlephTree based COBOL parser
pub struct AlephCobolParser;

impl AlephCobolParser {
    /// Create a new AlephTree parser instance
    pub fn new() -> Result<Self, ParserError> {
        Ok(Self)
    }

    /// Extract information from the AlephTree AST
    fn extract_from_ast(
        &self,
        ast: &AlephTree,
        source: &str,
        filename: &str,
        config: &ParserConfig,
    ) -> ParseResult {
        let mut warnings = Vec::new();

        // Extract program ID from AST or fallback to filename
        let program_id = self.extract_program_id(ast).unwrap_or_else(|| {
            warnings.push("Could not extract PROGRAM-ID from AlephTree".to_string());
            Path::new(filename)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("UNKNOWN")
                .to_uppercase()
        });

        let urn = generate_urn(&ObjectType::Program, &program_id, &config.app, config.urn_format);
        let mut program = Program::new(urn, program_id.clone(), "COBOL".to_string());
        program.base.origin = Origin {
            file: filename.to_string(),
            line: Some(1),
            column: Some(1),
        };

        // Extract divisions by scanning the source text
        program.divisions = self.extract_divisions_from_source(source);

        // Extract procedures
        let procedures = self.extract_procedures_from_source(source, &program_id, config);
        program.procedures = procedures.iter().map(|p| p.base.name.clone()).collect();

        // Extract data definitions
        program.data_definitions = self.extract_data_definitions_from_source(source);

        // Extract file operations
        let (reads, writes) = self.extract_file_operations_from_source(source);
        program.reads_entities = reads;
        program.writes_entities = writes;

        // Extract CALL statements
        program.calls = self.extract_calls_from_source(source);

        // Extract COPY statements
        program.copybooks = self.extract_copybooks_from_source(source);

        ParseResult {
            program,
            entities: Vec::new(),
            screens: Vec::new(),
            procedures,
            errors: Vec::new(),
            warnings,
            parser_name: self.name().to_string(),
            parse_time_ms: 0,
        }
    }

    /// Extract PROGRAM-ID from the AST
    fn extract_program_id(&self, ast: &AlephTree) -> Option<String> {
        // The AlephTree structure depends on the parsed COBOL
        // Try to find PROGRAM-ID in the AST text representation
        let text = format!("{:?}", ast);

        // Look for program-id pattern in the debug output
        if let Some(idx) = text.to_uppercase().find("PROGRAM-ID") {
            let rest = &text[idx..];
            // Find the program name after PROGRAM-ID
            let words: Vec<&str> = rest.split_whitespace().collect();
            if words.len() >= 2 {
                let name = words[1].trim_matches(|c| c == '.' || c == '"' || c == '\'' || c == ',' || c == ')');
                if !name.is_empty() && name.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false) {
                    return Some(name.to_uppercase());
                }
            }
        }
        None
    }

    /// Extract divisions by scanning source text
    fn extract_divisions_from_source(&self, source: &str) -> Vec<Division> {
        let mut divisions = Vec::new();
        let division_names = ["IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE"];

        for (line_num, line) in source.lines().enumerate() {
            let upper = line.to_uppercase();
            for div_name in &division_names {
                if upper.contains(&format!("{} DIVISION", div_name)) {
                    divisions.push(Division {
                        name: format!("{} DIVISION", div_name),
                        start_line: line_num + 1,
                        end_line: None,
                    });
                    break;
                }
            }
        }

        // Set end lines
        for i in 0..divisions.len() {
            if i + 1 < divisions.len() {
                divisions[i].end_line = Some(divisions[i + 1].start_line - 1);
            }
        }

        divisions
    }

    /// Extract procedures from source text
    fn extract_procedures_from_source(
        &self,
        source: &str,
        program_id: &str,
        config: &ParserConfig,
    ) -> Vec<Procedure> {
        let mut procedures = Vec::new();
        let mut in_procedure_division = false;
        let mut prev_line_complete = true; // Track if previous statement was complete

        for (line_num, line) in source.lines().enumerate() {
            let upper = line.to_uppercase();
            let trimmed = upper.trim();

            // Skip empty lines and comments (column 7 = *)
            if trimmed.is_empty() {
                continue;
            }
            if line.len() > 6 && line.chars().nth(6) == Some('*') {
                continue;
            }

            if trimmed.contains("PROCEDURE DIVISION") {
                in_procedure_division = true;
                prev_line_complete = true;
                continue;
            }

            if !in_procedure_division {
                continue;
            }

            // Look for paragraph/section headers (start in column 8, end with period)
            // In COBOL, paragraphs start at column 8 (Area A) and end with a period
            // A paragraph name must be on a line where the previous statement was complete
            if trimmed.ends_with('.') && !trimmed.contains(' ') && prev_line_complete {
                let name = trimmed.trim_end_matches('.').to_string();
                if !name.is_empty()
                    && name.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false)
                    && !is_cobol_reserved_word(&name)
                {
                    let urn = generate_urn(
                        &ObjectType::Procedure,
                        &format!("{}:{}", program_id, name),
                        &config.app,
                        config.urn_format,
                    );

                    let mut proc = Procedure::new(urn, name);
                    proc.source_start_line = Some(line_num + 1);
                    procedures.push(proc);
                }
            }

            // Update prev_line_complete: statement is complete if line ends with period
            prev_line_complete = trimmed.ends_with('.');

            // Also look for SECTION headers
            if trimmed.contains(" SECTION") || trimmed.ends_with(" SECTION.") {
                let parts: Vec<&str> = trimmed.split_whitespace().collect();
                if !parts.is_empty() {
                    let name = parts[0].to_string();
                    if !name.is_empty() && name.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false) {
                        let urn = generate_urn(
                            &ObjectType::Procedure,
                            &format!("{}:{}", program_id, name),
                            &config.app,
                            config.urn_format,
                        );

                        let mut proc = Procedure::new(urn, name);
                        proc.source_start_line = Some(line_num + 1);
                        procedures.push(proc);
                    }
                }
            }
        }

        // Set end lines
        for i in 0..procedures.len() {
            if i + 1 < procedures.len() {
                if let Some(next_start) = procedures[i + 1].source_start_line {
                    procedures[i].source_end_line = Some(next_start - 1);
                }
            }
        }

        procedures
    }

    /// Extract data definitions from source text
    fn extract_data_definitions_from_source(&self, source: &str) -> Vec<DataDefinition> {
        let mut definitions = Vec::new();
        let mut in_data_division = false;
        let mut in_working_storage = false;

        for line in source.lines() {
            let upper = line.to_uppercase();
            let trimmed = upper.trim();

            if trimmed.contains("DATA DIVISION") {
                in_data_division = true;
                continue;
            }

            if trimmed.contains("PROCEDURE DIVISION") {
                break;
            }

            if !in_data_division {
                continue;
            }

            if trimmed.contains("WORKING-STORAGE SECTION") || trimmed.contains("FILE SECTION") {
                in_working_storage = true;
                continue;
            }

            if !in_working_storage {
                continue;
            }

            // Look for data definitions (start with level number)
            let parts: Vec<&str> = trimmed.split_whitespace().collect();
            if parts.len() >= 2 {
                if let Ok(level) = parts[0].parse::<u8>() {
                    if level > 0 && level <= 88 {
                        let name = parts[1].trim_end_matches('.').to_string();

                        // Extract PICTURE clause
                        let picture = if upper.contains("PIC") {
                            upper.find("PIC")
                                .and_then(|i| upper[i..].split_whitespace().nth(1))
                                .map(|s| s.trim_end_matches('.').to_string())
                        } else {
                            None
                        };

                        // Extract VALUE clause
                        let value = if upper.contains("VALUE") {
                            upper.find("VALUE")
                                .map(|i| {
                                    let rest = &line[i + 5..].trim();
                                    rest.trim_start_matches("IS")
                                        .trim()
                                        .split_whitespace()
                                        .next()
                                        .unwrap_or("")
                                        .trim_end_matches('.')
                                        .to_string()
                                })
                        } else {
                            None
                        };

                        definitions.push(DataDefinition {
                            name,
                            level,
                            picture,
                            usage: None,
                            value,
                            redefines: None,
                            occurs: None,
                            children: Vec::new(),
                        });
                    }
                }
            }
        }

        definitions
    }

    /// Extract file operations from source text
    fn extract_file_operations_from_source(&self, source: &str) -> (Vec<String>, Vec<String>) {
        let mut reads = Vec::new();
        let mut writes = Vec::new();

        for line in source.lines() {
            let upper = line.to_uppercase();
            let trimmed = upper.trim();

            if trimmed.starts_with("READ ") {
                let parts: Vec<&str> = trimmed.split_whitespace().collect();
                if parts.len() >= 2 {
                    reads.push(parts[1].to_string());
                }
            } else if trimmed.starts_with("WRITE ") {
                let parts: Vec<&str> = trimmed.split_whitespace().collect();
                if parts.len() >= 2 {
                    writes.push(parts[1].to_string());
                }
            }
        }

        reads.sort();
        reads.dedup();
        writes.sort();
        writes.dedup();
        (reads, writes)
    }

    /// Extract CALL statements from source text
    fn extract_calls_from_source(&self, source: &str) -> Vec<String> {
        let mut calls = Vec::new();

        for line in source.lines() {
            let upper = line.to_uppercase();
            let trimmed = upper.trim();

            if trimmed.contains("CALL ") {
                // Find the CALL target
                if let Some(idx) = trimmed.find("CALL ") {
                    let rest = &trimmed[idx + 5..];
                    let target = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_matches(|c| c == '"' || c == '\'' || c == '.')
                        .to_string();

                    if !target.is_empty() {
                        calls.push(target);
                    }
                }
            }
        }

        calls.sort();
        calls.dedup();
        calls
    }

    /// Extract COPY statements from source text
    fn extract_copybooks_from_source(&self, source: &str) -> Vec<String> {
        let mut copybooks = Vec::new();

        for line in source.lines() {
            let upper = line.to_uppercase();
            let trimmed = upper.trim();

            if trimmed.contains("COPY ") {
                if let Some(idx) = trimmed.find("COPY ") {
                    let rest = &trimmed[idx + 5..];
                    let copybook = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_end_matches('.')
                        .to_string();

                    if !copybook.is_empty() {
                        copybooks.push(copybook);
                    }
                }
            }
        }

        copybooks.sort();
        copybooks.dedup();
        copybooks
    }
}

impl CobolParser for AlephCobolParser {
    fn name(&self) -> &str {
        "cobolparser-aleph"
    }

    fn parse_file(&self, path: &Path, config: &ParserConfig) -> Result<ParseResult, ParserError> {
        let source = std::fs::read_to_string(path)?;
        let filename = path.to_string_lossy().to_string();
        self.parse_source(&source, &filename, config)
    }

    fn parse_source(
        &self,
        source: &str,
        filename: &str,
        config: &ParserConfig,
    ) -> Result<ParseResult, ParserError> {
        let start = Instant::now();

        // Normalize source to handle non-standard sequence numbers and change markers
        let normalized_source = normalize_cobol_source(source);

        // Use cobolparser to parse the source
        // Note: cobolparser::parse returns AlephTree directly, not Result
        let ast = cobolparser::parse(normalized_source.clone());

        let mut result = self.extract_from_ast(&ast, &normalized_source, filename, config);
        result.parse_time_ms = start.elapsed().as_millis() as u64;

        Ok(result)
    }

    fn supports_feature(&self, feature: &str) -> bool {
        matches!(
            feature,
            features::PROGRAM_ID
                | features::PROCEDURES
                | features::CALLS
                | features::FILES
                | features::COPYBOOKS
                | features::DIVISIONS
                | features::DATA_DEFINITIONS
        )
    }
}
