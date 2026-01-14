//! Tree-sitter based COBOL parser
//!
//! Uses the tree-sitter-cobol grammar to parse COBOL source code
//! into an AST, then extracts IR objects from the AST.

use super::{features, normalize_cobol_source, CobolParser, ParserConfig, ParserError};
use crate::model::*;
use std::path::Path;
use std::time::Instant;
use tree_sitter::{Parser, Tree, Node, TreeCursor};
use regex::Regex;

/// Tree-sitter based COBOL parser
pub struct TreeSitterCobolParser {
    #[allow(dead_code)]
    parser: Parser,
}

impl TreeSitterCobolParser {
    /// Create a new tree-sitter parser instance
    pub fn new() -> Result<Self, ParserError> {
        let mut parser = Parser::new();
        let language = tree_sitter_cobol::language();
        parser
            .set_language(&language)
            .map_err(|e| ParserError::Grammar(format!("Failed to load COBOL grammar: {}", e)))?;

        Ok(Self { parser })
    }

    /// Parse source code into a tree-sitter AST
    #[allow(dead_code)]
    fn parse_to_tree(&mut self, source: &str) -> Result<Tree, ParserError> {
        self.parser
            .parse(source, None)
            .ok_or_else(|| ParserError::Parse("Failed to parse source code".to_string()))
    }

    /// Extract all information from the AST
    fn extract_from_tree(
        &self,
        tree: &Tree,
        source: &[u8],
        filename: &str,
        config: &ParserConfig,
    ) -> ParseResult {
        let root = tree.root_node();
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // Extract program ID
        let program_id = self.extract_program_id(&root, source).unwrap_or_else(|| {
            warnings.push("Could not extract PROGRAM-ID".to_string());
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

        // Extract divisions
        program.divisions = self.extract_divisions(&root, source);

        // Extract procedures (paragraphs and sections)
        let procedures = self.extract_procedures(&root, source, &program_id, config);
        program.procedures = procedures.iter().map(|p| p.base.name.clone()).collect();

        // Extract data definitions
        program.data_definitions = self.extract_data_definitions(&root, source);

        // Extract file references
        let (reads, writes) = self.extract_file_operations(&root, source);
        program.reads_entities = reads;
        program.writes_entities = writes;

        // Extract CALL statements
        program.calls = self.extract_calls(&root, source);

        // Extract COPY statements
        program.copybooks = self.extract_copybooks(&root, source);

        // Check for parse errors in the AST
        self.collect_errors(&root, source, &mut errors);

        ParseResult {
            program,
            entities: Vec::new(),
            screens: Vec::new(),
            procedures,
            errors,
            warnings,
            parser_name: self.name().to_string(),
            parse_time_ms: 0, // Set by caller
        }
    }

    /// Extract PROGRAM-ID from the AST
    fn extract_program_id(&self, root: &Node, source: &[u8]) -> Option<String> {
        let mut cursor = root.walk();
        self.find_program_id_recursive(&mut cursor, source)
    }

    fn find_program_id_recursive(&self, cursor: &mut TreeCursor, source: &[u8]) -> Option<String> {
        loop {
            let node = cursor.node();
            let kind = node.kind();

            // Look for program_id or identification_division patterns
            if kind.contains("program") && kind.contains("id") {
                // Try to extract the actual name
                if let Some(name_node) = node.child_by_field_name("name") {
                    return Some(self.node_text(&name_node, source).to_uppercase());
                }
                // Try child nodes
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        let text = self.node_text(&child, source);
                        if !text.is_empty() && !text.contains("PROGRAM-ID") && text.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_') {
                            return Some(text.to_uppercase());
                        }
                    }
                }
            }

            // Also check for word nodes that might be program names
            if kind == "WORD" || kind == "word" || kind == "program_name" || kind == "identifier" {
                let parent = node.parent();
                if let Some(p) = parent {
                    if p.kind().contains("program") {
                        return Some(self.node_text(&node, source).to_uppercase());
                    }
                }
            }

            // Recurse into children
            if cursor.goto_first_child() {
                if let Some(result) = self.find_program_id_recursive(cursor, source) {
                    return Some(result);
                }
                cursor.goto_parent();
            }

            if !cursor.goto_next_sibling() {
                break;
            }
        }
        None
    }

    /// Extract division markers
    fn extract_divisions(&self, root: &Node, _source: &[u8]) -> Vec<Division> {
        let mut divisions = Vec::new();
        let mut cursor = root.walk();

        // Tree-sitter-cobol uses these specific division node types
        let division_types = [
            ("identification_division", "IDENTIFICATION"),
            ("environment_division", "ENVIRONMENT"),
            ("data_division", "DATA"),
            ("procedure_division", "PROCEDURE"),
        ];

        self.walk_tree(&mut cursor, |node| {
            let kind = node.kind();

            for (node_type, div_name) in &division_types {
                if kind == *node_type {
                    divisions.push(Division {
                        name: format!("{} DIVISION", div_name),
                        start_line: node.start_position().row + 1,
                        end_line: Some(node.end_position().row + 1),
                    });
                    break;
                }
            }
        });

        divisions
    }

    /// Extract procedures (paragraphs and sections) with full semantic analysis
    fn extract_procedures(
        &self,
        root: &Node,
        source: &[u8],
        program_id: &str,
        config: &ParserConfig,
    ) -> Vec<Procedure> {
        let source_str = String::from_utf8_lossy(source);
        let source_lines: Vec<&str> = source_str.lines().collect();

        // First pass: collect all procedure positions
        let mut proc_positions: Vec<(String, usize, usize)> = Vec::new();
        let mut cursor = root.walk();

        self.walk_tree(&mut cursor, |node| {
            let kind = node.kind();
            if kind == "paragraph_header" || kind == "section_header" {
                if let Some(name) = self.extract_procedure_name(node, source) {
                    let start = node.start_position().row;
                    let end = node.end_position().row;
                    proc_positions.push((name, start, end));
                }
            }
        });

        // Sort by start line
        proc_positions.sort_by_key(|p| p.1);

        // Second pass: extract body and analyze each procedure
        let mut procedures = Vec::new();

        for (i, (name, start_line, _)) in proc_positions.iter().enumerate() {
            let urn = generate_urn(
                &ObjectType::Procedure,
                &format!("{}:{}", program_id, name),
                &config.app,
                config.urn_format,
            );

            let mut proc = Procedure::new(urn, name.clone());
            proc.source_start_line = Some(*start_line + 1);

            // Determine end line (next procedure or end of source)
            let end_line = if i + 1 < proc_positions.len() {
                proc_positions[i + 1].1
            } else {
                source_lines.len()
            };
            proc.source_end_line = Some(end_line);

            // Extract procedure body
            let body_lines: Vec<&str> = source_lines
                .get(*start_line..end_line.min(source_lines.len()))
                .unwrap_or(&[])
                .to_vec();
            let body = body_lines.join("\n");

            // Store code body (limited to 2000 chars for efficiency)
            proc.code_body = Some(if body.len() > 2000 {
                body[..2000].to_string()
            } else {
                body.clone()
            });

            // Analyze the body
            proc.performs = self.extract_performs(&body);
            proc.calls = self.extract_procedure_calls(&body);
            proc.business_rules = self.extract_business_rules(&body);
            proc.reason_codes = self.extract_reason_codes(&body);
            proc.reads = self.extract_reads(&body);
            proc.writes = self.extract_writes(&body);

            // Generate summary
            proc.summary = Some(self.generate_procedure_summary(&proc));

            procedures.push(proc);
        }

        procedures
    }

    /// Extract PERFORM targets from procedure body
    fn extract_performs(&self, body: &str) -> Vec<String> {
        let mut performs = Vec::new();
        let re = Regex::new(r"(?i)PERFORM\s+([A-Z0-9][\w-]+)").unwrap();

        for cap in re.captures_iter(body) {
            if let Some(target) = cap.get(1) {
                let name = target.as_str().to_uppercase();
                // Skip VARYING, UNTIL, etc.
                if !["VARYING", "UNTIL", "TIMES", "WITH", "TEST"].contains(&name.as_str()) {
                    if !performs.contains(&name) {
                        performs.push(name);
                    }
                }
            }
        }
        performs
    }

    /// Extract CALL statements from procedure body
    fn extract_procedure_calls(&self, body: &str) -> Vec<String> {
        let mut calls = Vec::new();
        let re = Regex::new(r#"(?i)CALL\s+["']([A-Z0-9]+)["']"#).unwrap();

        for cap in re.captures_iter(body) {
            if let Some(target) = cap.get(1) {
                let name = target.as_str().to_uppercase();
                if !calls.contains(&name) {
                    calls.push(name);
                }
            }
        }
        calls
    }

    /// Extract business rules from IF conditions
    fn extract_business_rules(&self, body: &str) -> Vec<BusinessRule> {
        let mut rules = Vec::new();
        let re = Regex::new(r#"(?i)IF\s+([A-Z0-9][\w-]*)\s*(=|NOT\s*=|>|<|>=|<=|NOT\s+EQUAL|EQUAL)\s*(["']?[\w\s-]+["']?)"#).unwrap();

        for cap in re.captures_iter(body) {
            if let (Some(var), Some(op), Some(val)) = (cap.get(1), cap.get(2), cap.get(3)) {
                let variable = var.as_str().to_uppercase();
                // Skip loop variables and counters
                if !variable.starts_with("WS-CNT") && !variable.starts_with("WS-IDX") {
                    let value_str = val.as_str().trim();
                    let clean_value = value_str
                        .trim_matches('"')
                        .trim_matches(|c| c == '\'' || c == '"')
                        .to_string();
                    rules.push(BusinessRule {
                        variable,
                        operator: op.as_str().to_uppercase(),
                        value: clean_value,
                        raw_condition: cap.get(0).map(|m| m.as_str().to_string()).unwrap_or_default(),
                    });
                }
            }
        }

        // Limit to 10 rules per procedure
        rules.truncate(10);
        rules
    }

    /// Extract reason/error codes
    fn extract_reason_codes(&self, body: &str) -> Vec<String> {
        let mut codes = Vec::new();
        let re = Regex::new(r"(?i)(RSN\d{4}|ERR\d{3,4}|[A-Z]{3}\d{4})").unwrap();

        for cap in re.captures_iter(body) {
            if let Some(code) = cap.get(1) {
                let code_str = code.as_str().to_uppercase();
                if !codes.contains(&code_str) {
                    codes.push(code_str);
                }
            }
        }
        codes
    }

    /// Extract READ operations from procedure body
    fn extract_reads(&self, body: &str) -> Vec<String> {
        let mut reads = Vec::new();
        let re = Regex::new(r"(?i)READ\s+([A-Z0-9][\w-]+)").unwrap();

        for cap in re.captures_iter(body) {
            if let Some(table) = cap.get(1) {
                let name = table.as_str().to_uppercase();
                if !reads.contains(&name) {
                    reads.push(name);
                }
            }
        }
        reads
    }

    /// Extract WRITE operations from procedure body
    fn extract_writes(&self, body: &str) -> Vec<String> {
        let mut writes = Vec::new();
        let re = Regex::new(r"(?i)WRITE\s+([A-Z0-9][\w-]+)").unwrap();

        for cap in re.captures_iter(body) {
            if let Some(table) = cap.get(1) {
                let name = table.as_str().to_uppercase();
                if !writes.contains(&name) {
                    writes.push(name);
                }
            }
        }
        writes
    }

    /// Generate natural language summary of procedure
    fn generate_procedure_summary(&self, proc: &Procedure) -> String {
        let mut parts = vec![format!("Procedure {}", proc.base.name)];

        if !proc.calls.is_empty() {
            parts.push(format!("calls {}", proc.calls.join(", ")));
        }
        if !proc.performs.is_empty() {
            parts.push(format!("performs {}", proc.performs.iter().take(3).cloned().collect::<Vec<_>>().join(", ")));
        }
        if !proc.reads.is_empty() {
            parts.push(format!("reads {}", proc.reads.join(", ")));
        }
        if !proc.business_rules.is_empty() {
            let rule_count = proc.business_rules.len();
            parts.push(format!("has {} business condition(s)", rule_count));
        }
        if !proc.reason_codes.is_empty() {
            parts.push(format!("uses reason codes {}", proc.reason_codes.join(", ")));
        }

        parts.join("; ")
    }

    /// Extract procedure name from a paragraph_header or section_header node
    fn extract_procedure_name(&self, node: &Node, source: &[u8]) -> Option<String> {
        // The procedure name is typically the first child that is a WORD
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                let child_kind = child.kind();
                if child_kind == "WORD" || child_kind == "word" || child_kind == "identifier" {
                    let text = self.node_text(&child, source);
                    let name = text.trim().trim_end_matches('.').to_uppercase();
                    if !name.is_empty() && name.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false) {
                        return Some(name);
                    }
                }
            }
        }
        // Fallback: try to parse the full node text
        let text = self.node_text(node, source);
        let parts: Vec<&str> = text.split_whitespace().collect();
        if !parts.is_empty() {
            let name = parts[0].trim_end_matches('.').to_uppercase();
            if !name.is_empty() && name.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false) {
                return Some(name);
            }
        }
        None
    }

    /// Extract data definitions (working-storage items)
    fn extract_data_definitions(&self, root: &Node, source: &[u8]) -> Vec<DataDefinition> {
        let mut definitions = Vec::new();
        let mut cursor = root.walk();

        self.walk_tree(&mut cursor, |node| {
            let kind = node.kind();

            // Look for data item definitions
            if kind.contains("data_item") || kind.contains("data_description") || kind == "data_entry" {
                if let Some(def) = self.parse_data_definition(node, source) {
                    definitions.push(def);
                }
            }
        });

        definitions
    }

    fn parse_data_definition(&self, node: &Node, source: &[u8]) -> Option<DataDefinition> {
        let text = self.node_text(node, source);

        // Try to parse level number
        let level: u8 = text
            .split_whitespace()
            .next()
            .and_then(|s| s.parse().ok())
            .unwrap_or(0);

        if level == 0 {
            return None;
        }

        // Extract name (usually second token)
        let name = text
            .split_whitespace()
            .nth(1)
            .map(|s| s.trim_end_matches('.').to_uppercase())
            .unwrap_or_default();

        if name.is_empty() {
            return None;
        }

        // Extract PICTURE clause
        let picture = if text.to_uppercase().contains("PIC") {
            let upper = text.to_uppercase();
            upper
                .find("PIC")
                .map(|i| {
                    upper[i..]
                        .split_whitespace()
                        .nth(1)
                        .unwrap_or("")
                        .trim_end_matches('.')
                        .to_string()
                })
        } else {
            None
        };

        // Extract VALUE clause
        let value = if text.to_uppercase().contains("VALUE") {
            let upper = text.to_uppercase();
            upper.find("VALUE").map(|i| {
                let rest = &text[i + 5..];
                rest.trim()
                    .trim_start_matches("IS")
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

        Some(DataDefinition {
            name,
            level,
            picture,
            usage: None,
            value,
            redefines: None,
            occurs: None,
            children: Vec::new(),
        })
    }

    /// Extract file operations (READ/WRITE)
    fn extract_file_operations(&self, root: &Node, source: &[u8]) -> (Vec<String>, Vec<String>) {
        let mut reads = Vec::new();
        let mut writes = Vec::new();
        let mut cursor = root.walk();

        self.walk_tree(&mut cursor, |node| {
            let kind = node.kind();
            let text = self.node_text(node, source).to_uppercase();

            if kind.contains("read") || text.starts_with("READ ") {
                // Extract file name from READ statement
                if let Some(file) = text.split_whitespace().nth(1) {
                    reads.push(file.to_string());
                }
            } else if kind.contains("write") || text.starts_with("WRITE ") {
                if let Some(file) = text.split_whitespace().nth(1) {
                    writes.push(file.to_string());
                }
            }
        });

        reads.sort();
        reads.dedup();
        writes.sort();
        writes.dedup();
        (reads, writes)
    }

    /// Extract CALL statements
    fn extract_calls(&self, root: &Node, source: &[u8]) -> Vec<String> {
        let mut calls = Vec::new();
        let mut cursor = root.walk();

        self.walk_tree(&mut cursor, |node| {
            let kind = node.kind();

            if kind.contains("call") {
                // Try to get the called program name
                // The grammar uses "string" for quoted literals like "PROGNAME"
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        let child_kind = child.kind();
                        if child_kind == "string" || child_kind.contains("literal") || child_kind.contains("identifier") {
                            let name = self.node_text(&child, source)
                                .chars()
                                .filter(|c| c.is_alphanumeric() || *c == '-' || *c == '_')
                                .collect::<String>()
                                .to_uppercase();
                            if !name.is_empty() && name != "CALL" {
                                calls.push(name);
                                break;
                            }
                        }
                    }
                }
            }
        });

        calls.sort();
        calls.dedup();
        calls
    }

    /// Extract COPY statements
    fn extract_copybooks(&self, root: &Node, source: &[u8]) -> Vec<String> {
        let mut copybooks = Vec::new();
        let mut cursor = root.walk();

        self.walk_tree(&mut cursor, |node| {
            let kind = node.kind();

            if kind.contains("copy") {
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        let text = self.node_text(&child, source).to_uppercase();
                        if !text.is_empty() && text != "COPY" && !text.contains(".") {
                            copybooks.push(text);
                            break;
                        }
                    }
                }
            }
        });

        copybooks.sort();
        copybooks.dedup();
        copybooks
    }

    /// Collect parse errors from the AST
    fn collect_errors(&self, root: &Node, source: &[u8], errors: &mut Vec<ParseError>) {
        let mut cursor = root.walk();

        self.walk_tree(&mut cursor, |node| {
            if node.is_error() || node.is_missing() {
                errors.push(ParseError {
                    message: format!(
                        "Parse error at node: {}",
                        self.node_text(node, source)
                    ),
                    line: Some(node.start_position().row + 1),
                    column: Some(node.start_position().column + 1),
                    severity: if node.is_error() {
                        ErrorSeverity::Error
                    } else {
                        ErrorSeverity::Warning
                    },
                });
            }
        });
    }

    /// Helper to get node text
    fn node_text<'a>(&self, node: &Node, source: &'a [u8]) -> &'a str {
        node.utf8_text(source).unwrap_or("")
    }

    /// Walk the tree calling the visitor for each node
    fn walk_tree<F>(&self, cursor: &mut TreeCursor, mut visitor: F)
    where
        F: FnMut(&Node),
    {
        loop {
            visitor(&cursor.node());

            if cursor.goto_first_child() {
                continue;
            }

            loop {
                if cursor.goto_next_sibling() {
                    break;
                }
                if !cursor.goto_parent() {
                    return;
                }
            }
        }
    }
}

impl CobolParser for TreeSitterCobolParser {
    fn name(&self) -> &str {
        "tree-sitter-cobol"
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

        // Need mutable parser for parsing
        let mut parser = Parser::new();
        let language = tree_sitter_cobol::language();
        parser
            .set_language(&language)
            .map_err(|e| ParserError::Grammar(format!("Failed to load COBOL grammar: {}", e)))?;

        let tree = parser
            .parse(&normalized_source, None)
            .ok_or_else(|| ParserError::Parse("Failed to parse source code".to_string()))?;

        let mut result = self.extract_from_tree(&tree, normalized_source.as_bytes(), filename, config);
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
                | features::VARIABLES
        )
    }
}
