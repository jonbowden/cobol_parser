//! COBOL Source Preprocessor
//!
//! Fixes common formatting issues in COBOL source files:
//! - Moves inline sequence numbers from code area to columns 1-6
//! - Converts continuation indicators (- in column 7) to comments (*)
//! - Normalizes column alignment

/// Result of preprocessing a COBOL source file
#[derive(Debug, Clone)]
pub struct PreprocessResult {
    /// The fixed source code
    pub source: String,
    /// Issues that were detected and fixed
    pub fixes: Vec<SourceFix>,
    /// Warnings about potential issues that weren't auto-fixed
    pub warnings: Vec<String>,
}

/// A fix that was applied to the source
#[derive(Debug, Clone)]
pub struct SourceFix {
    pub line: usize,
    pub fix_type: FixType,
    pub description: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FixType {
    /// Moved sequence number from code area to columns 1-6
    MovedSequenceNumber,
    /// Converted continuation indicator to comment
    ContinuationToComment,
    /// Converted pipe indicator to comment
    PipeToComment,
    /// Fixed misaligned comment indicator
    FixedCommentAlignment,
    /// Fixed change marker that spanned into column 7
    FixedChangeMarkerAlignment,
    /// Padded short line to minimum length
    PaddedLine,
    /// Normalized indentation for line without change marker
    NormalizedIndentation,
}

/// Preprocess COBOL source code to fix formatting issues.
///
/// This function:
/// 1. Detects inline sequence numbers in the code area and moves them to columns 1-6
/// 2. Converts continuation indicators (- in column 7) to comments (*)
/// 3. Handles * in column 7 as continuation when previous line has unclosed string
/// 4. Joins string continuation lines to handle multiline strings
/// 5. Ensures proper column alignment
/// 6. Normalizes indentation for files with change markers
pub fn preprocess(source: &str) -> PreprocessResult {
    // First, check if this file uses change markers in columns 1-6
    let has_change_markers = source.lines().any(|line| {
        let chars: Vec<char> = line.chars().collect();
        if chars.len() >= 6 {
            let cols_1_6: String = chars[0..6].iter().collect();
            is_change_marker(&cols_1_6)
        } else {
            false
        }
    });

    let mut result = String::with_capacity(source.len());
    let mut fixes = Vec::new();
    let mut warnings = Vec::new();
    let mut prev_line_has_unclosed_string = false;
    let mut accumulated_line = String::new();
    let mut _accumulated_line_num = 0usize;

    for (line_num, line) in source.lines().enumerate() {
        let line_number = line_num + 1;
        let (fixed_line, line_fixes, line_warnings) =
            preprocess_line(line, line_number, prev_line_has_unclosed_string, has_change_markers);

        fixes.extend(line_fixes);
        warnings.extend(line_warnings);

        // Check if this line is a string continuation
        let chars: Vec<char> = fixed_line.chars().collect();
        let is_continuation = chars.len() > 6 && chars[6] == '-';

        if prev_line_has_unclosed_string && is_continuation {
            // This is a continuation line - join with previous
            // Extract the continuation content (column 8+, trimmed of leading spaces up to quote)
            if chars.len() > 7 {
                let cont_content: String = chars[7..].iter().collect();
                // Append to accumulated line, removing the unclosed string part
                accumulated_line.push_str(cont_content.trim_start());
            }
            // Check if this line closes the string
            prev_line_has_unclosed_string = has_unclosed_string(&accumulated_line);

            // If string is now closed, emit the accumulated line
            if !prev_line_has_unclosed_string {
                result.push_str(&accumulated_line);
                result.push('\n');
                accumulated_line.clear();
            }
        } else {
            // Not a continuation - emit any accumulated line first
            if !accumulated_line.is_empty() {
                result.push_str(&accumulated_line);
                result.push('\n');
                accumulated_line.clear();
            }

            // Check if this line has an unclosed string
            prev_line_has_unclosed_string = has_unclosed_string(&fixed_line);

            if prev_line_has_unclosed_string {
                // Start accumulating
                accumulated_line = fixed_line;
                _accumulated_line_num = line_number;
            } else {
                result.push_str(&fixed_line);
                result.push('\n');
            }
        }
    }

    // Emit any remaining accumulated line
    if !accumulated_line.is_empty() {
        result.push_str(&accumulated_line);
        result.push('\n');
    }

    // Second pass: join orphaned PIC clauses with previous data definitions
    let joined = join_orphaned_clauses(&result);

    // Third pass: strip IBM i DATABASE- prefix from ASSIGN clauses
    let final_source = strip_database_prefix(&joined);

    PreprocessResult {
        source: final_source,
        fixes,
        warnings,
    }
}

/// Strip IBM i DATABASE- prefix from ASSIGN TO clauses.
/// DATABASE-filename is an IBM i/AS400 syntax that maps to DB2/400 files.
fn strip_database_prefix(source: &str) -> String {
    let mut result = String::with_capacity(source.len());

    for line in source.lines() {
        let upper = line.to_uppercase();
        if upper.contains("ASSIGN") && upper.contains("DATABASE-") {
            // Replace DATABASE- with empty string (case-insensitive)
            let mut new_line = line.to_string();
            if let Some(pos) = upper.find("DATABASE-") {
                new_line = format!("{}{}", &line[..pos], &line[pos + 9..]);
            }
            result.push_str(&new_line);
        } else {
            result.push_str(line);
        }
        result.push('\n');
    }

    result
}

/// Join orphaned clauses (like PIC X(20).) with the previous data definition
fn join_orphaned_clauses(source: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let mut result = String::with_capacity(source.len());
    let mut skip_next = false;

    for i in 0..lines.len() {
        if skip_next {
            skip_next = false;
            continue;
        }

        let line = lines[i];
        let next_line = lines.get(i + 1);

        // Check if next line is an orphaned clause (starts with whitespace and PIC/VALUE/OCCURS etc.)
        if let Some(next) = next_line {
            let next_trimmed = next.trim();
            // Check if next line is an orphaned clause - starts with just a data clause keyword
            if is_orphaned_clause(next_trimmed) {
                // Check if current line ends with a data definition (level number + name + clauses)
                let line_trimmed = line.trim();
                if !line_trimmed.is_empty() && !line_trimmed.starts_with('*') {
                    // Join the lines
                    result.push_str(line);
                    result.push(' ');
                    result.push_str(next_trimmed);
                    result.push('\n');
                    skip_next = true;
                    continue;
                }
            }
        }

        result.push_str(line);
        result.push('\n');
    }

    result
}

/// Check if a line is an orphaned data clause
fn is_orphaned_clause(line: &str) -> bool {
    let upper = line.to_uppercase();
    // Orphaned clause: starts with a data clause keyword, not a level number
    // These are clauses that should be on the same line as their data definition
    upper.starts_with("PIC ")
        || upper.starts_with("PIC(")
        || upper.starts_with("PICTURE ")
        || upper.starts_with("VALUE ")
        || upper.starts_with("VALUES ")
        || upper.starts_with("OCCURS ")
        || upper.starts_with("REDEFINES ")
        || upper.starts_with("USAGE ")
}

/// Check if a line has an unclosed string literal
fn has_unclosed_string(line: &str) -> bool {
    // Get the code area (column 8+) - skip sequence numbers and indicator
    let chars: Vec<char> = line.chars().collect();
    let code = if chars.len() > 7 {
        chars[7..].iter().collect::<String>()
    } else {
        return false;
    };

    // Check indicator - if it's a comment, no unclosed string
    if chars.len() > 6 && (chars[6] == '*' || chars[6] == '/') {
        return false;
    }

    // Count quotes, accounting for doubled quotes as escapes
    let mut in_string = false;
    let mut i = 0;
    let code_chars: Vec<char> = code.chars().collect();

    while i < code_chars.len() {
        let ch = code_chars[i];
        if ch == '"' {
            // Check if this is a doubled quote (escape)
            if i + 1 < code_chars.len() && code_chars[i + 1] == '"' {
                i += 2; // Skip both quotes
                continue;
            }
            in_string = !in_string;
        }
        i += 1;
    }

    in_string
}

/// Preprocess a single line of COBOL source
fn preprocess_line(
    line: &str,
    line_number: usize,
    prev_line_has_unclosed_string: bool,
    file_has_change_markers: bool,
) -> (String, Vec<SourceFix>, Vec<String>) {
    let mut fixes = Vec::new();
    let warnings = Vec::new();

    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();

    // Empty line - just return as-is
    if len == 0 {
        return (String::new(), fixes, warnings);
    }

    // Convert EJECT and SKIP compiler directives to comments
    // These are listing control directives that tree-sitter doesn't understand
    let trimmed = line.trim();
    let upper_trimmed = trimmed.to_uppercase();
    if upper_trimmed == "EJECT"
        || upper_trimmed.starts_with("SKIP")
        || upper_trimmed == "EJECT."
    {
        // Convert to a comment line
        return (format!("      * {}", trimmed), fixes, warnings);
    }

    // Get current columns
    let cols_1_6: String = if len >= 6 {
        chars[0..6].iter().collect()
    } else {
        format!("{:<6}", line)
    };

    let mut col_7: char = if len > 6 { chars[6] } else { ' ' };

    let code_area: String = if len > 7 {
        chars[7..].iter().collect()
    } else {
        String::new()
    };

    // Track if this line is a string continuation
    let is_string_continuation = prev_line_has_unclosed_string && (col_7 == '*' || col_7 == '-');

    // Check if columns 1-6 are blank and code area starts with a sequence number
    let cols_1_6_blank = cols_1_6.trim().is_empty();
    let this_line_has_marker = is_change_marker(&cols_1_6);

    let mut new_seq_area = cols_1_6.clone();
    let mut new_indicator = col_7;
    let mut new_code = code_area.clone();

    // Check for split sequence number spanning columns 1-7 and beyond
    // This happens when sequence numbers like "10700" are formatted as:
    // cols 1-6: "     1", col 7: "0", col 8+: "700  *code"
    if !cols_1_6_blank && col_7.is_ascii_digit() {
        if let Some((seq_num, remaining, new_ind)) = extract_split_sequence(&cols_1_6, col_7, &code_area) {
            new_seq_area = format!("{:>6}", seq_num);
            new_indicator = new_ind;
            new_code = remaining;

            fixes.push(SourceFix {
                line: line_number,
                fix_type: FixType::MovedSequenceNumber,
                description: format!("Fixed split sequence number '{}' to columns 1-6", seq_num),
            });

            // Check if remaining code starts with comment indicator
            let remaining_trimmed = new_code.trim_start();
            if remaining_trimmed.starts_with('*') || remaining_trimmed.starts_with('/') {
                new_indicator = remaining_trimmed.chars().next().unwrap();
                new_code = format!(" {}", &remaining_trimmed[1..]);

                fixes.push(SourceFix {
                    line: line_number,
                    fix_type: FixType::FixedCommentAlignment,
                    description: "Moved comment indicator to column 7".to_string(),
                });
            }
        }
    }
    // Check for inline sequence number in code area (cols 1-6 blank)
    else if cols_1_6_blank && !code_area.is_empty() {
        if let Some((seq_num, remaining)) = extract_inline_sequence(&code_area) {
            // Move sequence number to columns 1-6
            new_seq_area = format!("{:>6}", seq_num);
            new_code = remaining;

            fixes.push(SourceFix {
                line: line_number,
                fix_type: FixType::MovedSequenceNumber,
                description: format!("Moved sequence number '{}' to columns 1-6", seq_num),
            });

            // Check if remaining code starts with comment indicator
            let remaining_trimmed = new_code.trim_start();
            if remaining_trimmed.starts_with('*') || remaining_trimmed.starts_with('/') {
                new_indicator = remaining_trimmed.chars().next().unwrap();
                new_code = format!(" {}", &remaining_trimmed[1..]);

                fixes.push(SourceFix {
                    line: line_number,
                    fix_type: FixType::FixedCommentAlignment,
                    description: "Moved comment indicator to column 7".to_string(),
                });
            }
        }
    }

    // Handle string continuations - convert * to - to preserve as continuation
    if is_string_continuation && new_indicator == '*' {
        new_indicator = '-';
        fixes.push(SourceFix {
            line: line_number,
            fix_type: FixType::ContinuationToComment,
            description: "Converted * to continuation indicator '-' for string continuation".to_string(),
        });
    }
    // Convert continuation indicator to comment, unless this is a string continuation
    // String continuations need to be preserved for proper parsing
    else if new_indicator == '-' && !is_string_continuation {
        new_indicator = '*';
        fixes.push(SourceFix {
            line: line_number,
            fix_type: FixType::ContinuationToComment,
            description: "Converted continuation indicator '-' to comment '*'".to_string(),
        });
    }

    // Convert pipe indicator to comment (non-standard but used in some sources)
    if new_indicator == '|' {
        new_indicator = '*';
        fixes.push(SourceFix {
            line: line_number,
            fix_type: FixType::PipeToComment,
            description: "Converted pipe indicator '|' to comment '*'".to_string(),
        });
    }

    // Handle case where column 7 contains an alphanumeric that's part of a change marker
    // e.g., "      G2BL00           AND..." where "G2BL00" is a change marker
    // Valid column 7 indicators are: space, *, /, -, D, d
    if new_indicator.is_ascii_alphanumeric() && new_indicator != 'D' && new_indicator != 'd' {
        // Check if column 7 + code area starts with a change marker pattern
        let potential_marker = format!("{}{}", new_indicator, new_code.chars().take(6).collect::<String>());
        let marker_end = potential_marker
            .find(|c: char| !c.is_ascii_alphanumeric())
            .unwrap_or(potential_marker.len());
        let marker = &potential_marker[..marker_end];

        // Change markers are 5-7 alphanumeric chars with at least one digit
        if marker.len() >= 5 && marker.len() <= 7
           && marker.chars().all(|c| c.is_ascii_alphanumeric())
           && marker.chars().any(|c| c.is_ascii_digit()) {
            // This looks like a misaligned change marker - shift it into code area
            new_indicator = ' ';
            new_code = format!("{}{}", marker, &new_code[(marker.len() - 1)..]);
            fixes.push(SourceFix {
                line: line_number,
                fix_type: FixType::FixedChangeMarkerAlignment,
                description: format!("Fixed misaligned change marker '{}' spanning column 7", marker),
            });
        }
    }

    // Normalize indentation for lines without change markers in files that use them
    // When a file uses change markers (e.g., SGX201 in cols 1-6), lines without markers
    // often have 6 extra spaces of indentation to maintain visual alignment.
    // This breaks COBOL column structure, so we remove the extra spaces.
    if file_has_change_markers && !this_line_has_marker && cols_1_6_blank && new_indicator == ' ' {
        // Check if code area starts with 6 spaces (the extra indentation)
        if new_code.len() >= 6 && new_code.chars().take(6).all(|c| c == ' ') {
            // Check that there's actual content after the 6 spaces
            let after_spaces = &new_code[6..];
            if !after_spaces.trim().is_empty() {
                new_code = after_spaces.to_string();
                fixes.push(SourceFix {
                    line: line_number,
                    fix_type: FixType::NormalizedIndentation,
                    description: "Removed 6 extra spaces from line without change marker".to_string(),
                });
            }
        }
    }

    // Build the fixed line
    let fixed_line = format!("{}{}{}", new_seq_area, new_indicator, new_code);

    (fixed_line, fixes, warnings)
}

/// Extract a split sequence number that spans columns 1-7 and possibly into column 8+.
///
/// Example: cols 1-6 = "     1", col 7 = '0', col 8+ = "700  *code"
/// Returns: ("10700", "  *code", ' ')
fn extract_split_sequence(cols_1_6: &str, col_7: char, code_area: &str) -> Option<(String, String, char)> {
    // Get trailing digits from cols 1-6
    let trailing_digits: String = cols_1_6
        .chars()
        .rev()
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    if trailing_digits.is_empty() {
        return None;
    }

    // Combine with col 7
    let mut seq_num = format!("{}{}", trailing_digits, col_7);

    // Get leading digits from code area
    let leading_digits: String = code_area
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect();

    seq_num.push_str(&leading_digits);

    // Sequence numbers should be 3-6 digits (not 1-2 which are level numbers)
    if seq_num.len() < 3 || seq_num.len() > 6 {
        return None;
    }

    // Get the remaining code after the sequence number
    let remaining = &code_area[leading_digits.len()..];

    // Don't treat COBOL level numbers as sequence numbers
    if is_cobol_level_number(&seq_num, remaining) {
        return None;
    }

    // The new indicator should be ' ' (space) unless the remaining starts with indicator
    let new_indicator = ' ';

    Some((seq_num, remaining.to_string(), new_indicator))
}

/// COBOL level numbers that should NOT be treated as sequence numbers.
/// These are valid data definition level indicators.
const COBOL_LEVEL_NUMBERS: &[&str] = &[
    "01", "02", "03", "04", "05", "06", "07", "08", "09",
    "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
    "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
    "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
    "66", "77", "88",
];

/// Check if a number string looks like a COBOL level number rather than a sequence number.
fn is_cobol_level_number(num: &str, remaining_code: &str) -> bool {
    // Level numbers are 01-49, 66, 77, or 88
    if !COBOL_LEVEL_NUMBERS.contains(&num) {
        return false;
    }

    // Level numbers are followed by a data name or FILLER
    let trimmed = remaining_code.trim_start().to_uppercase();

    // Common patterns after level numbers
    trimmed.starts_with("FILLER")
        || trimmed.starts_with("WK-")
        || trimmed.starts_with("WS-")
        || trimmed.starts_with("PIC")
        || trimmed.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false)
}

/// Check if a string in columns 1-6 looks like a change marker.
/// Change markers are typically 5-6 alphanumeric characters containing at least one digit.
/// Examples: SGX201, GP3C00, VASA01, GP4D02
fn is_change_marker(cols_1_6: &str) -> bool {
    let trimmed = cols_1_6.trim();

    // Must be 5-6 alphanumeric characters
    if trimmed.len() < 5 || trimmed.len() > 6 {
        return false;
    }

    // Must be all alphanumeric
    if !trimmed.chars().all(|c| c.is_ascii_alphanumeric()) {
        return false;
    }

    // Must contain at least one digit (to distinguish from words)
    if !trimmed.chars().any(|c| c.is_ascii_digit()) {
        return false;
    }

    // Must contain at least one letter (to distinguish from sequence numbers)
    if !trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
        return false;
    }

    true
}

/// Extract an inline sequence number from the start of the code area.
///
/// Returns (sequence_number, remaining_code) if found.
/// Does NOT extract COBOL level numbers (01-49, 66, 77, 88).
fn extract_inline_sequence(code: &str) -> Option<(String, String)> {
    let trimmed = code.trim_start();

    if trimmed.is_empty() {
        return None;
    }

    // Find the first non-digit character
    let digit_end = trimmed
        .char_indices()
        .find(|(_, c)| !c.is_ascii_digit())
        .map(|(i, _)| i)
        .unwrap_or(trimmed.len());

    if digit_end == 0 {
        return None;
    }

    let seq_num = &trimmed[..digit_end];

    // Sequence numbers are typically 3-6 digits (100, 200, 10700, etc.)
    // NOT 1-2 digits which are usually level numbers
    if seq_num.len() < 3 || seq_num.len() > 6 {
        return None;
    }

    // Must be followed by whitespace (or end of line)
    let after_seq = &trimmed[digit_end..];
    if !after_seq.is_empty() && !after_seq.starts_with(char::is_whitespace) {
        return None;
    }

    // Don't treat COBOL level numbers as sequence numbers
    if is_cobol_level_number(seq_num, after_seq) {
        return None;
    }

    // Calculate original spacing to preserve alignment
    let leading_spaces = code.len() - trimmed.len();
    let remaining = format!("{}{}", " ".repeat(leading_spaces), after_seq);

    Some((seq_num.to_string(), remaining))
}

/// Preprocess a source file in place, returning the fixes applied.
pub fn preprocess_file(path: &std::path::Path) -> Result<PreprocessResult, std::io::Error> {
    let source = std::fs::read_to_string(path)?;
    Ok(preprocess(&source))
}

/// Preprocess and write fixed source back to file.
pub fn fix_file(path: &std::path::Path) -> Result<PreprocessResult, std::io::Error> {
    let result = preprocess_file(path)?;
    std::fs::write(path, &result.source)?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inline_sequence_number() {
        let source = "       100  IDENTIFICATION DIVISION.";
        let result = preprocess(source);

        assert!(result.source.starts_with("   100 "));
        assert!(result.fixes.iter().any(|f| f.fix_type == FixType::MovedSequenceNumber));
    }

    #[test]
    fn test_continuation_to_comment() {
        let source = "      -    CONTINUED LINE.";
        let result = preprocess(source);

        // Column 7 should be * now
        let chars: Vec<char> = result.source.chars().collect();
        assert_eq!(chars[6], '*');
        assert!(result.fixes.iter().any(|f| f.fix_type == FixType::ContinuationToComment));
    }

    #[test]
    fn test_inline_sequence_with_comment() {
        let source = "       600  *This is a comment";
        let result = preprocess(source);

        // Sequence should move to cols 1-6, * should be in col 7
        let chars: Vec<char> = result.source.chars().collect();
        assert_eq!(chars[6], '*');
        assert!(result.fixes.iter().any(|f| f.fix_type == FixType::MovedSequenceNumber));
    }
}
