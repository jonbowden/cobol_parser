# COBOL Parser - Rust Implementation

A Rust-based COBOL parser for legacy code analysis, comparing two parsing approaches:
1. **tree-sitter-cobol**: AST-based parsing using the tree-sitter COBOL grammar
2. **cobolparser**: AlephTree-based parsing (with regex fallback for extraction)

## Purpose

This project is part of the IR_analyser ecosystem for legacy code analysis. It provides:
- An Intermediate Representation (IR) compatible with the Python IR_analyser
- Comparison framework for evaluating different parsing strategies
- Production-ready Rust implementation for performance-critical parsing

## Project Structure

```
cobol_parser/
├── src/
│   ├── lib.rs              # Library entry point
│   ├── main.rs             # CLI application
│   ├── model.rs            # IR object model (Program, Procedure, Entity, etc.)
│   ├── preprocessor.rs     # COBOL source preprocessing (continuation lines, etc.)
│   └── parsers/
│       ├── mod.rs          # Parser trait, factory, and source normalization
│       ├── tree_sitter_parser.rs  # Tree-sitter implementation
│       └── aleph_parser.rs # Cobolparser/AlephTree implementation
├── tree-sitter-cobol-grammar/  # Local tree-sitter COBOL grammar (modified)
├── sources/                    # COBOL source files for testing
│   ├── pgm/               # COBOL programs
│   └── cpy/               # Copybooks
└── examples/
    ├── parse_program.rs   # Example usage
    ├── count_clean.rs     # Count files parsing without errors
    └── show_all_errors.rs # Display parsing errors across files
```

## Usage

### CLI

```bash
# Compare both parsers (default)
cargo run -- sources/pgm/TRFVBAC.cob

# Use specific parser
cargo run -- sources/pgm/TRFVBAC.cob --parser tree-sitter
cargo run -- sources/pgm/TRFVBAC.cob --parser aleph

# Output as JSON
cargo run -- sources/pgm/TRFVBAC.cob --json
```

### Library

```rust
use cobol_parser::{parse_file, compare_parsers, ParserType, ParserConfig};
use std::path::Path;

let config = ParserConfig::default();

// Parse with specific parser
let result = parse_file(Path::new("program.cob"), ParserType::TreeSitter, &config)?;

// Compare both parsers
let comparison = compare_parsers(Path::new("program.cob"), &config)?;
println!("{}", comparison.report());
```

## Parser Comparison

### Tree-sitter-cobol

**Strengths:**
- Robust AST-based parsing
- Accurate node type identification (paragraph_header, section_header)
- Well-tested with NIST COBOL85 test suite
- Incremental parsing support
- EXEC SQL/CICS/DLI block support (via external scanner)

**Source Normalization Features:**
- Tab expansion (8-column tab stops)
- Change marker removal (inline version control markers)
- Continuation line handling
- Multiple space collapsing (reduces line length)
- Split keyword fixing (e.g., "AN D" → "AND")
- Level number indentation correction
- Support for both fixed-format and free-format COBOL

**Limitations:**
- Some complex multi-line statements may not parse correctly
- Source files with typos (e.g., `0I` instead of `01`) will have errors

**Best for:** Production COBOL code with any formatting style

### Cobolparser (AlephTree)

**Strengths:**
- More lenient parsing
- Works with less strictly formatted code
- Fast parsing (regex-based extraction fallback)

**Limitations:**
- May produce false positives (e.g., END-IF as paragraph names)
- Less accurate AST structure
- Sparse documentation (6.9% coverage)

**Best for:** Quick analysis, exploration, code with column formatting issues

## Test Results

### Current Parsing Status (21 test files)

| Status | Count |
|--------|-------|
| Parsing cleanly (0 errors) | 9 |
| Parsing with errors | 12 |
| Total error nodes | 34 |

**Clean files:** TRFVGLAC, TRFVCUYP, TRFVBACU, TRFXGSPA, GHOINSSTPL, TRFVBAC, TRFVTAG57, TRFVTB3, TRFVLMT

### TRFVBAC.cob (well-formatted, small file)

| Metric | Tree-sitter | Cobolparser |
|--------|-------------|-------------|
| Parse Time | 6 ms | 1 ms |
| Procedures Found | 7 | 7 |
| Copybooks | 5 | 5 |
| Accuracy | High | High |

Both parsers correctly identified all 7 procedures:
- MAIN-MODULE
- A000-PROCESS-CALLED-ROUTINE
- A080-MOVE-DATA
- A099-PROCESS-CALLED-ROUTINE-EX
- Y900-ABNORMAL-TERMINATION
- Z000-END-PROGRAM-ROUTINE
- Z999-END-PROGRAM-ROUTINE-EX

### Remaining Error Types

The 34 remaining errors across 12 files are caused by:
- **Source typos**: e.g., `0I` instead of `01` in level numbers
- **Complex multi-line statements**: Conditions spanning multiple lines
- **Non-standard VALUE/PIC ordering**: `VALUE "X". PIC X(20)` instead of `PIC X(20) VALUE "X"`
- **Comment edge cases**: Unusual comment marker combinations

## Object Model

The IR object model is compatible with IR_analyser (Python):

```rust
// Program representation
pub struct Program {
    pub base: IRObject,          // URN, type, name, origin
    pub language: String,        // "COBOL"
    pub procedures: Vec<String>, // Paragraph/section names
    pub calls: Vec<String>,      // External CALL targets
    pub copybooks: Vec<String>,  // COPY references
    pub data_definitions: Vec<DataDefinition>,
    pub divisions: Vec<Division>,
    // ... more fields
}

// URN formats supported
// Legacy:   urn:{type}:{namespace}:{name}
// Platform: urn:ir:{app}:{type}:{name}
```

## Building

```bash
# Build
cargo build --release

# Run tests
cargo test

# Run benchmark (TODO)
cargo bench
```

## Dependencies

- `tree-sitter` (0.24) - Parsing library
- `tree-sitter-cobol` (local) - COBOL grammar
- `cobolparser` (0.1) - AlephTree parser
- `serde` / `serde_json` - Serialization
- `anyhow` / `thiserror` - Error handling

## Related Projects

- [IR_analyser](../IR_analyser) - Python-based legacy code analysis platform
- [tree-sitter-cobol](https://github.com/yutaro-sakamoto/tree-sitter-cobol) - COBOL85 grammar

## Comparison with Python

| Aspect | Rust (this project) | Python (IR_analyser) |
|--------|---------------------|----------------------|
| Parse Speed | ~6ms (small file) | ~50ms (estimated) |
| Memory | Lower | Higher |
| Deployment | Single binary | Requires Python runtime |
| Tree-sitter | Native support | Via ctypes FFI |
| Maturity | New | Production-ready |

## Recent Improvements

- **EXEC SQL/CICS/DLI support** - Grammar modified to use external scanner for multi-line embedded SQL blocks
- **Tab expansion** - Source files with tabs are properly handled
- **Space collapsing** - Excessive whitespace in source is collapsed to reduce line length
- **Change marker handling** - Inline version control markers (e.g., `GP3M00`, `5Q1ARV`) are stripped
- **Continuation line handling** - Multi-line strings and statements are properly joined
- **Free-format support** - Both fixed-format and free-format COBOL files are supported

## Next Steps

1. **Add copybook resolution** - Inline COPY statements before parsing
2. **Benchmark suite** - Compare performance systematically
3. **Grammar refinements** - Handle remaining edge cases (multi-line conditions, VALUE before PIC)
4. **Error recovery** - Better handling of source files with typos
