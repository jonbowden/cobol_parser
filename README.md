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
│   └── parsers/
│       ├── mod.rs          # Parser trait and factory
│       ├── tree_sitter_parser.rs  # Tree-sitter implementation
│       └── aleph_parser.rs # Cobolparser/AlephTree implementation
├── tree-sitter-cobol-grammar/  # Local tree-sitter COBOL grammar
├── sources/                    # COBOL source files for testing
│   ├── pgm/               # COBOL programs
│   └── cpy/               # Copybooks
└── examples/
    └── parse_program.rs   # Example usage
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

**Limitations:**
- Strict column formatting requirements (COBOL fixed-format)
- May miss paragraphs in non-standard or scanned code
- Requires exact COBOL85 compliance

**Best for:** Production COBOL code with proper column formatting

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

## Initial Test Results

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

### TRFVTB1.cob (larger file, possible column issues)

| Metric | Tree-sitter | Cobolparser |
|--------|-------------|-------------|
| Parse Time | 27 ms | 14 ms |
| Procedures Found | 4 | 62 |
| External Calls | 0 | 10 |

**Analysis:**
- Tree-sitter found only 4 procedures (strict column formatting)
- Cobolparser found 62 but includes false positives like "END-IF"
- This validates the concern about column adherence with scanned code

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

## Next Steps

1. **Improve tree-sitter extraction** - Handle more node types for robustness
2. **Add copybook resolution** - Inline COPY statements before parsing
3. **Benchmark suite** - Compare performance systematically
4. **Error recovery** - Better handling of malformed COBOL
5. **SQL/CICS extraction** - Parse embedded SQL and CICS commands
