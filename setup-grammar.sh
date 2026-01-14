#!/bin/bash
# Setup script for tree-sitter-cobol-grammar submodule
# Run this after cloning or updating the submodule

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GRAMMAR_DIR="$SCRIPT_DIR/tree-sitter-cobol-grammar"

if [ ! -d "$GRAMMAR_DIR" ]; then
    echo "Error: tree-sitter-cobol-grammar directory not found"
    echo "Run: git submodule update --init --recursive"
    exit 1
fi

echo "Applying local modifications to tree-sitter-cobol-grammar..."

# Change package name from tree-sitter-COBOL to tree-sitter-cobol
sed -i 's/name = "tree-sitter-COBOL"/name = "tree-sitter-cobol"/' "$GRAMMAR_DIR/Cargo.toml"

# Update version to match our Cargo.lock expectations
sed -i 's/version = "0.0.1"/version = "0.0.2"/' "$GRAMMAR_DIR/Cargo.toml"

# Update tree-sitter dependency to 0.24 (grammar ships with 0.20.3)
sed -i 's/tree-sitter = "~0.20.3"/tree-sitter = "0.24"/' "$GRAMMAR_DIR/Cargo.toml"

echo "Done. Grammar modifications applied:"
echo "  - Package name: tree-sitter-cobol"
echo "  - Version: 0.0.2"
echo "  - tree-sitter: 0.24"
echo ""
echo "Now run: cargo build --release"
