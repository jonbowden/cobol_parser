//! Copybook Resolution Module
//!
//! Handles COPY statement resolution by inlining copybook content before parsing.
//! This is critical for proper parsing of COBOL programs that use external copybooks.
//!
//! COPY statement formats supported:
//! - COPY copybook-name.
//! - COPY copybook-name OF library-name.
//! - COPY DDS-ALL-FORMATS OF file-name.

use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Copybook resolution errors
#[derive(Debug, Error)]
pub enum CopybookError {
    #[error("Copybook not found: {0}")]
    NotFound(String),

    #[error("IO error reading copybook {name}: {source}")]
    Io {
        name: String,
        #[source]
        source: std::io::Error,
    },

    #[error("Maximum recursion depth ({0}) exceeded while resolving copybooks")]
    MaxRecursionDepth(usize),

    #[error("Circular dependency detected: {0}")]
    CircularDependency(String),
}

/// Configuration for copybook resolution
#[derive(Debug, Clone)]
pub struct CopybookConfig {
    /// Directories to search for copybooks
    pub search_paths: Vec<PathBuf>,

    /// File extensions to try when searching
    pub extensions: Vec<String>,

    /// Maximum recursion depth for nested COPY statements
    pub max_depth: usize,

    /// Whether to mark inlined sections with comments
    pub mark_inlined: bool,

    /// Whether to preserve original COPY statement as comment
    pub preserve_original: bool,
}

impl Default for CopybookConfig {
    fn default() -> Self {
        Self {
            search_paths: vec![PathBuf::from("sources/cpy")],
            extensions: vec![
                "cpy".to_string(),
                "CPY".to_string(),
                "cbl".to_string(),
                "CBL".to_string(),
                "".to_string(), // Try without extension too
            ],
            max_depth: 10,
            mark_inlined: true,
            preserve_original: true,
        }
    }
}

/// Copybook resolver - inlines COPY statements
pub struct CopybookResolver {
    config: CopybookConfig,
    cache: HashMap<String, String>,
    copy_pattern: Regex,
}

impl CopybookResolver {
    /// Create a new copybook resolver
    pub fn new(config: CopybookConfig) -> Self {
        // Pattern to match COPY statements
        // Handles: COPY name. / COPY name OF lib. / COPY DDS-xxx OF file.
        let copy_pattern = Regex::new(
            r"(?i)^(.{0,6})\s*COPY\s+([A-Z0-9_-]+)(?:\s+OF\s+([A-Z0-9_-]+))?\s*\.\s*$"
        ).expect("Invalid COPY regex pattern");

        Self {
            config,
            cache: HashMap::new(),
            copy_pattern,
        }
    }

    /// Create resolver with default config and additional search paths
    pub fn with_paths(paths: Vec<PathBuf>) -> Self {
        let mut config = CopybookConfig::default();
        config.search_paths.extend(paths);
        Self::new(config)
    }

    /// Resolve all COPY statements in the source code
    pub fn resolve(&mut self, source: &str) -> Result<ResolvedSource, CopybookError> {
        let mut resolved_copybooks = Vec::new();
        let mut visiting = HashSet::new();

        let resolved = self.resolve_recursive(source, 0, &mut resolved_copybooks, &mut visiting)?;

        Ok(ResolvedSource {
            source: resolved,
            copybooks: resolved_copybooks,
        })
    }

    /// Recursive resolution with cycle detection
    fn resolve_recursive(
        &mut self,
        source: &str,
        depth: usize,
        resolved: &mut Vec<ResolvedCopybook>,
        visiting: &mut HashSet<String>,
    ) -> Result<String, CopybookError> {
        if depth > self.config.max_depth {
            return Err(CopybookError::MaxRecursionDepth(self.config.max_depth));
        }

        let mut result = String::with_capacity(source.len() * 2);

        for (line_num, line) in source.lines().enumerate() {
            if let Some(caps) = self.copy_pattern.captures(line) {
                let prefix = caps.get(1).map(|m| m.as_str()).unwrap_or("");
                let copybook_name = caps.get(2).map(|m| m.as_str()).unwrap_or("");
                let library = caps.get(3).map(|m| m.as_str());

                // Check for circular dependency
                let key = copybook_name.to_uppercase();
                if visiting.contains(&key) {
                    return Err(CopybookError::CircularDependency(key));
                }

                // Load copybook content
                match self.load_copybook(copybook_name, library) {
                    Ok(content) => {
                        visiting.insert(key.clone());

                        // Recursively resolve nested COPY statements
                        let nested_content = self.resolve_recursive(
                            &content,
                            depth + 1,
                            resolved,
                            visiting,
                        )?;

                        visiting.remove(&key);

                        let content_lines = nested_content.lines().count() as i32;

                        // Record resolved copybook
                        resolved.push(ResolvedCopybook {
                            name: copybook_name.to_uppercase(),
                            library: library.map(|s| s.to_uppercase()),
                            original_line: line_num + 1,
                            inserted_lines: content_lines as usize,
                        });

                        // Add markers and content
                        if self.config.preserve_original {
                            result.push_str(prefix);
                            result.push_str("*>>> COPY ");
                            result.push_str(copybook_name);
                            if let Some(lib) = library {
                                result.push_str(" OF ");
                                result.push_str(lib);
                            }
                            result.push_str(" - ORIGINAL <<<\n");
                        }

                        if self.config.mark_inlined {
                            result.push_str(prefix);
                            result.push_str("*>>> BEGIN INLINED COPY ");
                            result.push_str(copybook_name);
                            result.push_str(" <<<\n");
                        }

                        result.push_str(&nested_content);

                        if self.config.mark_inlined {
                            if !nested_content.ends_with('\n') {
                                result.push('\n');
                            }
                            result.push_str(prefix);
                            result.push_str("*>>> END INLINED COPY ");
                            result.push_str(copybook_name);
                            result.push_str(" <<<\n");
                        }
                    }
                    Err(e) => {
                        // Keep original COPY statement if copybook not found
                        // Add warning comment
                        result.push_str(prefix);
                        result.push_str("*>>> WARNING: ");
                        result.push_str(&e.to_string());
                        result.push_str(" <<<\n");
                        result.push_str(line);
                        result.push('\n');
                    }
                }
            } else {
                result.push_str(line);
                result.push('\n');
            }
        }

        Ok(result)
    }

    /// Load a copybook from disk (with caching)
    fn load_copybook(&mut self, name: &str, library: Option<&str>) -> Result<String, CopybookError> {
        let key = if let Some(lib) = library {
            format!("{}:{}", lib.to_uppercase(), name.to_uppercase())
        } else {
            name.to_uppercase()
        };

        // Check cache first
        if let Some(content) = self.cache.get(&key) {
            return Ok(content.clone());
        }

        // Search for the copybook
        let content = self.find_and_read_copybook(name, library)?;

        // Cache it
        self.cache.insert(key, content.clone());

        Ok(content)
    }

    /// Find and read a copybook file
    fn find_and_read_copybook(&self, name: &str, library: Option<&str>) -> Result<String, CopybookError> {
        // Build search candidates
        let mut candidates = Vec::new();

        for search_path in &self.config.search_paths {
            // If library specified, try library-specific paths first
            if let Some(lib) = library {
                for ext in &self.config.extensions {
                    let filename = if ext.is_empty() {
                        name.to_string()
                    } else {
                        format!("{}.{}", name, ext)
                    };

                    // Try: search_path/library/filename
                    candidates.push(search_path.join(lib).join(&filename));
                    // Try: search_path/filename (library as prefix)
                    candidates.push(search_path.join(format!("{}-{}", lib, filename)));
                }
            }

            // Standard search
            for ext in &self.config.extensions {
                let filename = if ext.is_empty() {
                    name.to_string()
                } else {
                    format!("{}.{}", name, ext)
                };
                candidates.push(search_path.join(&filename));

                // Try uppercase/lowercase variants
                candidates.push(search_path.join(filename.to_uppercase()));
                candidates.push(search_path.join(filename.to_lowercase()));
            }
        }

        // Try each candidate
        for path in &candidates {
            if path.exists() {
                return std::fs::read_to_string(path).map_err(|e| CopybookError::Io {
                    name: name.to_string(),
                    source: e,
                });
            }
        }

        Err(CopybookError::NotFound(name.to_string()))
    }

    /// Clear the copybook cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }

    /// Add a search path
    pub fn add_search_path(&mut self, path: PathBuf) {
        if !self.config.search_paths.contains(&path) {
            self.config.search_paths.push(path);
        }
    }

    /// Pre-load copybooks from a directory
    pub fn preload_directory(&mut self, dir: &Path) -> Result<usize, CopybookError> {
        let mut count = 0;

        if !dir.exists() {
            return Ok(0);
        }

        for entry in std::fs::read_dir(dir).map_err(|e| CopybookError::Io {
            name: dir.to_string_lossy().to_string(),
            source: e,
        })? {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.is_file() {
                    if let Some(stem) = path.file_stem() {
                        let name = stem.to_string_lossy().to_uppercase();
                        if let Ok(content) = std::fs::read_to_string(&path) {
                            self.cache.insert(name, content);
                            count += 1;
                        }
                    }
                }
            }
        }

        Ok(count)
    }
}

/// Result of copybook resolution
#[derive(Debug, Clone)]
pub struct ResolvedSource {
    /// The source code with COPY statements inlined
    pub source: String,

    /// List of copybooks that were resolved
    pub copybooks: Vec<ResolvedCopybook>,
}

/// Information about a resolved copybook
#[derive(Debug, Clone)]
pub struct ResolvedCopybook {
    /// Copybook name
    pub name: String,

    /// Library name (if specified)
    pub library: Option<String>,

    /// Original line number where COPY appeared
    pub original_line: usize,

    /// Number of lines inserted
    pub inserted_lines: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copy_pattern() {
        let resolver = CopybookResolver::new(CopybookConfig::default());

        // Test basic COPY
        assert!(resolver.copy_pattern.is_match("       COPY MYBOOK."));
        assert!(resolver.copy_pattern.is_match("      COPY MYBOOK."));

        // Test COPY OF
        assert!(resolver.copy_pattern.is_match("       COPY MYBOOK OF MYLIB."));
        assert!(resolver.copy_pattern.is_match("       COPY DDS-ALL-FORMATS OF TFSBNKAC."));

        // Test case insensitivity
        assert!(resolver.copy_pattern.is_match("       copy mybook."));

        // Non-matches
        assert!(!resolver.copy_pattern.is_match("      * COPY MYBOOK.")); // Comment
        assert!(!resolver.copy_pattern.is_match("       MOVE X TO COPY.")); // Not a COPY statement
    }
}
