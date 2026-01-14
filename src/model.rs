//! IR Object Model - Compatible with IR_analyser Python implementation
//!
//! Supports both legacy and platform URN formats:
//! - Legacy: urn:{type}:{namespace}:{name}
//! - Platform: urn:ir:{app}:{type}:{name}

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// IR object types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ObjectType {
    Program,
    Module,
    Procedure,
    Service,
    Operation,
    Entity,
    Attribute,
    Relationship,
    Channel,
    MessageType,
    Screen,
    Widget,
    Workflow,
    Job,
    Step,
    Dependency,
    Config,
    Policy,
    Bundle,
}

impl ObjectType {
    pub fn as_str(&self) -> &'static str {
        match self {
            ObjectType::Program => "program",
            ObjectType::Module => "module",
            ObjectType::Procedure => "procedure",
            ObjectType::Service => "service",
            ObjectType::Operation => "operation",
            ObjectType::Entity => "entity",
            ObjectType::Attribute => "attribute",
            ObjectType::Relationship => "relationship",
            ObjectType::Channel => "channel",
            ObjectType::MessageType => "message_type",
            ObjectType::Screen => "screen",
            ObjectType::Widget => "widget",
            ObjectType::Workflow => "workflow",
            ObjectType::Job => "job",
            ObjectType::Step => "step",
            ObjectType::Dependency => "dependency",
            ObjectType::Config => "config",
            ObjectType::Policy => "policy",
            ObjectType::Bundle => "bundle",
        }
    }
}

/// Edge types for relationships
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EdgeType {
    Reads,
    Writes,
    Calls,
    Publishes,
    Displays,
    DependsOn,
    Includes,
    Implements,
}

/// Origin information for traceability
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Origin {
    pub file: String,
    pub line: Option<usize>,
    pub column: Option<usize>,
}

/// Base IR object with common properties
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRObject {
    pub urn: String,
    #[serde(rename = "type")]
    pub object_type: ObjectType,
    pub name: String,
    pub version: String,
    pub origin: Origin,
    pub hash: String,
    pub tags: Vec<String>,
    pub links: Vec<String>,
    #[serde(default)]
    pub extensions: HashMap<String, serde_json::Value>,
}

impl IRObject {
    pub fn new(urn: String, object_type: ObjectType, name: String) -> Self {
        let mut obj = Self {
            urn,
            object_type,
            name: name.clone(),
            version: "1.0".to_string(),
            origin: Origin::default(),
            hash: String::new(),
            tags: Vec::new(),
            links: Vec::new(),
            extensions: HashMap::new(),
        };
        obj.hash = obj.compute_hash();
        obj
    }

    fn compute_hash(&self) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        self.object_type.as_str().hash(&mut hasher);
        self.name.hash(&mut hasher);
        self.version.hash(&mut hasher);
        format!("{:016x}", hasher.finish())
    }
}

/// Program object - represents a COBOL program
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    #[serde(flatten)]
    pub base: IRObject,
    pub language: String,
    pub modules: Vec<String>,
    pub procedures: Vec<String>,
    pub screens: Vec<String>,
    pub reads_entities: Vec<String>,
    pub writes_entities: Vec<String>,
    pub calls: Vec<String>,
    pub publishes: Vec<String>,
    pub depends_on: Vec<String>,
    pub copybooks: Vec<String>,
    pub data_definitions: Vec<DataDefinition>,
    pub divisions: Vec<Division>,
}

impl Program {
    pub fn new(urn: String, name: String, language: String) -> Self {
        Self {
            base: IRObject::new(urn, ObjectType::Program, name),
            language,
            modules: Vec::new(),
            procedures: Vec::new(),
            screens: Vec::new(),
            reads_entities: Vec::new(),
            writes_entities: Vec::new(),
            calls: Vec::new(),
            publishes: Vec::new(),
            depends_on: Vec::new(),
            copybooks: Vec::new(),
            data_definitions: Vec::new(),
            divisions: Vec::new(),
        }
    }
}

/// Entity (table/file) object
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entity {
    #[serde(flatten)]
    pub base: IRObject,
    pub datastore: String,
    pub schema: String,
    pub attributes: Vec<Attribute>,
    pub relationships: Vec<String>,
    pub indexes: Vec<String>,
}

impl Entity {
    pub fn new(urn: String, name: String, datastore: String) -> Self {
        Self {
            base: IRObject::new(urn, ObjectType::Entity, name),
            datastore,
            schema: String::new(),
            attributes: Vec::new(),
            relationships: Vec::new(),
            indexes: Vec::new(),
        }
    }
}

/// Attribute (field) definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Attribute {
    pub name: String,
    pub data_type: String,
    pub length: Option<usize>,
    pub decimals: Option<usize>,
    pub picture: Option<String>,
    pub value: Option<String>,
    pub redefines: Option<String>,
    pub occurs: Option<usize>,
}

/// Screen/UI object
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Screen {
    #[serde(flatten)]
    pub base: IRObject,
    pub technology: String,
    pub format: String,
    pub fields: Vec<ScreenField>,
    pub function_keys: Vec<FunctionKey>,
    pub bindings: Vec<String>,
}

impl Screen {
    pub fn new(urn: String, name: String, technology: String) -> Self {
        Self {
            base: IRObject::new(urn, ObjectType::Screen, name),
            technology,
            format: String::new(),
            fields: Vec::new(),
            function_keys: Vec::new(),
            bindings: Vec::new(),
        }
    }
}

/// Screen field definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScreenField {
    pub name: String,
    pub row: usize,
    pub col: usize,
    pub length: usize,
    pub field_type: String,
    pub editable: bool,
}

/// Function key mapping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionKey {
    pub key: String,
    pub action: String,
    pub label: Option<String>,
}

/// Business rule extracted from IF conditions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BusinessRule {
    pub variable: String,
    pub operator: String,
    pub value: String,
    pub raw_condition: String,
}

/// Procedure/Paragraph object
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Procedure {
    #[serde(flatten)]
    pub base: IRObject,
    pub parameters: Vec<String>,
    pub returns: Option<String>,
    pub calls: Vec<String>,
    pub performs: Vec<String>,           // PERFORM targets (internal procedure calls)
    pub reads: Vec<String>,
    pub writes: Vec<String>,
    pub variables_used: Vec<String>,
    pub business_rules: Vec<BusinessRule>, // Extracted IF conditions
    pub reason_codes: Vec<String>,        // RSNxxxx codes
    pub code_body: Option<String>,        // Actual source code
    pub summary: Option<String>,          // Natural language summary
    pub source_start_line: Option<usize>,
    pub source_end_line: Option<usize>,
}

impl Procedure {
    pub fn new(urn: String, name: String) -> Self {
        Self {
            base: IRObject::new(urn, ObjectType::Procedure, name),
            parameters: Vec::new(),
            returns: None,
            calls: Vec::new(),
            performs: Vec::new(),
            reads: Vec::new(),
            writes: Vec::new(),
            variables_used: Vec::new(),
            business_rules: Vec::new(),
            reason_codes: Vec::new(),
            code_body: None,
            summary: None,
            source_start_line: None,
            source_end_line: None,
        }
    }
}

/// Data definition (01-level records)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataDefinition {
    pub name: String,
    pub level: u8,
    pub picture: Option<String>,
    pub usage: Option<String>,
    pub value: Option<String>,
    pub redefines: Option<String>,
    pub occurs: Option<usize>,
    pub children: Vec<DataDefinition>,
}

/// COBOL division marker
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Division {
    pub name: String,
    pub start_line: usize,
    pub end_line: Option<usize>,
}

/// URN format configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UrnFormat {
    Legacy,   // urn:{type}:{namespace}:{name}
    Platform, // urn:ir:{app}:{type}:{name}
}

/// Generate URN for an object
pub fn generate_urn(
    object_type: &ObjectType,
    name: &str,
    app: &str,
    format: UrnFormat,
) -> String {
    match format {
        UrnFormat::Legacy => format!("urn:{}:{}:{}", object_type.as_str(), app, name.to_lowercase()),
        UrnFormat::Platform => format!("urn:ir:{}:{}:{}", app, object_type.as_str(), name.to_lowercase()),
    }
}

/// Parse result containing all extracted information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseResult {
    pub program: Program,
    pub entities: Vec<Entity>,
    pub screens: Vec<Screen>,
    pub procedures: Vec<Procedure>,
    pub errors: Vec<ParseError>,
    pub warnings: Vec<String>,
    pub parser_name: String,
    pub parse_time_ms: u64,
}

/// Parse error information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseError {
    pub message: String,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub severity: ErrorSeverity,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ErrorSeverity {
    Warning,
    Error,
    Fatal,
}
