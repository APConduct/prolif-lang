#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    // Basic expressions
    Literal(Literal),
    Identifier(String),

    // Function call / macro invocation (determined during resolution)
    Call {
        name: String,
        args: Vec<ASTNode>,
    },

    // Code blocks
    Block(Vec<ASTNode>),

    // Control flow (these will become macros)
    If {
        condition: Box<ASTNode>,
        then_branch: Box<ASTNode>,
        else_branch: Option<Box<ASTNode>>,
    },
    While {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
    },

    // Macro definition (identified by pattern matching)
    MacroDefinition {
        name: String,
        patterns: Vec<MacroPattern>,
    },

    // Regular assignment/function definition
    Assignment {
        name: String,
        value: Box<ASTNode>,
    },
    FunctionDefinition {
        name: String,
        params: Vec<String>,
        body: Box<ASTNode>,
    },

    // Primitive declaration
    PrimitiveDeclaration {
        name: String,
        builtin: String,
    },

    // Error node for recovery
    Error(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroPattern {
    pub parameters: Vec<MacroParameter>,
    pub expansion: Box<ASTNode>,
    pub guard: Option<Box<ASTNode>>, // for conditional patterns like 'where'
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroParameter {
    Single(String),                // $var
    Variadic(String),              // $var...
    Block(String),                 // { $body... }
    TypeAnnotated(String, String), // $var: Type
    Structured {
        // $($field: $type),*
        pattern: String,
        separator: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}
