// Macro-Centric Language Implementation
// Core AST structures, parser, and macro expansion engine

use std::collections::HashMap;
use std::fmt;

// ============================================================================
// Lexer - Tokenizes source code
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
// Identifiers and literals
Identifier(String),
Integer(i64),
Float(f64),
String(String),
Boolean(bool),

```
// Keywords
Primitive,
Fn,
Let,
Mut,
If,
Else,
While,
Where,

// Operators
Assign,         // =
Arrow,          // ->
Pipe,           // |
Dollar,         // $
Ellipsis,       // ...
Bang,           // !
Plus,           // +
Minus,          // -
Star,           // *
Slash,          // /
Equal,          // ==
NotEqual,       // !=
Less,           // <
Greater,        // >

// Delimiters
LeftParen,      // (
RightParen,     // )
LeftBrace,      // {
RightBrace,     // }
LeftBracket,    // [
RightBracket,   // ]
Comma,          // ,
Semicolon,      // ;
Colon,          // :

// Special
Newline,
Eof,
```

}

pub struct Lexer {
input: String,
position: usize,
current_char: Option<char>,
}

impl Lexer {
pub fn new(input: String) -> Self {
let mut lexer = Lexer {
current_char: input.chars().next(),
input,
position: 0,
};
lexer
}

```
fn advance(&mut self) {
    self.position += 1;
    self.current_char = self.input.chars().nth(self.position);
}

fn peek(&self) -> Option<char> {
    self.input.chars().nth(self.position + 1)
}

fn skip_whitespace(&mut self) {
    while let Some(ch) = self.current_char {
        if ch.is_whitespace() && ch != '\n' {
            self.advance();
        } else {
            break;
        }
    }
}

fn read_string(&mut self) -> String {
    let mut result = String::new();
    self.advance(); // Skip opening quote

    while let Some(ch) = self.current_char {
        if ch == '"' {
            self.advance(); // Skip closing quote
            break;
        }
        result.push(ch);
        self.advance();
    }

    result
}

fn read_number(&mut self) -> Token {
    let mut result = String::new();
    let mut is_float = false;

    while let Some(ch) = self.current_char {
        if ch.is_ascii_digit() {
            result.push(ch);
            self.advance();
        } else if ch == '.' && !is_float && self.peek().map_or(false, |c| c.is_ascii_digit()) {
            is_float = true;
            result.push(ch);
            self.advance();
        } else {
            break;
        }
    }

    if is_float {
        Token::Float(result.parse().unwrap_or(0.0))
    } else {
        Token::Integer(result.parse().unwrap_or(0))
    }
}

fn read_identifier(&mut self) -> Token {
    let mut result = String::new();

    while let Some(ch) = self.current_char {
        if ch.is_alphanumeric() || ch == '_' {
            result.push(ch);
            self.advance();
        } else {
            break;
        }
    }

    // Check for keywords
    match result.as_str() {
        "primitive" => Token::Primitive,
        "fn" => Token::Fn,
        "let" => Token::Let,
        "mut" => Token::Mut,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "where" => Token::Where,
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        _ => Token::Identifier(result),
    }
}

pub fn next_token(&mut self) -> Token {
    self.skip_whitespace();

    match self.current_char {
        None => Token::Eof,
        Some('\n') => {
            self.advance();
            Token::Newline
        },
        Some('"') => Token::String(self.read_string()),
        Some(ch) if ch.is_ascii_digit() => self.read_number(),
        Some(ch) if ch.is_alphabetic() || ch == '_' => self.read_identifier(),
        Some('=') => {
            self.advance();
            if self.current_char == Some('=') {
                self.advance();
                Token::Equal
            } else {
                Token::Assign
            }
        },
        Some('-') => {
            self.advance();
            if self.current_char == Some('>') {
                self.advance();
                Token::Arrow
            } else {
                Token::Minus
            }
        },
        Some('.') => {
            if self.peek() == Some('.') {
                self.advance();
                if self.peek() == Some('.') {
                    self.advance();
                    self.advance();
                    Token::Ellipsis
                } else {
                    Token::Identifier(".".to_string()) // Error case
                }
            } else {
                self.advance();
                Token::Identifier(".".to_string())
            }
        },
        Some('!') => {
            self.advance();
            if self.current_char == Some('=') {
                self.advance();
                Token::NotEqual
            } else {
                Token::Bang
            }
        },
        Some('|') => { self.advance(); Token::Pipe },
        Some('$') => { self.advance(); Token::Dollar },
        Some('+') => { self.advance(); Token::Plus },
        Some('*') => { self.advance(); Token::Star },
        Some('/') => { self.advance(); Token::Slash },
        Some('<') => { self.advance(); Token::Less },
        Some('>') => { self.advance(); Token::Greater },
        Some('(') => { self.advance(); Token::LeftParen },
        Some(')') => { self.advance(); Token::RightParen },
        Some('{') => { self.advance(); Token::LeftBrace },
        Some('}') => { self.advance(); Token::RightBrace },
        Some('[') => { self.advance(); Token::LeftBracket },
        Some(']') => { self.advance(); Token::RightBracket },
        Some(',') => { self.advance(); Token::Comma },
        Some(';') => { self.advance(); Token::Semicolon },
        Some(':') => { self.advance(); Token::Colon },
        Some(ch) => {
            self.advance();
            Token::Identifier(ch.to_string()) // Unknown character becomes identifier
        },
    }
}

pub fn tokenize(&mut self) -> Vec<Token> {
    let mut tokens = Vec::new();

    loop {
        let token = self.next_token();
        let is_eof = token == Token::Eof;
        tokens.push(token);
        if is_eof {
            break;
        }
    }

    tokens
}
```

}

// ============================================================================
// Parser - Converts tokens to AST
// ============================================================================

#[derive(Debug)]
pub enum ParseError {
UnexpectedToken(Token),
UnexpectedEof,
InvalidMacroPattern,
InvalidExpression,
}

impl fmt::Display for ParseError {
fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
match self {
ParseError::UnexpectedToken(token) => write!(f, “Unexpected token: {:?}”, token),
ParseError::UnexpectedEof => write!(f, “Unexpected end of file”),
ParseError::InvalidMacroPattern => write!(f, “Invalid macro pattern”),
ParseError::InvalidExpression => write!(f, “Invalid expression”),
}
}
}

pub struct Parser {
tokens: Vec<Token>,
position: usize,
}

impl Parser {
pub fn new(tokens: Vec<Token>) -> Self {
Parser { tokens, position: 0 }
}

```
fn current_token(&self) -> &Token {
    self.tokens.get(self.position).unwrap_or(&Token::Eof)
}

fn advance(&mut self) -> &Token {
    if self.position < self.tokens.len() {
        self.position += 1;
    }
    self.current_token()
}

fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
    if *self.current_token() == expected {
        self.advance();
        Ok(())
    } else {
        Err(ParseError::UnexpectedToken(self.current_token().clone()))
    }
}

fn skip_newlines(&mut self) {
    while *self.current_token() == Token::Newline {
        self.advance();
    }
}

// Main parsing entry point
pub fn parse(&mut self) -> Result<Vec<ASTNode>, ParseError> {
    let mut statements = Vec::new();

    self.skip_newlines();

    while *self.current_token() != Token::Eof {
        let stmt = self.parse_statement()?;
        statements.push(stmt);
        self.skip_newlines();
    }

    Ok(statements)
}

fn parse_statement(&mut self) -> Result<ASTNode, ParseError> {
    match self.current_token() {
        Token::Primitive => self.parse_primitive_declaration(),
        Token::Fn => self.parse_function_definition(),
        Token::Identifier(_) => self.parse_assignment_or_call(),
        _ => self.parse_expression(),
    }
}

fn parse_primitive_declaration(&mut self) -> Result<ASTNode, ParseError> {
    self.expect(Token::Primitive)?; // consume 'primitive'

    let name = match self.current_token() {
        Token::Identifier(name) => {
            let name = name.clone();
            self.advance();
            name
        },
        _ => return Err(ParseError::UnexpectedToken(self.current_token().clone())),
    };

    self.expect(Token::Assign)?; // consume '='

    let builtin = match self.current_token() {
        Token::Identifier(builtin) => {
            let builtin = builtin.clone();
            self.advance();
            builtin
        },
        _ => return Err(ParseError::UnexpectedToken(self.current_token().clone())),
    };

    Ok(ASTNode::PrimitiveDeclaration { name, builtin })
}

fn parse_function_definition(&mut self) -> Result<ASTNode, ParseError> {
    self.expect(Token::Fn)?; // consume 'fn'

    let name = match self.current_token() {
        Token::Identifier(name) => {
            let name = name.clone();
            self.advance();
            name
        },
        _ => return Err(ParseError::UnexpectedToken(self.current_token().clone())),
    };

    self.expect(Token::LeftParen)?;

    let mut params = Vec::new();
    while *self.current_token() != Token::RightParen {
        match self.current_token() {
            Token::Identifier(param) => {
                params.push(param.clone());
                self.advance();

                if *self.current_token() == Token::Comma {
                    self.advance();
                }
            },
            _ => return Err(ParseError::UnexpectedToken(self.current_token().clone())),
        }
    }

    self.expect(Token::RightParen)?;

    let body = Box::new(self.parse_block()?);

    Ok(ASTNode::FunctionDefinition { name, params, body })
}

fn parse_assignment_or_call(&mut self) -> Result<ASTNode, ParseError> {
    let name = match self.current_token() {
        Token::Identifier(name) => {
            let name = name.clone();
            self.advance();
            name
        },
        _ => return Err(ParseError::UnexpectedToken(self.current_token().clone())),
    };

    match self.current_token() {
        Token::Assign => {
            self.advance(); // consume '='

            // Check if this starts with a pipe (macro definition)
            if *self.current_token() == Token::Pipe {
                self.parse_macro_definition(name)
            } else {
                // Regular assignment
                let value = Box::new(self.parse_expression()?);
                Ok(ASTNode::Assignment { name, value })
            }
        },
        Token::LeftParen => {
            // Function/macro call
            self.advance(); // consume '('

            let mut args = Vec::new();
            while *self.current_token() != Token::RightParen {
                args.push(self.parse_expression()?);

                if *self.current_token() == Token::Comma {
                    self.advance();
                }
            }

            self.expect(Token::RightParen)?;

            Ok(ASTNode::Call { name, args })
        },
        _ => {
            // Just an identifier
            Ok(ASTNode::Identifier(name))
        }
    }
}

fn parse_macro_definition(&mut self, name: String) -> Result<ASTNode, ParseError> {
    let mut patterns = Vec::new();

    // Parse first pattern
    patterns.push(self.parse_macro_pattern()?);

    // Parse additional patterns if they exist
    while *self.current_token() == Token::Pipe {
        patterns.push(self.parse_macro_pattern()?);
    }

    Ok(ASTNode::MacroDefinition { name, patterns })
}

fn parse_macro_pattern(&mut self) -> Result<MacroPattern, ParseError> {
    self.expect(Token::Pipe)?; // consume '|'

    // Parse parameters
    let mut parameters = Vec::new();

    while *self.current_token() != Token::Arrow {
        parameters.push(self.parse_macro_parameter()?);

        if *self.current_token() == Token::Comma {
            self.advance();
        }
    }

    self.expect(Token::Arrow)?; // consume '->'

    // Parse expansion
    let expansion = Box::new(self.parse_expression()?);

    // TODO: Parse guard (where clause)
    let guard = None;

    Ok(MacroPattern {
        parameters,
        expansion,
        guard,
    })
}

fn parse_macro_parameter(&mut self) -> Result<MacroParameter, ParseError> {
    self.expect(Token::Dollar)?; // consume '$'

    let name = match self.current_token() {
        Token::Identifier(name) => {
            let name = name.clone();
            self.advance();
            name
        },
        _ => return Err(ParseError::InvalidMacroPattern),
    };

    // Check for variadic (...)
    if *self.current_token() == Token::Ellipsis {
        self.advance();
        Ok(MacroParameter::Variadic(name))
    } else {
        Ok(MacroParameter::Single(name))
    }
}

fn parse_expression(&mut self) -> Result<ASTNode, ParseError> {
    self.parse_primary()
}

fn parse_primary(&mut self) -> Result<ASTNode, ParseError> {
    match self.current_token().clone() {
        Token::Integer(val) => {
            self.advance();
            Ok(ASTNode::Literal(Literal::Integer(val)))
        },
        Token::Float(val) => {
            self.advance();
            Ok(ASTNode::Literal(Literal::Float(val)))
        },
        Token::String(val) => {
            self.advance();
            Ok(ASTNode::Literal(Literal::String(val)))
        },
        Token::Boolean(val) => {
            self.advance();
            Ok(ASTNode::Literal(Literal::Boolean(val)))
        },
        Token::Identifier(name) => {
            self.advance();

            // Check for function call
            if *self.current_token() == Token::LeftParen {
                self.advance(); // consume '('

                let mut args = Vec::new();
                while *self.current_token() != Token::RightParen {
                    args.push(self.parse_expression()?);

                    if *self.current_token() == Token::Comma {
                        self.advance();
                    }
                }

                self.expect(Token::RightParen)?;

                Ok(ASTNode::Call { name, args })
            } else {
                Ok(ASTNode::Identifier(name))
            }
        },
        Token::LeftBrace => self.parse_block(),
        Token::If => self.parse_if(),
        _ => Err(ParseError::InvalidExpression),
    }
}

fn parse_block(&mut self) -> Result<ASTNode, ParseError> {
    self.expect(Token::LeftBrace)?; // consume '{'

    let mut statements = Vec::new();
    self.skip_newlines();

    while *self.current_token() != Token::RightBrace {
        statements.push(self.parse_statement()?);
        self.skip_newlines();

        // Optional semicolon
        if *self.current_token() == Token::Semicolon {
            self.advance();
            self.skip_newlines();
        }
    }

    self.expect(Token::RightBrace)?; // consume '}'

    Ok(ASTNode::Block(statements))
}

fn parse_if(&mut self) -> Result<ASTNode, ParseError> {
    self.expect(Token::If)?; // consume 'if'

    let condition = Box::new(self.parse_expression()?);
    let then_branch = Box::new(self.parse_block()?);

    let else_branch = if *self.current_token() == Token::Else {
        self.advance(); // consume 'else'
        Some(Box::new(self.parse_block()?))
    } else {
        None
    };

    Ok(ASTNode::If {
        condition,
        then_branch,
        else_branch,
    })
}
```

}

// ============================================================================
// AST Node Definitions
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
// Basic expressions
Literal(Literal),
Identifier(String),

```
// Function call / macro invocation (determined during resolution)
Call { name: String, args: Vec<ASTNode> },

// Code blocks
Block(Vec<ASTNode>),

// Control flow (these will become macros)
If { condition: Box<ASTNode>, then_branch: Box<ASTNode>, else_branch: Option<Box<ASTNode>> },
While { condition: Box<ASTNode>, body: Box<ASTNode> },

// Macro definition (identified by pattern matching)
MacroDefinition {
    name: String,
    patterns: Vec<MacroPattern>,
},

// Regular assignment/function definition
Assignment { name: String, value: Box<ASTNode> },
FunctionDefinition { name: String, params: Vec<String>, body: Box<ASTNode> },

// Primitive declaration
PrimitiveDeclaration { name: String, builtin: String },

// Error node for recovery
Error(String),
```

}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroPattern {
pub parameters: Vec<MacroParameter>,
pub expansion: Box<ASTNode>,
pub guard: Option<Box<ASTNode>>, // for conditional patterns like ‘where’
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroParameter {
Single(String),           // $var
Variadic(String),         // $var…
Block(String),            // { $body… }
TypeAnnotated(String, String), // $var: Type
Structured {              // $($field: $type),*
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

// ============================================================================
// Macro Expansion Context
// ============================================================================

pub struct MacroContext {
// Defined macros at current expansion stage
pub macros: HashMap<String, Vec<MacroPattern>>,

```
// Built-in primitives
pub primitives: HashMap<String, String>,

// Current expansion depth (for cycle detection)
pub depth: usize,

// Maximum allowed expansion depth
pub max_depth: usize,

// Variable bindings during pattern matching
pub bindings: HashMap<String, ASTNode>,
```

}

impl MacroContext {
pub fn new() -> Self {
let mut primitives = HashMap::new();

```
    // Core built-ins that bootstrap the system
    primitives.insert("builtin_if".to_string(), "if".to_string());
    primitives.insert("builtin_while".to_string(), "while".to_string());
    primitives.insert("builtin_add".to_string(), "+".to_string());
    primitives.insert("builtin_print".to_string(), "print".to_string());

    MacroContext {
        macros: HashMap::new(),
        primitives,
        depth: 0,
        max_depth: 1000,
        bindings: HashMap::new(),
    }
}

pub fn define_macro(&mut self, name: String, patterns: Vec<MacroPattern>) {
    self.macros.insert(name, patterns);
}

pub fn define_primitive(&mut self, name: String, builtin: String) {
    self.primitives.insert(builtin, name);
}
```

}

// ============================================================================
// Pattern Matching and Expansion Engine
// ============================================================================

#[derive(Debug)]
pub enum ExpandError {
MaxDepthExceeded,
NoMatchingPattern(String),
InvalidParameter(String),
CyclicExpansion,
}

impl fmt::Display for ExpandError {
fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
match self {
ExpandError::MaxDepthExceeded => write!(f, “Maximum macro expansion depth exceeded”),
ExpandError::NoMatchingPattern(name) => write!(f, “No matching pattern for macro ‘{}’”, name),
ExpandError::InvalidParameter(msg) => write!(f, “Invalid macro parameter: {}”, msg),
ExpandError::CyclicExpansion => write!(f, “Cyclic macro expansion detected”),
}
}
}

pub struct MacroExpander {
context: MacroContext,
}

impl MacroExpander {
pub fn new() -> Self {
MacroExpander {
context: MacroContext::new(),
}
}

```
// Main expansion entry point
pub fn expand(&mut self, ast: ASTNode) -> Result<ASTNode, ExpandError> {
    if self.context.depth > self.context.max_depth {
        return Err(ExpandError::MaxDepthExceeded);
    }

    match ast {
        ASTNode::MacroDefinition { name, patterns } => {
            self.context.define_macro(name.clone(), patterns);
            Ok(ASTNode::Assignment {
                name,
                value: Box::new(ASTNode::Literal(Literal::String("macro".to_string())))
            })
        },

        ASTNode::PrimitiveDeclaration { name, builtin } => {
            self.context.define_primitive(name.clone(), builtin);
            Ok(ASTNode::Assignment {
                name,
                value: Box::new(ASTNode::Literal(Literal::String("primitive".to_string())))
            })
        },

        ASTNode::Call { name, args } => {
            self.expand_call(name, args)
        },

        ASTNode::Block(nodes) => {
            let expanded: Result<Vec<_>, _> = nodes.into_iter()
                .map(|node| self.expand(node))
                .collect();
            Ok(ASTNode::Block(expanded?))
        },

        // Recursively expand other node types
        ASTNode::If { condition, then_branch, else_branch } => {
            let expanded_condition = self.expand(*condition)?;
            let expanded_then = self.expand(*then_branch)?;
            let expanded_else = match else_branch {
                Some(e) => Some(Box::new(self.expand(*e)?)),
                None => None,
            };

            Ok(ASTNode::If {
                condition: Box::new(expanded_condition),
                then_branch: Box::new(expanded_then),
                else_branch: expanded_else,
            })
        },

        // Literals and identifiers don't need expansion
        node => Ok(node),
    }
}

fn expand_call(&mut self, name: String, args: Vec<ASTNode>) -> Result<ASTNode, ExpandError> {
    // Check if this is a macro call
    if let Some(patterns) = self.context.macros.get(&name).cloned() {
        self.context.depth += 1;

        // Try each pattern until one matches
        for pattern in patterns {
            if let Ok(expanded) = self.try_pattern_match(&pattern, &args) {
                let result = self.expand(expanded);
                self.context.depth -= 1;
                return result;
            }
        }

        self.context.depth -= 1;
        return Err(ExpandError::NoMatchingPattern(name));
    }

    // Not a macro, expand arguments and return regular call
    let expanded_args: Result<Vec<_>, _> = args.into_iter()
        .map(|arg| self.expand(arg))
        .collect();

    Ok(ASTNode::Call { name, args: expanded_args? })
}

fn try_pattern_match(&mut self, pattern: &MacroPattern, args: &[ASTNode]) -> Result<ASTNode, ExpandError> {
    // Save current bindings
    let saved_bindings = self.context.bindings.clone();

    // Try to match parameters with arguments
    if !self.match_parameters(&pattern.parameters, args) {
        // Restore bindings on failure
        self.context.bindings = saved_bindings;
        return Err(ExpandError::NoMatchingPattern("pattern".to_string()));
    }

    // Check guard condition if present
    if let Some(guard) = &pattern.guard {
        let guard_result = self.evaluate_guard(guard)?;
        if !guard_result {
            self.context.bindings = saved_bindings;
            return Err(ExpandError::NoMatchingPattern("guard failed".to_string()));
        }
    }

    // Substitute bindings in expansion
    let result = self.substitute_bindings(&pattern.expansion);

    // Clean up bindings
    self.context.bindings = saved_bindings;

    Ok(result)
}

fn match_parameters(&mut self, parameters: &[MacroParameter], args: &[ASTNode]) -> bool {
    // Simplified pattern matching - full implementation would be more sophisticated
    if parameters.len() != args.len() {
        return false;
    }

    for (param, arg) in parameters.iter().zip(args.iter()) {
        match param {
            MacroParameter::Single(name) => {
                self.context.bindings.insert(name.clone(), arg.clone());
            },
            MacroParameter::Block(name) => {
                if let ASTNode::Block(_) = arg {
                    self.context.bindings.insert(name.clone(), arg.clone());
                } else {
                    return false;
                }
            },
            // TODO: Implement variadic and other parameter types
            _ => return false,
        }
    }

    true
}

fn evaluate_guard(&mut self, _guard: &ASTNode) -> Result<bool, ExpandError> {
    // Simplified guard evaluation
    // Real implementation would evaluate compile-time conditions
    Ok(true)
}

fn substitute_bindings(&self, ast: &ASTNode) -> ASTNode {
    match ast {
        ASTNode::Identifier(name) => {
            if let Some(binding) = self.context.bindings.get(name) {
                binding.clone()
            } else {
                ast.clone()
            }
        },

        ASTNode::Call { name, args } => {
            let substituted_args: Vec<_> = args.iter()
                .map(|arg| self.substitute_bindings(arg))
                .collect();

            ASTNode::Call {
                name: name.clone(),
                args: substituted_args,
            }
        },

        ASTNode::Block(nodes) => {
            let substituted_nodes: Vec<_> = nodes.iter()
                .map(|node| self.substitute_bindings(node))
                .collect();
            ASTNode::Block(substituted_nodes)
        },

        // TODO: Handle other node types
        _ => ast.clone(),
    }
}
```

}

// ============================================================================
// Example Usage and Tests
// ============================================================================

#[cfg(test)]
mod tests {
use super::*;

```
#[test]
fn test_lexer_basic_tokens() {
    let mut lexer = Lexer::new("= -> | $ ... ( ) { }".to_string());
    let tokens = lexer.tokenize();

    assert_eq!(tokens, vec![
        Token::Assign,
        Token::Arrow,
        Token::Pipe,
        Token::Dollar,
        Token::Ellipsis,
        Token::LeftParen,
        Token::RightParen,
        Token::LeftBrace,
        Token::RightBrace,
        Token::Eof,
    ]);
}

#[test]
fn test_lexer_identifiers_and_keywords() {
    let mut lexer = Lexer::new("primitive fn if else when true false".to_string());
    let tokens = lexer.tokenize();

    assert_eq!(tokens, vec![
        Token::Primitive,
        Token::Fn,
        Token::If,
        Token::Else,
        Token::Identifier("when".to_string()),
        Token::Boolean(true),
        Token::Boolean(false),
        Token::Eof,
    ]);
}

#[test]
fn test_parser_primitive_declaration() {
    let mut lexer = Lexer::new("primitive if = builtin_if".to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);

    let ast = parser.parse().unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0] {
        ASTNode::PrimitiveDeclaration { name, builtin } => {
            assert_eq!(name, "if");
            assert_eq!(builtin, "builtin_if");
        },
        _ => panic!("Expected PrimitiveDeclaration"),
    }
}

#[test]
fn test_parser_macro_definition() {
    let mut lexer = Lexer::new("when = | $condition { $body } -> if $condition { $body }".to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);

    let ast = parser.parse().unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0] {
        ASTNode::MacroDefinition { name, patterns } => {
            assert_eq!(name, "when");
            assert_eq!(patterns.len(), 1);

            let pattern = &patterns[0];
            assert_eq!(pattern.parameters.len(), 2);

            match &pattern.parameters[0] {
                MacroParameter::Single(name) => assert_eq!(name, "condition"),
                _ => panic!("Expected Single parameter"),
            }
        },
        _ => panic!("Expected MacroDefinition"),
    }
}

#[test]
fn test_parser_function_call() {
    let mut lexer = Lexer::new("print(\"hello\", 42)".to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);

    let ast = parser.parse().unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0] {
        ASTNode::Call { name, args } => {
            assert_eq!(name, "print");
            assert_eq!(args.len(), 2);

            match &args[0] {
                ASTNode::Literal(Literal::String(s)) => assert_eq!(s, "hello"),
                _ => panic!("Expected string literal"),
            }

            match &args[1] {
                ASTNode::Literal(Literal::Integer(i)) => assert_eq!(*i, 42),
                _ => panic!("Expected integer literal"),
            }
        },
        _ => panic!("Expected Call"),
    }
}

#[test]
fn test_complete_macro_expansion() {
    let source = r#"
        primitive if = builtin_if

        when = | $condition { $body } -> if $condition { $body }

        when(true) {
            print("hello")
        }
    "#;

    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();

    let mut expander = MacroExpander::new();

    // Process all statements
    for node in ast {
        let result = expander.expand(node).unwrap();
        println!("Expanded: {:?}", result);
    }
}

#[test]
fn test_basic_macro_expansion() {
    let mut expander = MacroExpander::new();

    // Define a simple 'when' macro: when = | $condition { $body... } -> if $condition { $body... }
    let when_pattern = MacroPattern {
        parameters: vec![
            MacroParameter::Single("condition".to_string()),
            MacroParameter::Block("body".to_string()),
        ],
        expansion: Box::new(ASTNode::If {
            condition: Box::new(ASTNode::Identifier("condition".to_string())),
            then_branch: Box::new(ASTNode::Identifier("body".to_string())),
            else_branch: None,
        }),
        guard: None,
    };

    expander.context.define_macro("when".to_string(), vec![when_pattern]);

    // Test expansion of: when(true, { print("hello") })
    let macro_call = ASTNode::Call {
        name: "when".to_string(),
        args: vec![
            ASTNode::Literal(Literal::Boolean(true)),
            ASTNode::Block(vec![
                ASTNode::Call {
                    name: "print".to_string(),
                    args: vec![ASTNode::Literal(Literal::String("hello".to_string()))],
                }
            ]),
        ],
    };

    let result = expander.expand(macro_call).unwrap();

    // Should expand to: if true { print("hello") }
    match result {
        ASTNode::If { condition, then_branch, else_branch } => {
            assert_eq!(*condition, ASTNode::Literal(Literal::Boolean(true)));
            assert!(else_branch.is_none());
        },
        _ => panic!("Expected If node"),
    }
}
```

}

fn main() {
println!(“Macro-Centric Language Implementation”);
println!(”====================================”);

```
// Example usage
let source = r#"
    primitive if = builtin_if
    primitive print = builtin_print

    when = | $condition { $body } -> if $condition { $body }

    negate = | $x -> !$x

    when(true) {
        print("Hello from macro!")
    }
"#;

println!("Source code:");
println!("{}", source);
println!();

// Tokenize
let mut lexer = Lexer::new(source.to_string());
let tokens = lexer.tokenize();
println!("Tokens: {:?}", tokens);
println!();

// Parse
let mut parser = Parser::new(tokens);
match parser.parse() {
    Ok(ast) => {
        println!("AST:");
        for node in &ast {
            println!("  {:?}", node);
        }
        println!();

        // Expand macros
        let mut expander = MacroExpander::new();
        println!("Expanding macros:");

        for node in ast {
            match expander.expand(node) {
                Ok(expanded) => println!("  Expanded: {:?}", expanded),
                Err(e) => println!("  Error: {}", e),
            }
        }
    },
    Err(e) => {
        println!("Parse error: {}", e);
    }
}
```

}
