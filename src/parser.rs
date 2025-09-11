use std::fmt;

use crate::{
    ast::{ASTNode, Literal, MacroParameter, MacroPattern},
    lexer::Token,
};

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
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::UnexpectedEof => write!(f, "Unexpected end of file"),
            ParseError::InvalidMacroPattern => write!(f, "Invalid macro pattern"),
            ParseError::InvalidExpression => write!(f, "Invalid expression"),
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

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
            }
            Token::If => {
                self.advance();
                "if".to_string()
            }
            Token::Else => {
                self.advance();
                "else".to_string()
            }
            Token::While => {
                self.advance();
                "while".to_string()
            }
            Token::Fn => {
                self.advance();
                "fn".to_string()
            }
            _ => return Err(ParseError::UnexpectedToken(self.current_token().clone())),
        };

        self.expect(Token::Assign)?; // consume '='

        let builtin = match self.current_token() {
            Token::Identifier(builtin) => {
                let builtin = builtin.clone();
                self.advance();
                builtin
            }
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
            }
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
                }
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
            }
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
            }
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

                // Check if there's a block immediately following the call
                if *self.current_token() == Token::LeftBrace {
                    args.push(self.parse_block()?);
                }

                Ok(ASTNode::Call { name, args })
            }
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
            // Check if this is a block parameter { $identifier }
            if *self.current_token() == Token::LeftBrace {
                self.advance(); // consume '{'
                self.expect(Token::Dollar)?; // consume '$'

                let name = match self.current_token() {
                    Token::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        name
                    }
                    _ => return Err(ParseError::InvalidMacroPattern),
                };

                self.expect(Token::RightBrace)?; // consume '}'
                parameters.push(MacroParameter::Block(name));
            } else {
                // Regular $identifier parameter
                parameters.push(self.parse_macro_parameter()?);
            }

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
            }
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
            }
            Token::Float(val) => {
                self.advance();
                Ok(ASTNode::Literal(Literal::Float(val)))
            }
            Token::String(val) => {
                self.advance();
                Ok(ASTNode::Literal(Literal::String(val)))
            }
            Token::Boolean(val) => {
                self.advance();
                Ok(ASTNode::Literal(Literal::Boolean(val)))
            }
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
            }
            Token::LeftBrace => self.parse_block(),
            Token::If => self.parse_if(),
            Token::Dollar => {
                self.advance(); // consume '$'
                match self.current_token() {
                    Token::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        Ok(ASTNode::Identifier(name))
                    }
                    _ => Err(ParseError::InvalidExpression),
                }
            }
            Token::Bang => {
                self.advance(); // consume '!'
                let operand = Box::new(self.parse_primary()?);
                Ok(ASTNode::Call {
                    name: "!".to_string(),
                    args: vec![*operand],
                })
            }
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
}
