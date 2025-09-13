#[cfg(test)]
mod tests {
    use crate::{
        ast::{ASTNode, Literal, MacroParameter, MacroPattern},
        expansion::MacroExpander,
        lexer::{Lexer, Token},
        parser::Parser,
    };

    #[test]
    fn test_lexer_basic_tokens() {
        let mut lexer = Lexer::new("= -> | $ ... ( ) { }".to_string());
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
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
            ]
        );
    }

    #[test]
    fn test_lexer_identifiers_and_keywords() {
        let mut lexer = Lexer::new("primitive fn if else when true false".to_string());
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                Token::Primitive,
                Token::Fn,
                Token::If,
                Token::Else,
                Token::Identifier("when".to_string()),
                Token::Boolean(true),
                Token::Boolean(false),
                Token::Eof,
            ]
        );
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
            }
            _ => panic!("Expected PrimitiveDeclaration"),
        }
    }

    #[test]
    fn test_parser_macro_definition() {
        let mut lexer =
            Lexer::new("when = | $condition { $body } -> if $condition { $body }".to_string());
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
            }
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
            }
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

        // Define the macro by expanding a macro definition
        let macro_def = ASTNode::MacroDefinition {
            name: "when".to_string(),
            patterns: vec![when_pattern],
        };

        expander.expand(macro_def).unwrap();

        // Test expansion of: when(true, { print("hello") })
        let macro_call = ASTNode::Call {
            name: "when".to_string(),
            args: vec![
                ASTNode::Literal(Literal::Boolean(true)),
                ASTNode::Block(vec![ASTNode::Call {
                    name: "print".to_string(),
                    args: vec![ASTNode::Literal(Literal::String("hello".to_string()))],
                }]),
            ],
        };

        let result = expander.expand(macro_call).unwrap();

        // Should expand to: if true { print("hello") }
        match result {
            ASTNode::If {
                condition,
                then_branch: _,
                else_branch,
            } => {
                assert_eq!(*condition, ASTNode::Literal(Literal::Boolean(true)));
                assert!(else_branch.is_none());
            }
            _ => panic!("Expected If node"),
        }
    }

    #[test]
    fn test_block_flattening() {
        let mut expander = MacroExpander::new();

        // Define a 'when' macro: when = | $condition { $body } -> if $condition { $body }
        let when_pattern = MacroPattern {
            parameters: vec![
                MacroParameter::Single("condition".to_string()),
                MacroParameter::Block("body".to_string()),
            ],
            expansion: Box::new(ASTNode::If {
                condition: Box::new(ASTNode::Identifier("condition".to_string())),
                then_branch: Box::new(ASTNode::Block(vec![ASTNode::Identifier(
                    "body".to_string(),
                )])),
                else_branch: None,
            }),
            guard: None,
        };

        // Define the macro by expanding a macro definition
        let macro_def = ASTNode::MacroDefinition {
            name: "when".to_string(),
            patterns: vec![when_pattern],
        };

        expander.expand(macro_def).unwrap();

        // Test expansion of: when(true, { print("hello") })
        let macro_call = ASTNode::Call {
            name: "when".to_string(),
            args: vec![
                ASTNode::Literal(Literal::Boolean(true)),
                ASTNode::Block(vec![ASTNode::Call {
                    name: "print".to_string(),
                    args: vec![ASTNode::Literal(Literal::String("hello".to_string()))],
                }]),
            ],
        };

        let result = expander.expand(macro_call).unwrap();

        // Should expand to: if true { print("hello") } with proper block flattening
        match result {
            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert_eq!(*condition, ASTNode::Literal(Literal::Boolean(true)));
                assert!(else_branch.is_none());

                // Verify that the block is flattened - should contain the print call directly
                match then_branch.as_ref() {
                    ASTNode::Block(statements) => {
                        assert_eq!(statements.len(), 1);
                        match &statements[0] {
                            ASTNode::Call { name, args } => {
                                assert_eq!(name, "print");
                                assert_eq!(args.len(), 1);
                                match &args[0] {
                                    ASTNode::Literal(Literal::String(s)) => assert_eq!(s, "hello"),
                                    _ => panic!("Expected string literal"),
                                }
                            }
                            _ => panic!("Expected Call node"),
                        }
                    }
                    _ => panic!("Expected Block node"),
                }
            }
            _ => panic!("Expected If node"),
        }
    }
}
