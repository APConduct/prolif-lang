use crate::{expansion::MacroExpander, lexer::Lexer, parser::Parser};

pub mod ast;
pub mod context;
pub mod expansion;
pub mod lexer;
pub mod parser;
pub mod tests;

fn main() {
    println!("Macro-Centric Language Implementation");
    println!("====================================");

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
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
}
