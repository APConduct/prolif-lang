use std::fmt;

use crate::{
    ast::{ASTNode, Literal, MacroParameter, MacroPattern},
    context::MacroContext,
};

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
            ExpandError::MaxDepthExceeded => write!(f, "Maximum macro expansion depth exceeded"),
            ExpandError::NoMatchingPattern(name) => {
                write!(f, "No matching pattern for macro '{}'", name)
            }
            ExpandError::InvalidParameter(msg) => write!(f, "Invalid macro parameter: {}", msg),
            ExpandError::CyclicExpansion => write!(f, "Cyclic macro expansion detected"),
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
                    value: Box::new(ASTNode::Literal(Literal::String("macro".to_string()))),
                })
            }

            ASTNode::PrimitiveDeclaration { name, builtin } => {
                self.context.define_primitive(name.clone(), builtin);
                Ok(ASTNode::Assignment {
                    name,
                    value: Box::new(ASTNode::Literal(Literal::String("primitive".to_string()))),
                })
            }

            ASTNode::Call { name, args } => self.expand_call(name, args),

            ASTNode::Block(nodes) => {
                let expanded: Result<Vec<_>, _> =
                    nodes.into_iter().map(|node| self.expand(node)).collect();
                Ok(ASTNode::Block(expanded?))
            }

            // Recursively expand other node types
            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
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
            }

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
        let expanded_args: Result<Vec<_>, _> =
            args.into_iter().map(|arg| self.expand(arg)).collect();

        Ok(ASTNode::Call {
            name,
            args: expanded_args?,
        })
    }

    fn try_pattern_match(
        &mut self,
        pattern: &MacroPattern,
        args: &[ASTNode],
    ) -> Result<ASTNode, ExpandError> {
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
                }
                MacroParameter::Block(name) => {
                    if let ASTNode::Block(_) = arg {
                        self.context.bindings.insert(name.clone(), arg.clone());
                    } else {
                        return false;
                    }
                }
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
            }

            ASTNode::Call { name, args } => {
                let substituted_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.substitute_bindings(arg))
                    .collect();

                ASTNode::Call {
                    name: name.clone(),
                    args: substituted_args,
                }
            }

            ASTNode::Block(nodes) => {
                let mut substituted_nodes = Vec::new();

                for node in nodes {
                    match node {
                        ASTNode::Identifier(name) => {
                            if let Some(binding) = self.context.bindings.get(name) {
                                // If the binding is a block, flatten its contents
                                if let ASTNode::Block(inner_nodes) = binding {
                                    substituted_nodes.extend(inner_nodes.clone());
                                } else {
                                    substituted_nodes.push(binding.clone());
                                }
                            } else {
                                substituted_nodes.push(node.clone());
                            }
                        }
                        _ => {
                            substituted_nodes.push(self.substitute_bindings(node));
                        }
                    }
                }

                ASTNode::Block(substituted_nodes)
            }

            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let substituted_condition = Box::new(self.substitute_bindings(condition));
                let substituted_then = Box::new(self.substitute_bindings(then_branch));
                let substituted_else = else_branch
                    .as_ref()
                    .map(|e| Box::new(self.substitute_bindings(e)));

                ASTNode::If {
                    condition: substituted_condition,
                    then_branch: substituted_then,
                    else_branch: substituted_else,
                }
            }

            // TODO: Handle other node types
            _ => ast.clone(),
        }
    }
}
