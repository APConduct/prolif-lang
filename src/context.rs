use std::collections::HashMap;

use crate::ast::{ASTNode, MacroPattern};

pub struct MacroContext {
    // Defined macros at current expansion stage
    pub macros: HashMap<String, Vec<MacroPattern>>,

    // Built-in primitives
    pub primitives: HashMap<String, String>,

    // Current expansion depth (for cycle detection)
    pub depth: usize,

    // Maximum allowed expansion depth
    pub max_depth: usize,

    // Variable bindings during pattern matching
    pub bindings: HashMap<String, ASTNode>,
}

impl MacroContext {
    pub fn new() -> Self {
        let mut primitives = HashMap::new();

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
}
