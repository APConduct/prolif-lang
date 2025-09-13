use std::{collections::HashMap, fmt};

use either::Either;
#[allow(unused_imports)]
use inkwell::{
    AddressSpace, OptimizationLevel,
    builder::BuilderError,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    targets::{InitializationConfig, Target},
    values::{BasicValueEnum, FunctionValue},
};

use crate::ast::{ASTNode, Literal};

#[derive(Debug)]
pub enum CodegenError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    TypeMismatch(String),
    LLVMError(String),
    UnsupportedFeature(String),
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CodegenError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            CodegenError::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
            CodegenError::TypeMismatch(msg) => write!(f, "Type mismatch: {}", msg),
            CodegenError::LLVMError(msg) => write!(f, "LLVM error: {}", msg),
            CodegenError::UnsupportedFeature(feature) => {
                write!(f, "Unsupported feature: {}", feature)
            }
        }
    }
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Result<Self, CodegenError> {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        // Initialize LLVM targets
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| CodegenError::LLVMError(format!("Failed to initialize target: {}", e)))?;

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| {
                CodegenError::LLVMError(format!("Failed to create execution engine: {}", e))
            })?;

        let mut codegen = CodeGenerator {
            context,
            module,
            builder,
            execution_engine,
            variables: HashMap::new(),
            functions: HashMap::new(),
        };

        // Create built-in functions
        codegen.create_builtins()?;

        Ok(codegen)
    }

    fn create_builtins(&mut self) -> Result<(), CodegenError> {
        // Create printf for print functionality
        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();

        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_fn = self.module.add_function("printf", printf_type, None);

        self.functions.insert("printf".to_string(), printf_fn);

        // Create a wrapper print function that takes a string
        let print_type = self
            .context
            .void_type()
            .fn_type(&[i8_ptr_type.into()], false);
        let print_fn = self.module.add_function("print", print_type, None);

        // Implement print function body
        let entry_block = self.context.append_basic_block(print_fn, "entry");
        self.builder.position_at_end(entry_block);

        let format_str = self
            .builder
            .build_global_string_ptr("%s\n", "format_str")
            .map_err(|e| {
                CodegenError::LLVMError(format!("Failed to create format string: {}", e))
            })?;

        let str_param = print_fn.get_nth_param(0).unwrap();
        self.builder
            .build_call(
                printf_fn,
                &[format_str.as_pointer_value().into(), str_param.into()],
                "printf_call",
            )
            .map_err(|e| CodegenError::LLVMError(format!("Failed to build call: {}", e)))?;

        self.builder
            .build_return(None)
            .map_err(|e| CodegenError::LLVMError(format!("Failed to build return: {}", e)))?;

        self.functions.insert("print".to_string(), print_fn);

        Ok(())
    }

    pub fn generate_from_ast(&mut self, nodes: Vec<ASTNode>) -> Result<(), CodegenError> {
        // Create main function
        let i32_type = self.context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_type, None);

        let entry_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry_block);

        // Generate code for each statement
        for node in nodes {
            self.generate_statement(node)?;
        }

        // Return 0 from main
        let zero = i32_type.const_int(0, false);
        self.builder
            .build_return(Some(&zero))
            .map_err(|e| CodegenError::LLVMError(format!("Failed to build return: {}", e)))?;

        self.functions.insert("main".to_string(), main_fn);

        Ok(())
    }

    fn generate_statement(
        &mut self,
        node: ASTNode,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        match node {
            ASTNode::Call { name, args } => self.generate_call(name, args).map(Some),

            ASTNode::Assignment { name, value } => {
                let val = self.generate_expression(*value)?;
                self.variables.insert(name, val);
                Ok(Some(val))
            }

            ASTNode::Block(statements) => {
                let mut last_value = None;
                for stmt in statements {
                    last_value = self.generate_statement(stmt)?;
                }
                Ok(last_value)
            }

            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => self.generate_if(*condition, *then_branch, else_branch.map(|e| *e)),

            ASTNode::PrimitiveDeclaration { .. } => {
                // Primitives are handled during parsing/expansion
                Ok(None)
            }

            ASTNode::MacroDefinition { .. } => {
                // Macros are handled during expansion phase
                Ok(None)
            }

            _ => self.generate_expression(node).map(Some),
        }
    }

    fn generate_expression(&mut self, node: ASTNode) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match node {
            ASTNode::Literal(literal) => self.generate_literal(literal),

            ASTNode::Identifier(name) => self
                .variables
                .get(&name)
                .copied()
                .ok_or_else(|| CodegenError::UndefinedVariable(name)),

            ASTNode::Call { name, args } => self.generate_call(name, args),

            ASTNode::Block(statements) => {
                let mut last_value = None;
                for stmt in statements {
                    if let Some(val) = self.generate_statement(stmt)? {
                        last_value = Some(val);
                    }
                }
                last_value.ok_or_else(|| CodegenError::TypeMismatch("Empty block".to_string()))
            }

            _ => Err(CodegenError::UnsupportedFeature(format!(
                "Expression type: {:?}",
                node
            ))),
        }
    }

    fn generate_literal(&self, literal: Literal) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match literal {
            Literal::Integer(val) => Ok(self
                .context
                .i64_type()
                .const_int(val as u64, val < 0)
                .into()),

            Literal::Float(val) => Ok(self.context.f64_type().const_float(val).into()),

            Literal::Boolean(val) => Ok(self
                .context
                .bool_type()
                .const_int(if val { 1 } else { 0 }, false)
                .into()),

            Literal::String(val) => {
                let string_ptr = self
                    .builder
                    .build_global_string_ptr(&val, "string_literal")
                    .map_err(|e| {
                        CodegenError::LLVMError(format!("Failed to create string: {}", e))
                    })?;
                Ok(string_ptr.as_pointer_value().into())
            }
        }
    }

    fn generate_call(
        &mut self,
        name: String,
        args: Vec<ASTNode>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Generate argument values
        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.generate_expression(arg)?);
        }

        // Look up function
        let function = self
            .functions
            .get(&name)
            .copied()
            .ok_or_else(|| CodegenError::UndefinedFunction(name.clone()))?;

        let arg_metadata_values: Vec<inkwell::values::BasicMetadataValueEnum> =
            arg_values.iter().map(|val| (*val).into()).collect();

        // Build call
        let call_site = self
            .builder
            .build_call(function, &arg_metadata_values, &format!("{}_call", name))
            .map_err(|e| CodegenError::LLVMError(format!("Failed to build call: {}", e)))?;

        // Return value (if any)
        match call_site.try_as_basic_value() {
            Either::Left(val) => Ok(val),
            Either::Right(_) => {
                // Void function, return unit value
                Ok(self.context.i32_type().const_int(0, false).into())
            }
        }
    }

    fn generate_if(
        &mut self,
        condition: ASTNode,
        then_branch: ASTNode,
        else_branch: Option<ASTNode>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let condition_val = self.generate_expression(condition)?;

        // Convert condition to i1 (boolean)
        let condition_bool = match condition_val {
            BasicValueEnum::IntValue(int_val) => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    int_val,
                    int_val.get_type().const_zero(),
                    "condition",
                )
                .map_err(|e| {
                    CodegenError::LLVMError(format!("Failed to build comparison: {}", e))
                })?,
            _ => {
                return Err(CodegenError::TypeMismatch(
                    "Condition must be integer".to_string(),
                ));
            }
        };

        let current_fn = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let then_block = self.context.append_basic_block(current_fn, "then");
        let else_block = self.context.append_basic_block(current_fn, "else");
        let merge_block = self.context.append_basic_block(current_fn, "merge");

        // Build conditional branch
        self.builder
            .build_conditional_branch(condition_bool, then_block, else_block)
            .map_err(|e| {
                CodegenError::LLVMError(format!("Failed to build conditional branch: {}", e))
            })?;

        // Generate then branch
        self.builder.position_at_end(then_block);
        let then_val = self.generate_statement(then_branch)?;
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(|e| CodegenError::LLVMError(format!("Failed to build branch: {}", e)))?;

        // Generate else branch
        self.builder.position_at_end(else_block);
        if let Some(else_node) = else_branch {
            self.generate_statement(else_node)?;
        }
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(|e| CodegenError::LLVMError(format!("Failed to build branch: {}", e)))?;

        // Continue at merge block
        self.builder.position_at_end(merge_block);

        // For now, just return the then value (proper phi nodes would be more complex)
        Ok(then_val)
    }

    pub fn print_ir(&self) {
        println!("Generated LLVM IR:");
        println!("{}", self.module.print_to_string().to_string());
    }

    pub fn execute_main(&self) -> Result<i32, CodegenError> {
        unsafe {
            let main_fn: JitFunction<unsafe extern "C" fn() -> i32> =
                self.execution_engine.get_function("main").map_err(|e| {
                    CodegenError::LLVMError(format!("Failed to get main function: {}", e))
                })?;
            // end here.
            Ok(main_fn.call())
        }
    }

    pub fn write_object_file(&self, filename: &str) -> Result<(), CodegenError> {
        use inkwell::targets::{CodeModel, RelocMode, Target, TargetMachine};
        use std::path::Path;

        // Initialize target
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| CodegenError::LLVMError(format!("Failed to initialize target: {}", e)))?;

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).map_err(|e| {
            CodegenError::LLVMError(format!("Failed to create target from triple: {}", e))
        })?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic", // CPU
                "",        // Features
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| {
                CodegenError::LLVMError("Failed to create target machine".to_string())
            })?;

        // Write object file
        target_machine
            .write_to_file(
                &self.module,
                inkwell::targets::FileType::Object,
                Path::new(filename),
            )
            .map_err(|e| CodegenError::LLVMError(format!("Failed to write object file: {}", e)))?;

        Ok(())
    }

    pub fn write_bitcode(&self, filename: &str) -> Result<(), CodegenError> {
        use std::path::Path;

        if self.module.write_bitcode_to_path(Path::new(filename)) {
            Ok(())
        } else {
            Err(CodegenError::LLVMError(
                "Failed to write bitcode".to_string(),
            ))
        }
    }

    pub fn write_llvm_ir(&self, filename: &str) -> Result<(), CodegenError> {
        use std::fs;

        let ir = self.module.print_to_string().to_string();
        fs::write(filename, ir)
            .map_err(|e| CodegenError::LLVMError(format!("Failed to write IR file: {}", e)))?;

        Ok(())
    }

    pub fn compile_to_executable(&self, output_filename: &str) -> Result<(), CodegenError> {
        use std::fs;
        use std::process::Command;

        let object_filename = format!("{}.o", output_filename);

        // Step 1: Generate object file
        self.write_object_file(&object_filename)?;

        // Step 2: Link with system linker (requires libc for printf)
        let output = Command::new("cc") // Use system C compiler as linker
            .arg("-o")
            .arg(output_filename)
            .arg(&object_filename)
            .output()
            .map_err(|e| CodegenError::LLVMError(format!("Failed to run linker: {}", e)))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(CodegenError::LLVMError(format!(
                "Linker failed: {}",
                stderr
            )));
        }

        // Clean up object file
        let _ = fs::remove_file(&object_filename);

        Ok(())
    }
}

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    // Symbol tables
    variables: HashMap<String, BasicValueEnum<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_codegen() {
        // Example AST:
        // primitive print = builtin_print
        // print("Hello, World!")
        use super::CodeGenerator;
        use crate::ast::{ASTNode, Literal};
        use inkwell::context::Context;

        let context = Context::create();
        let mut codegen = CodeGenerator::new(&context, "test_module").unwrap();

        let ast = vec![
            ASTNode::PrimitiveDeclaration {
                name: "print".to_string(),
                builtin: "builtin_print".to_string(),
            },
            ASTNode::Call {
                name: "print".to_string(),
                args: vec![ASTNode::Literal(Literal::String(
                    "Hello, World!".to_string(),
                ))],
            },
        ];

        codegen.generate_from_ast(ast).unwrap();
        codegen.print_ir();

        let result = codegen.execute_main().unwrap();
        assert_eq!(result, 0);
    }

    #[test]
    fn test_codegen_if() {
        // Example AST:
        // primitive print = builtin_print
        // if (1) {
        //     print("Condition is true")
        // } else {
        //     print("Condition is false")
        // }
        use super::CodeGenerator;
        use crate::ast::{ASTNode, Literal};
        use inkwell::context::Context;

        // let start = chrono::Local::now();

        // for _ in 0..1000 {
        // println!("START: {}", chrono::Local::now());
        let context = Context::create();
        let mut codegen = CodeGenerator::new(&context, "test_if_module").unwrap();
        let ast = vec![
            ASTNode::PrimitiveDeclaration {
                name: "print".to_string(),
                builtin: "builtin_print".to_string(),
            },
            ASTNode::If {
                condition: Box::new(ASTNode::Literal(Literal::Integer(1))),
                then_branch: Box::new(ASTNode::Call {
                    name: "print".to_string(),
                    args: vec![ASTNode::Literal(Literal::String(
                        "Condition is true".to_string(),
                    ))],
                }),
                else_branch: Some(Box::new(ASTNode::Call {
                    name: "print".to_string(),
                    args: vec![ASTNode::Literal(Literal::String(
                        "Condition is false".to_string(),
                    ))],
                })),
            },
        ];
        codegen.generate_from_ast(ast).unwrap();

        // println!("END: {}", chrono::Local::now());
        // codegen.print_ir();
        let result = codegen.execute_main().unwrap();
        assert_eq!(result, 0);
        // }
        // let end = chrono::Local::now();

        // println!("DURATION: {}", (end - start).num_milliseconds());

        // let result = codegen.execute_main().unwrap();
        // assert_eq!(result, 0);
    }

    #[test]
    fn test_executable_generation() {
        // Example AST:
        // primitive print = builtin_print
        // print("Hello, Executable!")
        use super::CodeGenerator;
        use crate::ast::{ASTNode, Literal};
        use inkwell::context::Context;

        let context = Context::create();
        let mut codegen = CodeGenerator::new(&context, "exec_module").unwrap();

        let ast = vec![
            ASTNode::PrimitiveDeclaration {
                name: "print".to_string(),
                builtin: "builtin_print".to_string(),
            },
            ASTNode::Call {
                name: "print".to_string(),
                args: vec![ASTNode::Literal(Literal::String(
                    "Hello, Executable!".to_string(),
                ))],
            },
        ];

        codegen.generate_from_ast(ast).unwrap();
        codegen.print_ir();

        // Compile to executable
        codegen.compile_to_executable("output_executable").unwrap();

        // Optionally, you can run the generated executable and check its output
        let output = std::process::Command::new("./output_executable")
            .output()
            .expect("Failed to execute generated executable");

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("Hello, Executable!"));
    }
}

#[ignore]
#[test]
fn test() -> Result<(), BuilderError> {
    // Initialize LLVM context
    let context = Context::create();
    let module = context.create_module("my_module");
    let builder = context.create_builder();

    // Create a function prototype: int add(int a, int b)
    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
    let function = module.add_function("add", fn_type, None);

    // Create a basic block and position the builder
    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);

    // Get function parameters
    let a = function.get_nth_param(0).unwrap().into_int_value();
    let b = function.get_nth_param(1).unwrap().into_int_value();

    // Create the addition instruction
    let sum = builder.build_int_add(a, b, "sum").unwrap();

    // Return the result
    builder.build_return(Some(&sum))?;

    // Print the generated LLVM IR
    module.print_to_stderr();

    Ok(())
}
