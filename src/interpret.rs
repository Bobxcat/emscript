use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{ASTNode, Value},
    tree::{NodeId, Tree},
};

/// An error encountered during runtime. These should be few and far between, as emscript is designed to avoid a heavy runtime
///
/// In future, can be used to check for things like overflowing arithmetic (for debug builds)
#[derive(Debug, Clone)]
pub enum RuntimeErr {
    /// An unknown error was encountered during runtime, possibly due to not properly using `verify`
    Unchecked,
    /// A generic panic was encountered
    Panic(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MethodInfo {
    /// The node which contains the `MethodDef` definining the method
    declaration: NodeId,
    /// The node which contains the actual code of the method, the child of `declaration`
    body: NodeId,
}

impl MethodInfo {
    pub fn new(declaration: NodeId, ast: &Tree<ASTNode>) -> Self {
        MethodInfo {
            declaration,
            body: ast[declaration].children[0],
        }
    }
}

/// Defines a runtime used to interpret an AST
///
/// Can be cheaply cloned
#[derive(Debug, Clone)]
pub struct Runtime {
    /// Each method declaration stored as a pair of (method_name, (method_declaration, method_body))
    method_declarations: HashMap<String, MethodInfo>, //TODO: Define external methods, Rc<state>?
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            method_declarations: HashMap::new(),
        }
    }
    pub fn new_init(ast: &Tree<ASTNode>) -> Result<Self, RuntimeErr> {
        let mut r = Self::new();
        r.init(ast)?;
        Ok(r)
    }
    pub fn init(&mut self, ast: &Tree<ASTNode>) -> Result<(), RuntimeErr> {
        self.load_method_declarations(ast.find_head().ok_or(RuntimeErr::Unchecked)?, ast)?;

        Ok(())
    }
    /// Interprets an AST and returns a value produced if no errors are encountered
    pub fn interpret_ast(&self, ast: &Tree<ASTNode>) -> Result<Value, RuntimeErr> {
        let mut variables = HashMap::new();
        self.interpret(ast.find_head().unwrap(), ast, &mut variables)
    }
    fn load_method_declarations(
        &mut self,
        curr: NodeId,
        ast: &Tree<ASTNode>,
    ) -> Result<(), RuntimeErr> {
        use crate::ast::ASTNodeType::*;

        let children = ast[curr].children.clone();
        match &ast[curr].data.t {
            MethodDef { name, .. } => {
                let _ = self
                    .method_declarations
                    .insert(name.to_string(), MethodInfo::new(curr, ast));
            }
            _ => (),
        }
        for c in children {
            self.load_method_declarations(c, ast)?;
        }
        Ok(())
    }
    /// The backend of `interpret_ast`, which recurses on `ast`
    fn interpret(
        &self,
        curr: NodeId,
        ast: &Tree<ASTNode>,
        variables: &mut HashMap<String, Value>,
    ) -> Result<Value, RuntimeErr> {
        use crate::ast::ASTNodeType::*;

        let children = ast[curr].children.clone();

        //DUPLICATED from `type_check` (ish)
        //For now, define everything while descending the tree
        match &ast[curr].data.t {
            //Literals return themselves
            Literal { val } => Ok(val.clone()),
            //Binary ops
            Add => (self.interpret(children[0], ast, variables)?
                + self.interpret(children[1], ast, variables)?)
            .map_err(|_| RuntimeErr::Unchecked),
            Sub => (self.interpret(children[0], ast, variables)?
                - self.interpret(children[1], ast, variables)?)
            .map_err(|_| RuntimeErr::Unchecked),
            Mul => (self.interpret(children[0], ast, variables)?
                * self.interpret(children[1], ast, variables)?)
            .map_err(|_| RuntimeErr::Unchecked),
            Div => (self.interpret(children[0], ast, variables)?
                / self.interpret(children[1], ast, variables)?)
            .map_err(|_| RuntimeErr::Unchecked),

            Eq => (self.interpret(children[0], ast, variables)?
                == self.interpret(children[1], ast, variables)?)
            .map_err(|_| RuntimeErr::Unchecked),
            Lt => todo!(),
            Gt => todo!(),
            Le => todo!(),
            Ge => todo!(),
            //Assignment returns no value (much like `VariableDef`)
            Assign { name } => {
                let rhs = self.interpret(children[0], ast, variables)?;
                *variables.get_mut(name).unwrap() = rhs;
                Ok(Value::Void)
            }
            //Variable references return their underlying value
            VariableRef { name } => Ok(variables.get(name).ok_or(RuntimeErr::Unchecked)?.clone()),
            //A variable definition affects state, but does not return a value
            VariableDef { name } => {
                let val = self.interpret(children[0], ast, variables)?;
                variables.insert(name.clone(), val);
                Ok(Value::Void)
            }
            //MethodCall { name } => todo!(),
            //ValueConsume just executes children and returns `Void`
            ValueConsume => {
                self.interpret(children[0], ast, variables)?;
                Ok(Value::Void)
            }
            //LastValueReturn executes all children and returns the last value
            LastValueReturn => {
                let mut val = Value::Void;
                for c in children {
                    val = self.interpret(c, ast, variables)?;
                }
                Ok(val)
            }
            //Method definitions do nothing when interpreting. All methods are loaded pre-interpret
            MethodDef { .. } => Ok(Value::Void),
            MethodCall { name } => {
                self.interpret(self.method_declarations[name].body, ast, variables)
            }
        }
    }
}
