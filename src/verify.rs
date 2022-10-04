use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{ASTNode, ASTNodeType, Value},
    runtime::Runtime,
    tree::{NodeId, Tree},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    Bool,
    Int,
    Int32,
    String,
}

impl Type {
    /// Returns `true` if `self` is coercable to `other`.
    /// To be more precise, this is if implicit conversion from `self` to `other` is permissible
    fn coercable_to(self, other: Type) -> bool {
        use Type::*;
        //All the types that `self` can be coerced into. This will always contain `self` by default
        let mut coercable_types = match self {
            Int => vec![Int32],
            Int32 => vec![Int],
            _ => Vec::new(),
        };
        coercable_types.push(self);

        //Return whether or not `other` is a type coercable from `self`
        coercable_types.contains(&other)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Void => "void",
                Type::Bool => "bool",
                Type::Int => "integer",
                Type::Int32 => "i32",
                Type::String => "string",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct VerificationError<'a> {
    pub origin: &'a ASTNode,
    pub t: VerificationErrorType,
}

impl<'a> VerificationError<'a> {
    pub fn new(origin: &'a ASTNode, t: VerificationErrorType) -> Self {
        Self { origin, t }
    }
}

impl Display for VerificationError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_text = match &self.t {
            VerificationErrorType::MismatchedTypes { expected, given } => {
                format!("Mismatched types. Expected `{expected}`, received `{given}`")
            }
            VerificationErrorType::UndefinedVar { name } => {
                format!("Variable referenced which was not yet defined: `{name}`")
            }
        };
        let context_text = format!("{}", self.origin.context);
        write!(f, "Error: {err_text}\n{context_text}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationErrorType {
    /// An expression has types which are invalid for the given operation
    MismatchedTypes {
        expected: Type,
        given: Type,
    },
    UndefinedVar {
        name: String,
    },
    //UnknownType, <<for when
}

impl Runtime {
    /// Verifies an AST, returning either `Ok(())` or a list of all errors encountered.
    ///
    /// An `Err(_)` value is guaranteed to have at least one element
    pub fn verify<'a>(&self, ast: &'a Tree<ASTNode>) -> Result<(), Vec<VerificationError<'a>>> {
        //Type checking
        self.type_check(ast.find_head().unwrap(), ast, &mut HashMap::new())?;

        Ok(())
    }
    fn type_check<'a>(
        &self,
        curr: NodeId,
        ast: &'a Tree<ASTNode>,
        variables: &mut HashMap<String, Type>,
    ) -> Result<Type, Vec<VerificationError<'a>>> {
        use ASTNodeType::*;

        /// A macro to shorten `self.type_check(..)` calls
        macro_rules! type_check {
            ($curr:expr) => {
                self.type_check($curr, ast, variables)
            };
        }
        let children = ast[curr].children.clone();

        let mut errs = Vec::new();

        //DUPLICATED from `interpret` (ish)
        //For now, define everything while descending the tree

        let res = Ok(match &ast[curr].data.t {
            //For literals and variable references, always return their type (implicit conversion checked later)
            Literal { val } => match val {
                Value::Void => Type::Void,
                Value::Bool(_) => Type::Bool,
                Value::Int(_) => Type::Int,
                Value::Int32(_) => Type::Int32,
                Value::String(_) => Type::String,
            },
            //Binary ops
            Add | Sub | Mul | Div => {
                let lhs_t = type_check!(children[0])?;
                let rhs_t = type_check!(children[1])?;

                //`rhs` coerces into `lhs` by default
                if !rhs_t.coercable_to(lhs_t) {
                    //println!("Err: {rhs_t}-x>{lhs_t}");
                    //Return a mismatched type error as if `rhs` is the culprit
                    errs.push(VerificationError::new(
                        &ast[children[1]].data,
                        VerificationErrorType::MismatchedTypes {
                            expected: lhs_t,
                            given: rhs_t,
                        },
                    ));
                }

                lhs_t
            }
            Eq | Ne | Lt | Gt | Le | Ge => {
                let lhs_t = type_check!(children[0])?;
                let rhs_t = type_check!(children[1])?;

                //`rhs` coerces into `lhs` by default
                //TODO: Seperate restraints for `cmp` and `coerce` (or `arithmetic`)
                //When assigning, `rhs` coerces into `lhs`
                if !rhs_t.coercable_to(lhs_t) {
                    //Return a mismatched type error where `rhs` is the culprit
                    errs.push(VerificationError::new(
                        &ast[children[0]].data,
                        VerificationErrorType::MismatchedTypes {
                            expected: lhs_t,
                            given: rhs_t,
                        },
                    ));
                }
                Type::Bool
            }
            //Unlike binary ops, `Assign` returns `Void` (but still needs type coercability)
            Assign { name } => {
                let rhs_t = type_check!(children[0])?;
                let lhs_t = variables[name];

                //When assigning, `rhs` coerces into `lhs`
                if !rhs_t.coercable_to(lhs_t) {
                    //Return a mismatched type error where `rhs` is the culprit
                    errs.push(VerificationError::new(
                        &ast[children[0]].data,
                        VerificationErrorType::MismatchedTypes {
                            expected: lhs_t,
                            given: rhs_t,
                        },
                    ));
                }
                Type::Void
            }
            VariableRef { name } => {
                let t = variables.get(name).ok_or(vec![VerificationError::new(
                    &ast[curr].data,
                    VerificationErrorType::UndefinedVar {
                        name: name.to_string(),
                    },
                )])?;
                *t
            }
            //VariableDef acts like `Assign` in terms of
            VariableDef { name } => {
                let val_t = type_check!(children[0])?;
                variables.insert(name.to_string(), val_t);
                Type::Void
            }
            //By definition, ValueConsume returns `Void`. Still need to check children though
            ValueConsume => {
                type_check!(children[0])?;
                Type::Void
            }
            LastValueReturn => {
                let mut t = Type::Void;
                for c in children {
                    t = type_check!(c)?;
                }
                t
            }
            //Method definitions have no return type. But, the return type of the method itself must match its body
            MethodDef {
                name,
                return_type,
                inputs,
                ..
            } => {
                //The method should already be loaded when this method is called, so:
                let method_info = &self.method_declarations[name];

                //Add all the parameters as variables in order to call the body
                for (t, name) in inputs {
                    variables.insert(name.clone(), *t);
                }

                //The body needs to be coercable to the return
                let body = type_check!(method_info.body)?;
                if !body.coercable_to(*return_type) {
                    errs.push(VerificationError::new(
                        &ast[children[0]].data,
                        VerificationErrorType::MismatchedTypes {
                            expected: *return_type,
                            given: body,
                        },
                    ));
                }
                Type::Void
            }
            MethodCall { name } => {
                let method_info = &self.method_declarations[name];
                //Each parameter must have a coercable type
                for i in 0..method_info.inputs.len() {
                    let (expected, _) = method_info.inputs[i];
                    let param_t = type_check!(children[i])?;
                    if !param_t.coercable_to(expected) {
                        errs.push(VerificationError::new(
                            &ast[children[0]].data,
                            VerificationErrorType::MismatchedTypes {
                                expected,
                                given: param_t,
                            },
                        ));
                    }
                }

                method_info.return_type
            }
            //Raw if statements, that is ones without any else clause, need to always return void
            IfCondition => {
                let condition = type_check!(children[0])?;
                let body = type_check!(children[1])?;
                //The condition needs to be a `bool`
                if condition != Type::Bool {
                    errs.push(VerificationError::new(
                        &ast[children[0]].data,
                        VerificationErrorType::MismatchedTypes {
                            expected: Type::Bool,
                            given: condition,
                        },
                    ))
                }
                //The body needs to be a `void`
                if body != Type::Void {
                    errs.push(VerificationError::new(
                        &ast[children[1]].data,
                        VerificationErrorType::MismatchedTypes {
                            expected: Type::Void,
                            given: body,
                        },
                    ))
                }
                Type::Void
            }
        });

        if !errs.is_empty() {
            return Err(errs);
        }

        res
    }
}
