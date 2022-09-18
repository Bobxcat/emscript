use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{ASTNode, ASTNodeType, Value},
    tree::{NodeId, Tree},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    Int32,
    String,
}

impl Type {
    /// Returns `true` if `self` is coercable to `other`.
    /// To be more precise, this is if implicit conversion from `self` to `other` is permissible
    fn coercable_to(self, other: Type) -> bool {
        use Type::*;
        //All the types that `self` can be coerced into. This always contains `self`
        let mut coercable_types = match self {
            Int => vec![Int32],
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

/// Verifies an AST, returning either `Ok(())` or a list of all errors encountered.
///
/// An `Err(_)` value is guaranteed to have at least one element
pub fn verify<'a>(ast: &'a Tree<ASTNode>) -> Result<(), Vec<VerificationError<'a>>> {
    //Type checking
    let mut variables = HashMap::new();
    type_check(ast.find_head().unwrap(), ast, &mut variables)?;

    Ok(())
}

fn type_check<'a>(
    curr: NodeId,
    ast: &'a Tree<ASTNode>,
    variables: &mut HashMap<String, Type>,
) -> Result<Type, Vec<VerificationError<'a>>> {
    use ASTNodeType::*;

    let children = ast[curr].children.clone();

    //DUPLICATED from `interpret` (ish)
    //For now, define everything while descending the tree

    Ok(match &ast[curr].data.t {
        //For literals and variable references, always return their type (implicit conversion checked later)
        Literal { val } => match val {
            Value::Void => Type::Void,
            Value::Int { .. } => Type::Int,
            Value::Int32 { .. } => Type::Int32,
            Value::String { .. } => Type::String,
        },
        //Binary ops
        Add | Sub | Mul | Div => {
            let lhs_t = type_check(children[0], ast, variables)?;
            let rhs_t = type_check(children[1], ast, variables)?;

            //`rhs` coerces into `lhs` by default
            if rhs_t.coercable_to(lhs_t) {
                lhs_t
            } else {
                //Return a mismatched type error as if `rhs` is the culprit
                return Err(vec![VerificationError::new(
                    &ast[children[1]].data,
                    VerificationErrorType::MismatchedTypes {
                        expected: lhs_t,
                        given: rhs_t,
                    },
                )]);
            }
        }
        //Unlike binary ops, `Assign` returns `Void` (but still needs type coercability)
        Assign { name } => {
            let lhs_t = variables[name];
            let rhs_t = type_check(children[0], ast, variables)?;

            //When assigning, `rhs` coerces into `lhs`
            if rhs_t.coercable_to(lhs_t) {
                Type::Void
            } else {
                //Return a mismatched type error where `rhs` is the culprit
                return Err(vec![VerificationError::new(
                    &ast[children[0]].data,
                    VerificationErrorType::MismatchedTypes {
                        expected: lhs_t,
                        given: rhs_t,
                    },
                )]);
            }
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
            let val_t = type_check(children[0], ast, variables)?;
            variables.insert(name.to_string(), val_t);
            Type::Void
        }
        //By definition, ValueConsume returns `Void`. Still need to check children though
        ValueConsume => {
            type_check(children[0], ast, variables)?;
            Type::Void
        }
        LastValueReturn => {
            let mut t = Type::Void;
            for c in children {
                t = type_check(c, ast, variables)?;
            }
            t
        }
        //Method definitions have no return type. Still need to check children
        MethodDef { .. } => {
            type_check(children[0], ast, variables)?;
            Type::Void
        }
        MethodCall { name } => todo!(),
    })
}
