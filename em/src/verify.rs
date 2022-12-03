use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{ASTNode, ASTNodeType, StringContext},
    runtime::Runtime,
    tree::{NodeId, Tree},
    value::{Type, Value},
};

#[derive(Debug, Clone)]
pub struct VerificationError {
    pub ctx: StringContext,
    pub t: VerificationErrorType,
}

impl VerificationError {
    pub fn new(ctx: StringContext, t: VerificationErrorType) -> Self {
        Self { ctx, t }
    }
}

impl Display for VerificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_text = match &self.t {
            VerificationErrorType::MismatchedTypes { expected, given } => {
                format!("Mismatched types. Expected `{expected}`, received `{given}`")
            }
            VerificationErrorType::UndefinedVar { name } => {
                format!("Variable referenced which was not yet defined: `{name}`")
            }
        };
        let context_text = format!("{}", self.ctx);
        write!(f, "Error: {err_text}\n{context_text}")
    }
}

impl Error for VerificationError {}

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
}
