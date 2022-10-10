use std::collections::HashMap;

use crate::{
    ast::ASTNode,
    tree::{NodeId, Tree},
    verify::Type,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IRIdentID(usize);

pub enum IRNode {
    VarRef(IRIdentID),
    MethodDef {
        id: IRIdentID,
        params: Vec<(IRIdentID, Type)>,
        return_type: Type,
    },
}

/// An AST closely related to the EmScript AST, semantically.
///
/// Identifiers do not store the string themselves, instead a reference to which identifier they refer to
pub struct IRAST {
    pub tree: Tree<IRNode>,
    /// All identifiers in the AST
    ///
    /// TODO: how to deal with the difference between methods with parameter types & return type and variables,
    /// which just have a type?
    /// Possibly: store identifier info as an enum (method or variable). Or, have a seperate hashmap for methods
    pub idents: HashMap<IRIdentID, (String, Type)>,
}

impl IRAST {
    pub fn from_ast(ast: &Tree<ASTNode>) -> anyhow::Result<Self> {
        Self::from_ast_recurse(ast, ast.find_head().unwrap())
    }
    fn from_ast_recurse(ast: &Tree<ASTNode>, curr: NodeId) -> anyhow::Result<Self> {
        let children = ast[curr].children.clone();
        todo!()
    }
    fn new_ident(&mut self, ident_name: String) -> IRIdentID {
        let mut n = self.idents.len();
        while self.idents.contains_key(&IRIdentID(n)) {
            n += 1;
        }
        let id = IRIdentID(n);
        self.idents.insert(id, ident_name);

        id
    }
    /// Mangles the AST to guarantee that each identifier is globally unique
    pub fn mangle(&mut self) {
        let mut ident_count = 0;
        for var_name in self.idents.values_mut() {
            *var_name = format!("_{}_{}", ident_count, var_name);
            ident_count += 1;
        }
    }
}
