use wasm_ast::{encoder, Module};

use crate::{ast::ASTNode, tree::Tree};

pub fn ast_to_wasm_bin(ast: Tree<ASTNode>) -> Vec<u8> {
    let mut module = Module::builder();
    let mod_built = module.build();

    todo!()
}
