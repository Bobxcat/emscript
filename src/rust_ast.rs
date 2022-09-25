use std::fmt::Display;

use crate::{
    ast::{ASTNode, ASTNodeType, Value},
    tree::{NodeId, Tree, TreeError},
};

/// Represents a node in an AST for *Rust* code
#[derive(Debug, Clone)]
pub enum RASTNode {
    //Abstract
    ///`0+` children
    LastValueReturn,

    MethodCall,
    MethodDef,

    //Trivial
    /// A literal value
    ///
    /// `0` children
    Literal(Value),
    /// `2` children
    Add,
    /// `2` children
    Sub,
}

impl Display for RASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self {
            RASTNode::Literal(val) => format!("{val}"),
            RASTNode::Add => format!("Add"),
            RASTNode::Sub => format!("Sub"),
            RASTNode::LastValueReturn => format!("Last value return"),
        };
        write!(f, "{}", s)
    }
}

pub fn rast_to_string(rast: &Tree<RASTNode>) -> String {
    format!(
        r##"
    #![no_std]
    #![allow(unused_braces)]
    
    // dev profile
    #[cfg(debug_assertions)]
    extern crate panic_semihosting;
    
    // release profile
    #[cfg(not(debug_assertions))]
    extern crate panic_halt;

    fn main() {{

    }}

    {}
    "##,
        //#[no_mangle] extern "C" fn foo(my_args: i32) -> ReturnType
        rast_to_string_recurse(rast, rast.find_head().expect("Could not find AST head"))
    )
}

fn rast_to_string_recurse(rast: &Tree<RASTNode>, curr: NodeId) -> String {
    let children = rast[curr].children.clone();

    /// Calls this recursive method on a given node
    macro_rules! child {
        ($node:expr) => {
            rast_to_string_recurse(rast, $node)
        };
    }

    match &rast[curr].data {
        RASTNode::Literal(val) => match val {
            Value::Void => format!("()"),
            Value::Bool(b) => format!("{b}"),
            Value::Int(n) => format!("{n}"),
            Value::Int32(n) => format!("{n}i32"),
            Value::String(s) => format!("{s}"),
        },
        RASTNode::Add => format!("{} + {}", child!(children[0]), child!(children[1])),
        RASTNode::Sub => format!("{} - {}", child!(children[0]), child!(children[1])),
        RASTNode::LastValueReturn => {
            let mut children_strings = Vec::new();
            for c in children {
                children_strings.push(child!(c));
            }
            format!("{{{}}}", children_strings.join(""))
        }
    }
}

pub fn ast_to_rast(ast: &Tree<ASTNode>) -> Tree<RASTNode> {
    // let mut rast = Tree::new();
    ast_to_rast_substitution(ast, ast.find_head().expect("Could not find AST head"))
        .expect("Tree error encountered in AST -> RAST substitution")

    // rast
}

/// A recursive method which works by substituting each node in `ast` with a corresponding subtree in `rast`
fn ast_to_rast_substitution(
    ast: &Tree<ASTNode>,
    curr_ast: NodeId,
) -> Result<Tree<RASTNode>, TreeError> {
    let children = ast[curr_ast].children.clone();

    let mut rtree = Tree::new();
    let rtree_parent;

    /// Adds a new node to `rtree` and sets `rtree_parent` equal to its `NodeId`
    macro_rules! add_child {
        ($child:expr) => {{
            /*let child_id = rast.new_node($child);
            rast.append_to(curr_rast, child_id)?;*/
            rtree_parent = rtree.new_node($child);
        }};
    }

    match &ast[curr_ast].data.t {
        ASTNodeType::Add => add_child!(RASTNode::Add),
        ASTNodeType::Sub => add_child!(RASTNode::Sub),
        ASTNodeType::Literal { val } => add_child!(RASTNode::Literal(val.clone())),
        ASTNodeType::VariableRef { name } => todo!(),
        ASTNodeType::VariableDef { name } => todo!(),
        ASTNodeType::MethodDef {
            name,
            inputs,
            return_type,
        } => todo!(),
        ASTNodeType::MethodCall { name } => todo!(),
        ASTNodeType::IfCondition => todo!(),
        ASTNodeType::Mul => todo!(),
        ASTNodeType::Div => todo!(),
        ASTNodeType::Eq => todo!(),
        ASTNodeType::Ne => todo!(),
        ASTNodeType::Lt => todo!(),
        ASTNodeType::Gt => todo!(),
        ASTNodeType::Le => todo!(),
        ASTNodeType::Ge => todo!(),
        ASTNodeType::Assign { name } => todo!(),
        ASTNodeType::LastValueReturn => add_child!(RASTNode::LastValueReturn),
        ASTNodeType::ValueConsume => todo!(),
    }

    for c in children {
        let mut subtree = ast_to_rast_substitution(ast, c)?;
        rtree.append_tree(rtree_parent, &mut subtree)?;
    }

    Ok(rtree)
}
