use std::{fmt::Display, ops::Range};

use pomelo::pomelo;

fn list_to_last_value_return(exprs: Vec<PrimNode<ASTNode>>) -> PrimNode<ASTNode> {
    let ctx = exprs.last().unwrap().val.context.clone();
    new_node(ASTNodeType::LastValueReturn, ctx, exprs)
}

fn new_node(
    t: ASTNodeType,
    ctx: StringContext,
    children: Vec<PrimNode<ASTNode>>,
) -> PrimNode<ASTNode> {
    PrimNode::new_with_children(ASTNode::new(t, ctx), children)
}

pomelo! {
    %include {
        use crate::*;
        use ast::{ ASTNode, ASTNodeType::*, StringContext, Value };
        use prim_tree::PrimNode;
        use tree::Tree;
        use verify::Type;
        use super::{list_to_last_value_return, new_node};
    }

    //This is based on recursion, so a large stack is necessary:
    // %stack_size 1000;

    //Every token needs to know its context
    %extra_token StringContext;

    %token #[derive(Clone,Debug)]
       pub enum Token {};

    // %type input ASTNode;
    // %type expr_seq Vec<ASTNode>;
    // %type expr_list Vec<ASTNode>;
    // %type expr ASTNode;

    %type input Tree<ASTNode>;
    %type expr_seq Vec<PrimNode<ASTNode>>;
    %type expr_list Vec<PrimNode<ASTNode>>;
    %type expr PrimNode<ASTNode>;

    %type Int i128;
    %type Int32 i32;
    %type String String;
    %type Ident String;
    %type MethodCallStart String;
    %type TypeDec Type;

    //TMP (so that the token(s) are defined)
    %nonassoc Comma;

    //Binary Operators

    //Assign is `right` so that `a = b = 1` is possible
    %nonassoc Semicolon;
    %right Assign;
    %left Lt Gt Le Ge Eq;
    %left Add Sub;
    %left Mul Div;

    input ::= expr_seq(L) {
        list_to_last_value_return(L).into()
    };

    //A *sequence* of expresssions is multiple expressions that happen to be next to eachother (such as the body of a method)
    expr_seq ::= expr(A) { vec![A] }
    expr_seq ::= expr_seq(mut L) expr(A) { L.push(A); L }

    //A *list* of expressions is a comma-seperated list of expressions (such as inputs to a method call)
    // expr_list ::= expr(A) { vec![A] }
    // expr_list ::= expr_list(mut L) Comma expr(A) { L.push(A); L }

    //Methods

    //Declaration (body is *not* just any expression)
    expr ::= Fn(ctx) MethodCallStart((_ctx, s)) RParen LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(MethodDef { name: s, return_type: Type::Void }, ctx, vec![body])
    }
    expr ::= Fn(ctx) MethodCallStart((_, s)) RParen Arrow TypeDec((_, return_type)) LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(MethodDef { name: s, return_type }, ctx, vec![body])
    }

    //Calling
    expr ::= MethodCallStart((ctx, s)) RParen { new_node(MethodCall { name: s }, ctx, vec![]) };

    //Adding scopes and enclosing with brackets
    expr ::= LBracket expr_seq(L) RBracket {
        list_to_last_value_return(L)
    };

    //Literals
    expr ::= Int((ctx, n)) { new_node(Literal { val: Value::Int { n } }, ctx, vec![]) }
    expr ::= Int32((ctx, n)) { new_node(Literal { val: Value::Int32 { n } }, ctx, vec![]) }
    expr ::= String((ctx, s)) { new_node(Literal { val: Value::String { s } }, ctx, vec![]) }

    //Identifiers
    expr ::= Ident((ctx, s)) { new_node(VariableRef { name: s }, ctx, vec![]) };
    expr ::= Let Ident((ctx, s)) Assign expr(rhs) { new_node(VariableDef { name: s }, ctx, vec![rhs]) };
    expr ::= LParen expr(A) RParen { A }
    expr ::= expr(A) Semicolon(ctx) { new_node(ValueConsume, ctx, vec![A]) };

    //Binary Ops
    expr ::= Ident((_, name)) Assign(ctx) expr(C) { new_node(Assign { name }, ctx, vec![C]) };
    //Arithmetic
    expr ::= expr(B) Add(ctx) expr(C) { new_node(Add, ctx, vec![B, C]) };
    expr ::= expr(B) Sub(ctx) expr(C) { new_node(Sub, ctx, vec![B, C]) };
    expr ::= expr(B) Mul(ctx) expr(C) { new_node(Mul, ctx, vec![B, C]) };
    expr ::= expr(B) Div(ctx) expr(C) { new_node(Div, ctx, vec![B, C]) };
    expr ::= expr(B) Eq(ctx) expr(C) { new_node(Eq, ctx, vec![B, C]) };
    expr ::= expr(B) Lt(ctx) expr(C) { new_node(Lt, ctx, vec![B, C]) };
    expr ::= expr(B) Gt(ctx) expr(C) { new_node(Gt, ctx, vec![B, C]) };
    expr ::= expr(B) Le(ctx) expr(C) { new_node(Le, ctx, vec![B, C]) };
    expr ::= expr(B) Ge(ctx) expr(C) { new_node(Ge, ctx, vec![B, C]) };
}

pub use parser::*;

use crate::{
    ast::{ASTNode, ASTNodeType, StringContext},
    prim_tree::PrimNode,
    tree::Tree,
};

pub fn parse(tokens: Vec<Token>) -> Result<Tree<ASTNode>, ()> {
    let mut p = Parser::new();
    for t in tokens {
        p.parse(t)?;
    }

    let data = p.end_of_input()?;

    Ok(data)
}
