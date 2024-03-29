

use pomelo::pomelo;

pub(crate) fn list_to_last_value_return(exprs: Vec<PrimNode<ASTNode>>) -> PrimNode<ASTNode> {
    let ctx = exprs.last().unwrap().val.context.clone();
    new_node(ASTNodeType::LastValueReturn, ctx, exprs)
}

pub(crate) fn new_node(
    t: ASTNodeType,
    ctx: StringContext,
    children: Vec<PrimNode<ASTNode>>,
) -> PrimNode<ASTNode> {
    PrimNode::new_with_children(ASTNode::new(t, ctx), children)
}

pomelo! {
    %include {
        use crate::*;
        use ast::{ ASTNode, ASTNodeType::*, StringContext };
        use prim_tree::PrimNode;
        use tree::Tree;
        use value::{Type, Value, TypeOrName};
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
    %type var_dec_list Vec<(String, String)>;
    %type expr PrimNode<ASTNode>;

    %type Int i128;
    %type Int32 i32;
    %type String String;
    %type Bool bool;
    %type Ident String;
    %type MethodCallStart String;

    %nonassoc Comma;

    //Binary Operators

    //Assign is `right` so that `a = b = 1` is possible
    //^^Not actually a language feature at the moment, though
    %nonassoc Semicolon;
    %right Assign;
    %left Lt Gt Le Ge Eq Ne;
    %left Add Sub;
    %left Mul Div;

    %left Period;
    %nonassoc Ampersand;

    input ::= expr_seq(L) {
        list_to_last_value_return(L).into()
    };

    //A *sequence* of expresssions is multiple expressions that happen to be next to eachother (such as the body of a method)
    expr_seq ::= expr(A) { vec![A] }
    expr_seq ::= expr_seq(mut L) expr(A) { L.push(A); L }

    //A *list* of expressions is a comma-seperated list of expressions (such as inputs to a method call)
    expr_list ::= expr(A) { vec![A] }
    expr_list ::= expr_list(mut L) Comma expr(A) { L.push(A); L }

    //A list of variable declarations is a comma-seperated list of pairs of `[type_name], [variable_name]` (such as: `bool a, bool b, i32 c`)
    var_dec_list ::= Ident((_, t_name)) Ident((_, name)) { vec![(t_name, name)] }
    var_dec_list ::= var_dec_list(mut L) Comma Ident((_, t_name)) Ident((_, name)) { L.push((t_name, name)); L }

    //Methods

    //Declaration (body is *not* just any expression)
    expr ::= Fn(ctx) MethodCallStart((_ctx, s)) RParen LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(MethodDef { name: s, return_type: TypeOrName::T(Type::Void), inputs: vec![] }, ctx, vec![body])
    }
    expr ::= Fn(ctx) MethodCallStart((_, s)) RParen Arrow Ident((_, ret)) LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(MethodDef { name: s, return_type: TypeOrName::from_str(&ret), inputs: vec![] }, ctx, vec![body])
    }
    expr ::= Fn(ctx) MethodCallStart((_ctx, s)) var_dec_list(input_types) RParen LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(MethodDef { name: s, return_type: TypeOrName::T(Type::Void), inputs: input_types.into_iter().map(|(t, name)| (TypeOrName::from_str(&t), name)).collect() }, ctx, vec![body])
    }
    expr ::= Fn(ctx) MethodCallStart((_, s)) var_dec_list(input_types) RParen Arrow Ident((_, ret)) LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(MethodDef { name: s, return_type: TypeOrName::from_str(&ret), inputs: input_types.into_iter().map(|(t, s)| (TypeOrName::from_str(&t), s)).collect() }, ctx, vec![body])
    }

    //Calling
    expr ::= MethodCallStart((ctx, s)) RParen { new_node(MethodCall { name: s }, ctx, vec![]) };
    expr ::= MethodCallStart((ctx, s)) expr_list(inputs) RParen { new_node(MethodCall { name: s }, ctx, inputs) };

    //Control Flow and Conditionals
    expr ::= If(ctx) expr(condition) LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(IfCondition, ctx, vec![condition, body])
    }

    expr ::= Loop(ctx) LBracket expr_seq(L) RBracket {
        let body = list_to_last_value_return(L);
        new_node(Loop, ctx, vec![body])
    }

    expr ::= Break(ctx) {
        new_node(Break, ctx, vec![])
    }

    //Adding scopes and enclosing with brackets
    expr ::= LBracket expr_seq(L) RBracket {
        list_to_last_value_return(L)
    };

    //Custom types

    //Accessing member fields/methods
    expr ::= expr(A) Period Ident((ctx, rhs)) { new_node(FieldRef { field: rhs }, ctx, vec![A]) }

    //Literals
    expr ::= Int((ctx, n)) { new_node(Literal { val: Value::Int(n) }, ctx, vec![]) }
    expr ::= Int32((ctx, n)) { new_node(Literal { val: Value::Int32(n) }, ctx, vec![]) }
    expr ::= String((ctx, s)) { new_node(Literal { val: Value::String(s) }, ctx, vec![]) }
    expr ::= Bool((ctx, b)) { new_node(Literal { val: Value::Bool(b) }, ctx, vec![]) }

    //Identifiers
    expr ::= Ident((ctx, s)) { new_node(VariableRef { name: s }, ctx, vec![]) };
    expr ::= Ampersand(ctx) expr(A) { new_node(Reference, ctx, vec![A]) }; //&{expr}
    expr ::= Let Ident((ctx, s)) Assign expr(rhs) { new_node(VariableDef { name: s, t: None }, ctx, vec![rhs]) };
    expr ::= Let Ident((type_ctx, t)) Ident((ctx, s)) Assign expr(rhs) { new_node(VariableDef { name: s, t: Some(TypeOrName::from_str(&t)) }, ctx, vec![rhs]) };
    expr ::= LParen expr(A) RParen { A }
    expr ::= expr(A) Semicolon(ctx) { new_node(ValueConsume, ctx, vec![A]) };

    //Binary Ops
    expr ::= Ident((_, name)) Assign(ctx) expr(C) { new_node(Assign { name }, ctx, vec![C]) };
    //Arithmetic
    expr ::= expr(B) Add(ctx) expr(C) { new_node(Add, ctx, vec![B, C]) };
    expr ::= expr(B) Sub(ctx) expr(C) { new_node(Sub, ctx, vec![B, C]) };
    expr ::= expr(B) Mul(ctx) expr(C) { new_node(Mul, ctx, vec![B, C]) };
    expr ::= expr(B) Div(ctx) expr(C) { new_node(Div, ctx, vec![B, C]) };
    //Cmp
    expr ::= expr(B) Eq(ctx) expr(C) { new_node(Eq, ctx, vec![B, C]) };
    expr ::= expr(B) Ne(ctx) expr(C) { new_node(Ne, ctx, vec![B, C]) };
    expr ::= expr(B) Lt(ctx) expr(C) { new_node(Lt, ctx, vec![B, C]) };
    expr ::= expr(B) Gt(ctx) expr(C) { new_node(Gt, ctx, vec![B, C]) };
    expr ::= expr(B) Le(ctx) expr(C) { new_node(Le, ctx, vec![B, C]) };
    expr ::= expr(B) Ge(ctx) expr(C) { new_node(Ge, ctx, vec![B, C]) };
}

pub use parser::*;

use crate::{
    ast::{ASTNode, ASTNodeType, StringContext},
    prim_tree::PrimNode,
    tree::{NodeId, Tree},
};

fn set_scope_depths_recurse(ast: &mut Tree<ASTNode>, curr: NodeId, curr_scope: usize) {
    use ASTNodeType::*;

    ast[curr].data.scope_depth = curr_scope;
    let children = ast[curr].children.clone();
    /// Sets all children to have a scope depth which is `$scope_delta` higher than this level
    /// * `$scope_delta`
    macro_rules! set_all_children {
        ($scope_delta:expr) => {
            for c in children {
                set_scope_depths_recurse(ast, c, $scope_delta + curr_scope);
            }
        };
    }
    macro_rules! set_child {
        ($child_index:expr, $scope_delta:expr) => {
            set_scope_depths_recurse(ast, children[$child_index], $scope_delta + curr_scope)
        };
    }

    //Generally, scope increases for all children of a scope-increasing
    match &ast[curr].data.t {
        MethodDef { .. } | LastValueReturn => set_all_children!(1),
        IfCondition => {
            set_child!(0, 0);
            set_child!(1, 1);
        }
        _ => set_all_children!(0),
    };
}

pub fn parse(tokens: Vec<Token>) -> Result<Tree<ASTNode>, ()> {
    let mut p = Parser::new();
    for t in tokens {
        p.parse(t)?;
    }

    let mut ast = p.end_of_input()?;
    //Set the scope depth for the ast
    {
        let head = ast.find_head().unwrap();
        set_scope_depths_recurse(&mut ast, head, 0);
    }

    Ok(ast)
}
