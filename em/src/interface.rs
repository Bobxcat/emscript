use std::collections::HashMap;

use crate::{
    ast::ASTNode,
    interface::parse_interface::Parser,
    token::tokenize,
    tree::Tree,
    value::{Type, Value},
};

// use multi_map::MultiMap;
use pomelo::pomelo;
use wasmer::{Function, LazyInit, Memory, WasmerEnv};

use self::parse_interface::Token;

//1) parse and find all method declarations (along with info)
//2) runtime fills out callbacks for exports before compiling
//3) after compilation, runtime finds

struct InterfaceMethodDec {
    name: String,
    params: Vec<(Type, String)>,
    ret: Type,
    t: InterfaceMethodType,
}

impl InterfaceMethodDec {
    fn try_new(name: String, params: Vec<(String, String)>, ret: Type, t: &str) -> Option<Self> {
        Some(Self {
            name,
            params: params
                .into_iter()
                .map(|(t, name)| Ok((Type::try_from_str(&t)?, name)))
                .collect::<Result<Vec<_>, ()>>()
                .ok()?,
            ret,
            t: InterfaceMethodType::try_from_str(t)?,
        })
    }
}

enum InterfaceMethodType {
    /// Imported by the runtime from the code
    Import,
    /// Exported to the code by the runtime
    Export,
}

impl InterfaceMethodType {
    fn try_from_str(s: &str) -> Option<Self> {
        use InterfaceMethodType::*;
        match s {
            "import" => Some(Import),
            "export" => Some(Export),
            _ => None,
        }
    }
}

pomelo! {
    %include {
        use crate::*;
        use ast::{ ASTNode, ASTNodeType::*, StringContext };
        use prim_tree::PrimNode;
        use tree::Tree;
        use value::{Type, Value};
        use parse::{list_to_last_value_return, new_node};
        use super::InterfaceMethodDec;
        use super::InterfaceMethodType::*;
    }
    %module parse_interface;
    //Every token needs to know its context
    %extra_token StringContext;

    %type input Vec<InterfaceMethodDec>;

    %type Ident String;
    %type MethodCallStart String;
    %type method_seq Vec<InterfaceMethodDec>;
    %type method_dec InterfaceMethodDec;
    %type var_dec_list Vec<(String, String)>;
    %type expr PrimNode<ASTNode>;

    input ::= method_seq(L) {
        L
        // list_to_last_value_return(L).into()
    };

    //A *sequence* of expresssions is multiple expressions that happen to be next to eachother (such as the body of a method)
    method_seq ::= method_dec(A) { vec![A] }
    method_seq ::= method_seq(mut L) method_dec(A) { L.push(A); L }


    //A list of variable declarations is a comma-seperated list of pairs of `[type_name], [variable_name]` (such as: `bool a, bool b, i32 c`)
    var_dec_list ::= Ident((_, t_name)) Ident((_, name)) { vec![(t_name, name)] }
    var_dec_list ::= var_dec_list(mut L) Comma Ident((_, t_name)) Ident((_, name)) { L.push((t_name, name)); L }

    //Declaration (NO BODY -- implementation not provided in `.api` file)
    //Also MUST be prefixed by either `export` or `import`
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_ctx, s)) RParen Semicolon {
        InterfaceMethodDec::try_new(s, vec![], Type::Void, &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::Void, inputs: vec![] }, ctx, vec![])
    }
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_, s)) RParen Arrow Ident((_, ret)) Semicolon {
        InterfaceMethodDec::try_new(s, vec![], Type::try_from_str(&ret)?, &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::try_from_str(&ret)?, inputs: vec![] }, ctx, vec![])
    }
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_ctx, s)) var_dec_list(input_types) RParen Semicolon {
        InterfaceMethodDec::try_new(s, input_types, Type::Void, &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::Void, inputs: input_types.into_iter().map(|(t, name)| (Type::try_from_str(&t).unwrap(), name)).collect() }, ctx, vec![])
    }
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_, s)) var_dec_list(input_types) RParen Arrow Ident((_, ret)) Semicolon {
        InterfaceMethodDec::try_new(s, input_types, Type::try_from_str(&ret)?, &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::try_from_str(&ret)?, inputs: input_types.into_iter().map(|(t, s)| (Type::try_from_str(&t).unwrap(), s)).collect() }, ctx, vec![])
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MethodId(usize);

#[derive(WasmerEnv, Clone, Default)]
pub struct WasmEnv {
    #[wasmer(export)]
    pub memory: LazyInit<Memory>,
}

/// Represents a method import (from WASM perspective)
#[derive(Debug, Clone)]
pub struct MethodImport {
    pub mod_name: String,
    pub method_name: String,
    pub f: Function,
}

#[derive(Debug, Default, Clone)]
pub struct Interface {
    pub wasm_imports: Vec<MethodImport>,
}

impl Interface {
    pub fn new() -> Self {
        Self {
            wasm_imports: Vec::new(),
        }
    }
    fn get_import_by_name(&self, n: &str) -> Option<&MethodImport> {
        for imp in &self.wasm_imports {
            if imp.method_name == n {
                return Some(imp);
            }
        }
        None
    }
    /// Verifies that `self` covers exactly `interface_decs`
    fn verify(&self, interface_decs: &Vec<InterfaceMethodDec>) -> Result<(), APIErr> {
        for dec in interface_decs {
            //
        }
        Ok(())
    }
}

pub enum APIErr {
    //
}

fn token_to_interface_token(t: crate::parse::Token) -> Token {
    use Token::*;
    match t {
        crate::parse::Token::Ident(info) => Ident(info),
        crate::parse::Token::MethodCallStart(info) => MethodCallStart(info),
        crate::parse::Token::Comma(ctx) => Comma(ctx),
        crate::parse::Token::Semicolon(ctx) => Semicolon(ctx),
        crate::parse::Token::Fn(ctx) => Fn(ctx),
        // crate::parse::Token::LParen(ctx) => todo!(),
        crate::parse::Token::RParen(ctx) => RParen(ctx),
        //Brackets will be used for things like `export {}`, group identifying multiple methods as exports/imports
        // crate::parse::Token::LBracket(ctx) => todo!(),
        // crate::parse::Token::RBracket(ctx) => todo!(),
        crate::parse::Token::Arrow(ctx) => Arrow(ctx),
        _ => unimplemented!("Unexpected token in `.api` call:\n{}", t.extra()),
    }
}

/// Compile from the given api. The interface must cover all method exports
pub fn compile_api(raw: &str, interface: Interface) -> anyhow::Result<Interface> {
    let tokens = tokenize(raw)?;

    //Create parser and run it
    let mut p = Parser::new();
    for t in tokens {
        p.parse(token_to_interface_token(t))
            .map_err(|_| anyhow::format_err!("Parse error"))?;
    }

    //Collect the final AST
    let mut interface_decs = p
        .end_of_input()
        .map_err(|_| anyhow::format_err!("Parse EOI error"))?;

    interface.verify(&interface_decs)?;
    Ok(interface)
}
