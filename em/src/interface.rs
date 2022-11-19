use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    fmt::Display,
    sync::{Arc, Mutex, MutexGuard},
};

use crate::{
    ast::StringContext,
    interface::parse_interface::Token,
    token::tokenize,
    traits::GetRefFromMem,
    utils::MultiMap,
    value::{custom_types::str_to_type, CustomTypeId, Type, TypeOrName},
};

use em_proc::generate_translation_with_sizes;
use pomelo::pomelo;
use wasmer::{Function, LazyInit, Memory, Store, WasmerEnv};

use self::parse_interface::Parser;

lazy_static! {
    static ref PARSE_DATA: Mutex<ParseData> = {
        let p = ParseData::default();
        Mutex::new(p)
    };
}

//1) parse and find all method declarations (along with info)
//2) runtime fills out callbacks for exports before compiling
//3) after compilation, runtime finds

/// Data passed along while parsing
#[derive(Debug, Default, Clone)]
struct ParseData {
    method_decs: HashMap<String, InterfaceMethodDec>,
}

impl ParseData {
    fn insert_method(
        &mut self,
        ctx: StringContext,
        name: &str,
        params: Vec<(String, String)>,
        ret: &str,
    ) -> Option<InterfaceMethodDec> {
        //Map params
        let params = params
            .into_iter()
            //Map each type name into an actual type
            //Keep in mind that only interface-defined types are valid here
            .map(|(t, name)| {
                let t = str_to_type(&t);
                match t {
                    Some(t) => Some((t, name)),
                    None => None,
                }
            })
            .collect::<Option<Vec<_>>>()?;
        let ret = str_to_type(&ret)?;
        self.method_decs.insert(
            name.to_string(),
            InterfaceMethodDec {
                ctx,
                name: name.to_string(),
                params,
                ret,
                t: InterfaceMethodType::Export,
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceMethodDec {
    ctx: StringContext,
    name: String,
    params: Vec<(Type, String)>,
    ret: Type,
    t: InterfaceMethodType,
}

impl InterfaceMethodDec {
    fn try_new(
        ctx: StringContext,
        name: String,
        params: Vec<(Type, String)>,
        ret: Type,
        t: &str,
    ) -> Option<Self> {
        Some(Self {
            ctx,
            name,
            params,
            ret,
            t: InterfaceMethodType::try_from_str(t)?,
        })
    }
}

#[derive(Debug, Clone, Copy)]
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

//TODO: Find a way to pass `ParseDat` along when parsing each `method_dec`
//  Could use global state, would need to reset it on each parse (probably fine)
//  `lazy_static`, probably

pomelo! {
    %include {
        use crate::*;
        use ast::{ ASTNode, StringContext };
        use prim_tree::PrimNode;
        use tree::Tree;
        use value::{Type, TypeOrName};
        use super::InterfaceMethodDec;
        use super::InterfaceMethodType::*;
        use super::*;
    }
    %module parse_interface;
    //Every token needs to know its context
    %extra_token StringContext;

    // %type input ParseData;

    %type Ident String;
    %type MethodCallStart String;
    // %type method_seq ;
    // %type method_dec InterfaceMethodDec;
    %type var_dec_list Vec<(String, String)>;
    %type expr PrimNode<ASTNode>;

    input ::= method_seq(L) {
        L
        // list_to_last_value_return(L).into()
    };

    //A *sequence* of expresssions is multiple expressions that happen to be next to eachother (such as the body of a method)
    // method_seq ::= method_dec(dec) { let mut parse_dat = ParseData::default(); parse_dat.v.push(A); parse_dat }
    // method_seq ::= method_seq(mut m_seq) method_dec(dec) { m_seq.v.push(dec); m_seq }
    method_seq ::= method_dec {}
    method_seq ::= method_seq method_dec {}


    //A list of variable declarations is a comma-seperated list of pairs of `[type_name], [variable_name]` (such as: `bool a, bool b, i32 c`)
    var_dec_list ::= Ident((_, t_name)) Ident((_, name)) { vec![(t_name, name)] }
    var_dec_list ::= var_dec_list(mut L) Comma Ident((_, t_name)) Ident((_, name)) { L.push((t_name, name)); L }

    //Declaration (NO BODY -- implementation not provided in `.api` file)
    //Also MUST be prefixed by either `export` or `import`
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_ctx, s)) RParen Semicolon {
        // InterfaceMethodDec::try_new(ctx, s, vec![], Type::Void, &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::Void, inputs: vec![] }, ctx, vec![])
        //
        // PARSE_DATA.lock()?.
        lock_parse_data().unwrap().insert_method(ctx, &s, vec![], "()");
    }
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_, s)) RParen Arrow Ident((_, ret)) Semicolon {
        // InterfaceMethodDec::try_new(ctx, s, vec![], TypeOrName::from_str(&ret)?, &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::try_from_str(&ret)?, inputs: vec![] }, ctx, vec![])
        lock_parse_data().unwrap().insert_method(ctx, &s, vec![], &ret);
    }
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_ctx, s)) var_dec_list(input_types) RParen Semicolon {
        // InterfaceMethodDec::try_new(ctx, s, input_types, TypeOrName::T(Void), &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::Void, inputs: input_types.into_iter().map(|(t, name)| (Type::try_from_str(&t).unwrap(), name)).collect() }, ctx, vec![])
        lock_parse_data().unwrap().insert_method(ctx, &s, input_types, "()");
    }
    method_dec ::= Ident((ictx, t)) Fn(ctx) MethodCallStart((_, s)) var_dec_list(input_types) RParen Arrow Ident((_, ret)) Semicolon {
        // InterfaceMethodDec::try_new(ctx, s, input_types, TypeOrName::from_str(&ret)?, &t ).unwrap()
        // new_node(MethodDef { name: s, return_type: Type::try_from_str(&ret)?, inputs: input_types.into_iter().map(|(t, s)| (Type::try_from_str(&t).unwrap(), s)).collect() }, ctx, vec![])
        lock_parse_data().unwrap().insert_method(ctx, &s, input_types, &ret);
    }
}

fn lock_parse_data() -> anyhow::Result<MutexGuard<'static, ParseData>> {
    PARSE_DATA
        .lock()
        .map_err(|err| anyhow::format_err!("failed to lock `PARSE_DATA`: {:#?}", err))
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
    pub params: Vec<Type>,
    pub ret: Type,
    pub f: Function,
}

/// Represents a part of the standard library to be imported
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum StdImport {
    StdOut,
}

#[derive(Debug, Default, Clone)]
pub struct Interface {
    pub wasm_imports: HashMap<String, MethodImport>,
}

impl Interface {
    #[allow(unused)]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn new_with_std(std_imps: HashSet<StdImport>, store: &Store, env: &WasmEnv) -> Self {
        let mut interface = Self::default();
        interface.wasm_imports.reserve(std_imps.len());

        /// Inserts some number of method imports into the interface
        macro_rules! ins {
            ($($val:expr),*) => {
                $(interface.insert($val));*
            };
        }

        /// Generates a new import using the following information:
        /// * `env` -- the env name
        /// * `name` -- the name of the method
        /// * `translation` -- the code that would go inside of the appropriate
        ///     `generate_translation_with_sizes!(..)` macro call
        macro_rules! new_imp {
            ($env:literal, $name:literal, $params:expr, $ret:expr, $($translation:tt)+) => {
                MethodImport {
                    mod_name: $env.to_string(),
                    method_name: $name.to_string(),
                    params: $params,
                    ret: $ret,
                    f: Function::new_native_with_env(
                        &store,
                        env.clone(),
                        generate_translation_with_sizes!($($translation)*),
                    ),
                }
            };
        }

        //Loop through all the imports and add their corresponding implemenations
        for imp in std_imps {
            match imp {
                //StdOut, such as `print` and `print_num`
                StdImport::StdOut => {
                    fn c_print_num(n: i32) {
                        print!("{n}")
                    }
                    fn c_print(s: &str) {
                        print!("{s}")
                    }
                    fn c_println(s: &str) {
                        println!("{s}")
                    }
                    ins!(
                        new_imp!(
                            "env", "print_num", vec![Type::Int32], Type::Void, fn c_print_num(i32); (1)
                        ),
                        new_imp!("env", "print", vec![Type::Int32], Type::Void, fn c_print(&str); (1)),
                        new_imp!("env", "println", vec![Type::Int32], Type::Void, fn c_println(&str); (1))
                    );
                }
            };
        }

        interface
    }
    pub fn insert(&mut self, imp: MethodImport) -> Option<MethodImport> {
        self.wasm_imports.insert(imp.method_name.clone(), imp)
    }

    /// Verifies that `self` covers exactly `interface_decs`
    ///
    /// WARNING: Currently does no type checking (i.e. arguments/return types could be mismatched)
    fn verify(&self, interface_decs: &Vec<&InterfaceMethodDec>) -> Result<(), Vec<ApiErr>> {
        let mut wasm_imports_used = HashSet::new();
        let mut errs = Vec::new();
        for dec in interface_decs {
            //Make sure that the interface covers `dec`
            if let Some(imp) = self.wasm_imports.get(&dec.name) {
                wasm_imports_used.insert(imp.method_name.clone());
                continue;
            }
            //Otherwise, take note
            errs.push(ApiErr {
                ctx: dec.ctx.clone(),
                t: ApiErrType::MethodImportNotCoveredByInterface(dec.name.clone()),
            })
        }
        if errs.len() > 0 {
            Err(errs)
        } else {
            Ok(())
        }
        // todo!("Interface::verify -- type checking")
    }
}

#[derive(Debug, Clone)]
pub struct ApiErr {
    pub ctx: StringContext,
    pub t: ApiErrType,
}

#[derive(Debug, Clone)]
pub enum ApiErrType {
    MethodImportNotCoveredByInterface(String),
}

impl Display for ApiErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = match &self.t {
            ApiErrType::MethodImportNotCoveredByInterface(s) => {
                format!("Method import not covered by interface: `{s}`")
            }
        };
        write!(f, "API Error -- {}\n{}", t, self.ctx)
    }
}

impl Error for ApiErr {}

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
    let _interface_decs = p
        .end_of_input()
        .map_err(|_| anyhow::format_err!("Parse EOI error"))?;

    //Lock the parse data *after* finishing the parse
    let mut parse_data_lock = lock_parse_data()?;

    interface
        .verify(&parse_data_lock.method_decs.values().collect())
        .map_err(|err| {
            anyhow::format_err!(
                "Multiple errors encountered compiling API:\n{}",
                err.iter()
                    .map(|e| format!("{e}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        })?;

    //Reset the ParseData in case of a future parse
    *parse_data_lock = ParseData::default();

    Ok(interface)
}
