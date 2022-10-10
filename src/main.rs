use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use parse::parse;
use runtime::RuntimeCfg;

use crate::{runtime::Runtime, token::tokenize};

#[macro_use]
extern crate lazy_static;
extern crate pomelo;
extern crate regex_lexer;
extern crate wasmer;
extern crate wasmer_compiler_cranelift;
// extern crate wasmer_compiler_llvm;
extern crate anyhow;
extern crate wasmer_engine_universal;

mod ast;
mod c_ast;
mod ir;
mod parse;
mod prim_tree;
mod runtime;
mod rust_ast;
mod token;
/// The entire `tree` module is copied from the `ripstop` project on GitHub
mod tree;
mod verify;

fn compile_text(raw: &str, cfg: RuntimeCfg) -> anyhow::Result<Runtime> {
    //Build Token stream
    let tokens = tokenize(raw)?;
    // if let Err(e) = tokens {
    //     println!("TokenizeError: {:#?}", e);
    //     return Err(e);
    // }
    // let tokens = tokens.unwrap();

    //Build AST
    let ast = parse(tokens.clone());
    if let Err(_e) = ast {
        println!("AST parsing error encountered");
        println!("\n==Tokens==\n{:#?}\n=========", tokens);
        return Err(anyhow::format_err!("AST parsing error encountered"));
    }
    let mut ast = ast.unwrap();

    // println!("==AST==\n{}=======\n", ast);

    //At this point, a runtime needs to be created to proceed
    let mut runtime = Runtime::new_init(&ast, cfg).unwrap();

    // Verify AST
    // if let Err(e) = runtime.verify(&ast) {
    // println!("Errors encountered verifying AST:\n");
    // for e in e {
    // println!("{e}\n");
    // }
    // return;
    // }

    //Compile the AST to rust

    runtime.setup_target_dir_relative("./em_target/").unwrap();

    let _c_dir = runtime.compile_to_c(&mut ast).unwrap();
    // println!("c_dir: {}", c_dir.display());

    let wasm_path = runtime.compile_c().unwrap();
    // println!("wasm_dir: {}\n\n", wasm_path.display());

    println!("Running {}\n", wasm_path.display());
    runtime.run_wasm(wasm_path).unwrap();

    Ok(runtime)
}

fn main() -> anyhow::Result<()> {
    use runtime::OptLevel::*;

    let raw = include_str!("test.em");
    let runtime = compile_text(
        raw,
        RuntimeCfg {
            print_cast: false,
            verbose_compile: false,
            opt_level: Debug,
        },
    )?;
    Ok(())
}
