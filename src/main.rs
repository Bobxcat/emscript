use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use parse::parse;

use crate::{interpret::Runtime, token::tokenize};

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
mod interpret;
mod parse;
mod prim_tree;
mod rust_ast;
mod token;
/// The entire `tree` module is copied from the `ripstop` project on GitHub
mod tree;
mod verify;

fn main() {
    let raw = include_str!("test.em");

    //Build Token stream
    let tokens = tokenize(raw);
    if let Err(e) = tokens {
        println!("TokenizeError: {:#?}", e);
        return;
    }
    let tokens = tokens.unwrap();

    //Build AST
    let ast = parse(tokens.clone());
    if let Err(_e) = ast {
        println!("AST parsing error encountered");
        println!("\n==Tokens==\n{:#?}\n=========", tokens);
        return;
    }
    let ast = ast.unwrap();

    println!("==AST==\n{}=======\n", ast);

    //At this point, a runtime needs to be created to proceed
    let mut runtime = Runtime::new_init(&ast).unwrap();

    //Verify AST
    /*if let Err(e) = runtime.verify(&ast) {
        println!("Errors encountered verifying AST:\n");
        for e in e {
            println!("{e}\n");
        }
        return;
    }*/

    //Compile the AST to rust

    runtime.setup_target_dir_relative("./em_target/").unwrap();

    let rust_dir = runtime.compile_to_rust(&ast).unwrap();
    println!("rust_dir: {}", rust_dir.display());

    let wasm_path = runtime.compile_rust().unwrap();
    println!("wasm_dir: {}\n\n", wasm_path.display());

    println!("Running {}\n", wasm_path.display());
    runtime.run_wasm(wasm_path).unwrap();

    //Interpret the AST
    // let interpret_res = runtime.interpret_ast(&ast);
    // match interpret_res {
    //     Ok(val) => println!("Output: {}", val),
    //     Err(err) => println!("Runtime Error Encountered:\n{err}"),
    // }
}
