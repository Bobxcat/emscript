use parse::parse;

use crate::{interpret::Runtime, token::tokenize, verify::verify};

#[macro_use]
extern crate lazy_static;
extern crate pomelo;
extern crate regex_lexer;

mod ast;
mod interpret;
mod parse;
mod prim_tree;
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
        println!(
            "AST parsing error encountered\n==Tokens==\n{:#?}\n=========",
            tokens
        );
        return;
    }
    let ast = ast.unwrap();

    println!("==AST==\n{}=======\n", ast);

    //Verify AST
    if let Err(e) = verify(&ast) {
        println!("Errors encountered verifying AST:\n");
        for e in e {
            println!("{e}\n");
        }
        return;
    }

    //Interpret the AST
    let runtime = Runtime::new_init(&ast).unwrap();
    println!("Output: {}", runtime.interpret_ast(&ast).unwrap());
}
