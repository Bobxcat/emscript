#![feature(ptr_metadata)]
use std::{
    fs::{canonicalize, File},
    io::Read,
    path::{Path, PathBuf},
    str::FromStr,
};

use em_proc::generate_translation_with_sizes;
use interface::Interface;
use parse::parse;
use runtime::RuntimeCfg;
use wasmer::{Function, Instance, NativeFunc, Store};

use crate::{
    interface::{compile_api, MethodImport, WasmEnv},
    runtime::Runtime,
    token::tokenize,
};

#[macro_use]
extern crate lazy_static;
extern crate pomelo;
extern crate regex_lexer;
extern crate wasmer;
extern crate wasmer_compiler_cranelift;
// extern crate wasmer_compiler_llvm;
extern crate anyhow;
// extern crate multimap;
extern crate bimap;
extern crate wasmer_engine_universal;

mod ast;
mod c_ast;
mod interface;
mod ir;
mod parse;
mod prim_tree;
mod runtime;
mod token;
mod traits;
/// The entire `tree` module is copied from the `ripstop` project on GitHub
mod tree;
/// A module with helper methods in it
mod utils;
mod value;
mod verify;

/// Compilation steps:
/// - Generate an interface, providing methods for all exported methods (probably using `generate_translation_with_sizes!(..)`)
/// - Compile the API, which checks to make sure that the exports given were complete and correct ///then populates the imports///
/// - Generate an EmScript AST from the input code
/// - Translate EmScript AST to IR AST, keeping track of exported methods (NOTE: the name of a method and its wasm-imported name can be different)

fn compile_text(
    raw: &str,
    cfg: RuntimeCfg,
    interface: Interface,
    store: Store,
) -> anyhow::Result<Instance> {
    //Build Token stream
    let tokens = tokenize(raw)?;

    //Build AST
    let ast = parse(tokens.clone());
    if let Err(_e) = ast {
        println!("AST parsing error encountered");
        println!("\n==Tokens==\n{:#?}\n=========", tokens);
        return Err(anyhow::format_err!("AST parsing error encountered"));
    }
    let mut ast = ast.unwrap();

    println!("==AST==\n{}=======\n", ast);

    //At this point, a runtime needs to be created to proceed
    let mut runtime = Runtime::new_init(&ast, cfg, interface).unwrap();

    //Compile the AST to rust

    runtime.setup_target_dir_relative("./em_target/").unwrap();

    let _c_dir = runtime.compile_to_c(&mut ast).unwrap();

    let wasm_path = runtime.compile_c().unwrap();

    println!("Loading {}\n", wasm_path.display());
    let loaded_runtime = runtime.load_wasm(wasm_path, store)?;

    Ok(loaded_runtime)
}

//TODO:
//1- currently, a variable declaration listed in `no_mangle_vars` *can* overlap with tmp/mangled vars
//2- robust system for writing custom types in C. More importantly, recognize these identifiers and mangle them appropriately
//  - provide the ability to convert `C` code into a `C` ast, or at least to recognize identifiers
//  - IDEA: use format strings (or something like `$my_struct $my_ident`) to notate identifiers in the C code which reference
//    outside the method and need to be mangled (including struct names/fields).
//    _
//  - when writing a custom struct, able to define the fields of the struct using `Type` accompanied by an un-mangled name
//  - write the body of methods individually using these rules and provide the MethodDef info seperately

//HOW TO WRITE `.api` FILES:
//- Each .api file consists of a sequence of method declarations and type declarations
//  - Each method declaration is prefixed by either "export" or "import", signifying from the perspective of the runtime
//      - "export" => the method is implemented by the runtime, called by code (for ex, methods on a class)
//      - "import" => the method is implemented by the code, called by the runtime (for ex, game loop update methods)

//NOTES ON APIs AND CUSTOM TYPES:
//- Custom types are defined in .api files
//- Their state is held by the WASM code but compiled in such a way to be easily translatable to a `CustomObjRef` in Rust
//- Methods on classes are implemented as exported methods.
//  - Their implementation is in the Rust runtime, where they are given a mutable `CustomObjRef` and any parameters

fn main() -> anyhow::Result<()> {
    use runtime::OptLevel::*;

    let store = Store::default();
    let env = WasmEnv::default();

    let interface = {
        fn print_c(s: i32) {
            print!("{s}")
        }
        let mut interface = Interface::default();
        interface.insert(MethodImport {
            mod_name: "env".to_string(),
            method_name: "print".to_string(),
            f: Function::new_native_with_env(
                &store,
                env.clone(),
                generate_translation_with_sizes!(
                    fn print_c(i32); (1)
                ),
            ),
        });
        interface
    };

    let interface = {
        //Uses the workspace's `Cargo.toml`
        let mut f = File::open(r"./em/src/test.api")?;
        let mut s = String::new();
        f.read_to_string(&mut s)?;
        compile_api(&s, interface)?
    };

    println!("Compiled interface:\n{:#?}", interface);

    let raw = include_str!("test.em");
    let runtime = compile_text(
        raw,
        RuntimeCfg {
            print_cast: false,
            verbose_compile: false,
            opt_level: NoOpt,
        },
        interface,
        store,
    )?;

    //Now, call runtime methods
    {
        let f_hello: NativeFunc<(), i32> = runtime.exports.get_native_function("hello")?;
        let f_hello_ret = f_hello.call()?;

        println!("\nReturned from `hello(..)` call: {f_hello_ret}");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::utils::format_compact;

    #[test]
    fn test_format_compact() {
        const VALS: &[(u128, &str)] = &[
            (14300987, "y0L5"),
            (1943, "VL"),
            (u128::MAX, "7n42DGM5Tflk9n8mt7Fhc7"),
        ];
        for (num, str_desired) in VALS {
            assert_eq!(*str_desired, &format_compact(*num));
        }
    }
}
