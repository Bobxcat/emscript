#![feature(ptr_metadata)]
use std::{fs::File, io::Read};

use em_proc::generate_translation_with_sizes;
use interface::Interface;
use parse::parse;
use runtime::RuntimeCfg;
use wasmer::{Function, Instance, NativeFunc, Store, WasmPtr};

use crate::{
    interface::{compile_api, MethodImport, StdImport, WasmEnv},
    runtime::Runtime,
    token::tokenize,
    traits::{wasm_ptr_as_ref_mut, GetRefFromMem},
    value::{
        custom_types::{custom_types, insert_custom_type, str_to_type},
        CustomType, CustomTypeImpl, Type,
    },
};

#[macro_use]
extern crate lazy_static;
extern crate pomelo;
extern crate regex_lexer;
extern crate wasmer;
extern crate wasmer_compiler_cranelift;
// extern crate wasmer_compiler_llvm;
extern crate anyhow;
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
        // println!("\n==Tokens==\n{:#?}\n=========", tokens);
        return Err(anyhow::format_err!("AST parsing error encountered"));
    }
    let mut ast = ast.unwrap();

    // println!("==AST==\n{}=======\n", ast);
    // panic!();

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
//2- Declare all code-defined methods at the top of the file, show their declarations in arbitrary order

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

//Custom types
//1- When parsing `.api`, collect a HashMap of custom types. These are marked as external
//2-
//3- When generating IRAST, include custom types in `IdentStack` -- this requires type declarations be ordered, fine for now
//                                                                      ^^This caveat is also true for methods
//      - Note that the custom types introduced in AST are not marked as external
//4- When generating CAST: for now, replace custom type declarations with struct defs in C in-order.
//      - Note that typedefs in C are ordered

//Dirty fix to deal with stack overflow for now (without having to use `--release`)
fn main() {
    //Launch the `start` method on a second thread, with controlled stack size, and block for it
    let res = {
        use std::thread::*;
        const KB: usize = 1024;
        const MB: usize = 1024 * KB;
        const STACK_MB: usize = 4;

        Builder::new()
            .stack_size(STACK_MB * MB)
            .spawn(start)
            .unwrap()
            .join()
            .unwrap()
    };

    if let Err(e) = res {
        println!("Error encountered:\n{}", e);
    }
}

fn start() -> anyhow::Result<()> {
    use runtime::OptLevel::*;

    let store = Store::default();
    let env = WasmEnv::default();

    let interface = {
        use StdImport::*;
        let mut i = Interface::new_with_std(vec![StdOut].into_iter().collect(), &store, &env);

        //Custom Types

        // //`String`
        // {
        //     let t = CustomType {
        //         name: "String".into(),
        //         fields: vec![("start".into(), WasmPtr<u8>)].collect(),
        //         implementation: todo!(),
        //     };
        //     insert_custom_type(t);
        // }

        //`Foo`
        {
            let t = CustomType {
                name: "Foo".into(),
                mangled_name: None,
                fields: vec![("a".into(), Type::Int32), ("b".into(), Type::Int32)]
                    .into_iter()
                    .collect(),
                implementation: CustomTypeImpl::Shared,
            };
            insert_custom_type(t);
        }

        //Methods

        //`Foo`
        {
            #[repr(C)]
            struct Foo {
                a: i32,
                b: i32,
            }
            impl GetRefFromMem for Foo {
                fn as_mem_ref_mut<'a>(
                    offset: usize,
                    mem: &'a wasmer::Memory,
                ) -> Option<&'a mut Self> {
                    unsafe {
                        let p = mem.view::<u8>()[offset].as_ptr() as *mut Self;
                        Some(&mut *p)
                    }
                }
            }
            //`foo_new`
            {
                fn foo_new(a: i32, b: i32) -> Foo {
                    Foo { a, b }
                }
                i.insert(MethodImport {
                    mod_name: "env".into(),
                    method_name: "foo_new".into(),
                    params: vec![Type::Int32, Type::Int32],
                    ret: str_to_type("Foo").unwrap(),
                    f: Function::new_native_with_env(
                        &store,
                        env.clone(),
                        generate_translation_with_sizes!(fn foo_new(i32, i32) -> Foo; (1, 1) -> 2),
                    ),
                });
            }

            //`foo_fib`
            {
                fn foo_fib(foo: &mut Foo) {
                    foo.a = foo.a + foo.b;
                    std::mem::swap(&mut foo.a, &mut foo.b);
                    println!("{}, {}", foo.a, foo.b);
                }
                i.insert(MethodImport {
                    mod_name: "env".into(),
                    method_name: "foo_fib".into(),
                    params: vec![Type::Ref(Box::new(str_to_type("Foo").unwrap()))],
                    ret: Type::Void,
                    f: Function::new_native_with_env(
                        &store,
                        env.clone(),
                        generate_translation_with_sizes!(fn foo_fib(&mut Foo); (1)),
                    ),
                });
            }
        }

        //`add`
        {
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }
            i.insert(MethodImport {
                mod_name: "env".into(),
                method_name: "add".into(),
                params: vec![Type::Int32, str_to_type("i32").unwrap()],
                ret: Type::Int32,
                f: Function::new_native_with_env(
                    &store,
                    env.clone(),
                    generate_translation_with_sizes!(fn add(i32, i32) -> i32; (1, 1) -> 1),
                ),
            });
        }

        i
    };

    let interface = {
        //Uses the workspace's `Cargo.toml`
        let mut f = File::open(r"./em/src/test.api")?;
        let mut s = String::new();
        f.read_to_string(&mut s)?;
        compile_api(&s, interface)?
    };

    // println!("Compiled interface:\n{:#?}", interface);

    let raw = include_str!("test.em");
    let runtime = compile_text(
        raw,
        RuntimeCfg {
            print_cast: false,
            verbose_compile: false,
            opt_level: Debug,
        },
        interface,
        store,
    )?;

    //Now, call runtime methods.
    //For methods like frame update calls, the `NativeFunc<..>` can be cached
    {
        let f_hello: NativeFunc<(), i32> = runtime.exports.get_native_function("hello")?;
        let f_hello_ret = f_hello.call()?;

        println!("\nReturned from `hello()` call: {f_hello_ret}");
    }
    {
        let f_fib: NativeFunc<i32, i32> = runtime.exports.get_native_function("fibonacci")?;
        let f_fib = f_fib.call(15)?;

        println!("\nReturned from `fibonacci(15)` call: {f_fib}");
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
