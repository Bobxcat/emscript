#![feature(int_roundings)]
#![feature(ptr_metadata)]
#![feature(option_get_or_insert_default)]

// use core::slice::SlicePattern;
use std::{
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    ptr::NonNull,
    str::FromStr,
    sync::{Arc, Mutex, MutexGuard},
    time::SystemTime,
};

use em_core::memory::MemoryIndex;

// use em_mem::{EmMem, EmMemHandle};
use interface::InterfaceDef;
use ir::IRAST;
use once_cell::sync::Lazy;
use parse::parse;
use runtime::RuntimeCfg;
use wabt::wat2wasm;
// use runtime::RuntimeCfg;
// use wabt::ReadBinaryOptions;
use wasm::compile_irast;
use wasm_opt::OptimizationOptions;
// use wasm_opt::{Feature, OptimizationOptions};
use wasmer::{
    vm::VMMemoryDefinition, AsStoreMut, AsStoreRef, Extern, Function, FunctionEnv, Instance,
    Memory, MemoryType, Module, Pages, Store, StoreMut, StoreRef, TypedFunction,
};
use wasmer_vm::{InternalStoreHandle, LinearMemory, VMMemory};

use crate::{
    interface::{compile_api, MethodImport, StdImport},
    memory::{WAllocatorDefault, STACK_SIZE},
    token::tokenize,
    value::{
        custom_types::{insert_custom_type, str_to_type},
        CustomType, CustomTypeImpl, Type,
    },
};

extern crate pomelo;
extern crate regex_lexer;
extern crate wasmer;
extern crate wasmer_compiler_cranelift;
// extern crate wasmer_compiler_llvm;
extern crate anyhow;
extern crate bimap;
// extern crate wasmer_engine_universal;

mod ast;
mod c_ast;
/// The implementation of a custom `LinearMemory` for
mod em_mem;
mod interface;
mod ir;
mod memory;
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
/// Deals with compiling an IRAST into corresponding WASM code
mod wasm;
/// An intermediate representation which structurally resembles WASM.
///
/// In `WIR`:
/// - There are two sections of memory: the stack and the heap.
/// Allocations in the stack are released at the end of their scope, while
/// allocations in the heap are released manually (__`TODO`__: use GC)
///
/// - Types and basic ops on types are abstracted, so using `+` to add two custom types
/// which implement `Add` is represented using the `Add` node (just like in `IR`)
mod wir;

/// Compilation steps:
/// - Generate an interface, providing methods for all exported methods (probably using `generate_translation_with_sizes!(..)`)
/// - Compile the API, which checks to make sure that the exports given were complete and correct ///then populates the imports///
/// - Generate an EmScript AST from the input code
/// - Translate EmScript AST to IR AST, keeping track of exported methods (NOTE: the name of a method and its wasm-imported name can be different)

// fn instance(wasm_path: impl AsRef<Path>, interface: &Interface) -> anyhow::Result<Instance> {
//     //Get a store and (for callbacks) an environment
//     let mut store = Store::default();
//     // let env = WasmEnv::new();

//     //Define the WASM environment
//     // let import_obj = {
//     //     let mut import_obj = Imports::new();

//     //     // //Create the namespace "env" and populate it using `interface`
//     //     // let mut nm = Exports::new();

//     //     for (s, imp) in &interface.wasm_imports {
//     //         import_obj.define("env", s, imp.f.clone());
//     //         // nm.insert(s, imp.f.clone());
//     //     }

//     //     // import_obj.define("env", nm);

//     //     import_obj
//     // };
//     let alloc = Arc::new(Mutex::new(WAllocatorDefault::default()));
//     let import_obj = interface.get_imports_obj(alloc, instance, store, functions);

//     //Load the WASM
//     let wasm_bytes = {
//         let mut buf = Vec::new();
//         File::open(wasm_path)?.read_to_end(&mut buf)?;
//         buf
//     };

//     //Define WASM runtime
//     let module = Module::new(&store, &wasm_bytes)?;

//     //Finally, create the instance itself
//     let instance = Instance::new(&mut store, &module, &import_obj)?;

//     Ok(instance)
// }

static WASM_STORE: Lazy<Mutex<Option<Store>>> = Lazy::new(|| Mutex::new(None));

pub struct WasmEnv {
    pub mem: Option<InternalStoreHandle<VMMemory>>,
    /// The memory base is guaranteed to always be valid
    ///
    /// (see [VMMemoryDefinition])
    base: *mut u8,
}

impl WasmEnv {
    pub fn store() -> MutexGuard<'static, Option<Store>> {
        WASM_STORE.lock().unwrap()
    }
    pub fn mem<'a>(&self, store: &'a mut MutexGuard<'static, Option<Store>>) -> &'a mut VMMemory {
        let store: &'a mut Store = store.as_mut().unwrap();

        self.mem.unwrap().get_mut(store.objects_mut())
    }
    pub const fn base(&self) -> *mut u8 {
        self.base
    }
    pub const fn offset_to_ptr<T>(&self, offset: MemoryIndex) -> *mut T {
        unsafe { self.base().add(offset as usize) as *mut T }
    }
    fn init(&mut self, mem: InternalStoreHandle<VMMemory>) {
        self.mem = Some(mem);

        let mut s = Self::store();
        let mut mem_def = self
            .mem
            .unwrap()
            .get(s.as_mut().unwrap().objects_mut())
            .vmmemory();
        self.base = unsafe { mem_def.as_mut().base };
    }
}

unsafe impl Send for WasmEnv {}

/// * `raw` - The raw emscript text to be compiled
/// * `cfg` - The runtime configuration
/// * `interface` - The interface definition which defines the host methods which will be used
/// * `store` - The store for wasm code
/// * `implement_interface_methods` - A callback which is used to populate the method implementations of the given interface
///
/// The general idea of compiliation is to do these transformations:
///
/// `emscript text` -> `Vec<Token>` -> `AST` -> `IRAST` -> `WIR` -> `WASMAST` -> `wasm text` -> `wasm binary`
fn compile(
    raw: &str,
    _cfg: RuntimeCfg,
    mut interface: InterfaceDef,
    store: Store,
    // memory: EmMemHandle,
    implement_interface_methods: impl FnOnce(&mut InterfaceDef, &mut Store, &FunctionEnv<WasmEnv>),
) -> anyhow::Result<Instance> {
    // Set WASM_STORE
    *WASM_STORE.lock().unwrap() = Some(store);

    let toks = tokenize(raw)?;

    let ast = parse(toks);
    if let Err(_e) = ast {
        // println!("\n==Tokens==\n{:#?}\n=========", tokens);
        return Err(anyhow::format_err!("AST parsing error encountered"));
    }
    let ast = ast.unwrap();

    let mut ir_ast = IRAST::from_ast(&ast, &interface)?;
    ir_ast.mangle(vec!["foo"].iter().map(|s| *s).collect());

    let wasm_ast = compile_irast(&ir_ast)?;

    println!("Generated wasm:\n{wasm_ast:?}");

    let wasm_txt = &format!("{wasm_ast}");

    let wasm_nofilter = &PathBuf::from_str("em_target/wasm/main_nofilter.wat")?;
    File::create(&wasm_nofilter)?.write_all(wasm_txt.as_bytes())?;

    // Turn the WASM text into a `.wasm` binary, which is smaller and such
    // (also, [wabt] doesn't verify to the same extent as `wasm-opt`, which makes it easier to create)
    let mut store = WasmEnv::store();

    let wasm = &wat2wasm(wasm_txt)?;

    let wasm_path_preopt = &PathBuf::from_str("em_target/wasm/main_preopt.wasm")?;
    File::create(&wasm_path_preopt)?.write_all(wasm)?;

    println!("=====Running unoptimized=====");
    const ARGS: i32 = 10000;
    // Run unoptimized
    {
        // Get a wasm module from `wasm_path_preopt`
        let module = Module::new(store.as_ref().unwrap().engine(), wasm_txt)?;

        // Define the imports

        // Get the environment needed for all the methods
        let env = {
            FunctionEnv::new(
                store.as_mut().unwrap(),
                WasmEnv {
                    mem: None,
                    base: std::ptr::null_mut(),
                },
            )
        };

        // Populate `interface` with method implementations
        implement_interface_methods(&mut interface, store.as_mut().unwrap(), &env);

        // Generate the default allocator
        let allocator = Arc::new(Mutex::new(WAllocatorDefault::<64>::default()));

        // Use `interface` to populate `imports`

        // But first, drop the current handle to `store` to avoid a perma-lock
        std::mem::drop(store);
        let imports = interface.get_imports_obj(allocator, &env);
        let mut store = WasmEnv::store();

        let instance = Instance::new(store.as_mut().unwrap(), &module, &imports)?;

        // Ensure that the memory can fit the stack
        {
            let m = instance.exports.get_memory("memory")?;
            let delta = Pages(STACK_SIZE.div_ceil(Pages(1).bytes().0 as MemoryIndex));
            m.grow(store.as_mut().unwrap(), delta)?;
        }

        // Finish setting up `WasmEnv`
        {
            let m = instance.exports.get_extern("memory").unwrap();
            let m = m.to_vm_extern();
            let m = match m {
                wasmer::vm::VMExtern::Memory(m) => m,
                _ => panic!("Expected export `memory` to be a memory"),
            };

            env.as_mut(store.as_mut().unwrap()).mem = Some(m);
        }

        println!("Instance created. Now grabbing exported method `foo`");

        // Actually run the program
        let foo: TypedFunction<i32, i32> = instance
            .exports
            .get_typed_function(store.as_ref().unwrap(), "foo")
            .unwrap();

        println!("Export method `foo` found. Now calling `foo`");
        let store = store.as_mut().unwrap();

        let mut arr_1 = [0; ARGS as usize];

        {
            let start = SystemTime::now();

            for i in 0..ARGS {
                // println!("foo({i})");
                let f = foo.call(store, i).unwrap();
                arr_1[i as usize] = f;
            }

            println!(
                "Time taken: {:?}\n",
                SystemTime::now().duration_since(start)
            );
        }

        println!("Calling native method `foo`");

        let mut arr_2 = [0; ARGS as usize];

        {
            let start = SystemTime::now();

            #[allow(arithmetic_overflow)]
            #[inline(never)]
            fn foo(n: i32) -> i32 {
                let mut a = 0i32;
                let mut b = 1i32;

                let mut i = 1i32;

                loop {
                    if i >= n {
                        break;
                    }
                    let t = b;
                    //b = add(a, b);
                    b = a.wrapping_add(b);
                    a = t;

                    i = i + 1i32;
                }

                b
            }

            for i in 0..ARGS {
                let f = foo(i);
                arr_2[i as usize] = f;
                // println!("{i}: {f}");
            }

            println!(
                "Time taken: {:?}\n",
                SystemTime::now().duration_since(start)
            );
        }

        assert_eq!(arr_1, arr_2);
        println!("=====Foo equality assertion passed!=====");

        // Finally, return the instance created after much effort
        // Ok(instance)
    }

    println!("=====Creating and writing optimized binary=====");

    let wasm_path_opt = &PathBuf::from_str("em_target/wasm/main.wasm")?;
    OptimizationOptions::new_opt_level_4().run(wasm_path_preopt, wasm_path_opt)?;

    todo!()
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
    // std::env::set_var("RUST_BACKTRACE", "1");

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
    // std::env::set_var("RUST_BACKTRACE", "1");
    println!(
        "Memory index is: {} bytes ({} bits)",
        std::mem::size_of::<MemoryIndex>(),
        std::mem::size_of::<MemoryIndex>() * 8
    );
    use runtime::OptLevel::*;

    let _alloc = Arc::new(Mutex::new(WAllocatorDefault::<64>::default()));

    // let mem = EmMemHandle::new(EmMem::new());

    let store = Store::default();

    let interface = {
        use StdImport::*;
        let mut i = InterfaceDef::new_with_std(vec![StdOut].into_iter().collect());

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

        //`add`
        {
            i.insert(MethodImport {
                mod_name: "env".into(),
                method_name: "add".into(),
                params: vec![Type::Int32, Type::Int32],
                ret: Type::Int32,
                f: None,
            });
        }

        //`Foo`
        #[cfg(not)]
        if false {
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
                    f: Function::new_typed_with_env(
                        &mut store,
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
                    f: Function::new_typed_with_env(
                        &mut store,
                        &env,
                        generate_translation_with_sizes!(fn foo_fib(&mut Foo); (1)),
                    ),
                });
            }
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

    let runtime = compile(
        raw,
        RuntimeCfg {
            print_cast: false,
            verbose_compile: false,
            opt_level: Debug,
        },
        interface,
        store,
        // mem.clone(),
        // Implement non-core custom methods
        |interface, store, _env| {
            //`add`
            {
                fn add(a: i32, b: i32) -> i32 {
                    a + b
                }
                // Insert *over* the existing import for `add`, this time with an implementation
                interface.insert(MethodImport {
                    mod_name: "env".into(),
                    method_name: "add".into(),
                    params: vec![Type::Int32, str_to_type("i32").unwrap()],
                    ret: Type::Int32,
                    f: Some(Function::new_typed(store, add)),
                });
            }

            // //`get_0th`
            // {
            //     fn get_0th(env: FunctionEnvMut<WasmEnv>) -> i32 {
            //         let mut store = WasmEnv::store();
            //         let store = store.as_mut().unwrap();

            //         let view = env.data().memory.as_ref().unwrap().view(store);
            //         let vals = [
            //             view.read_u8(0).unwrap(),
            //             view.read_u8(1).unwrap(),
            //             view.read_u8(2).unwrap(),
            //             view.read_u8(3).unwrap(),
            //         ];

            //         i32::from_ne_bytes(vals)
            //     }

            //     interface.insert(MethodImport {
            //         mod_name: "env".into(),
            //         method_name: "add".into(),
            //         params: vec![Type::Int32, str_to_type("i32").unwrap()],
            //         ret: Type::Int32,
            //         f: Some(Function::new_typed_with_env(store, env, get_0th)),
            //     });
            // }
            // todo!()
        },
    )?;

    //Now, call runtime methods.
    //For methods like frame update calls, the `NativeFunc<..>` can be cached

    //Get the store which has now been moved
    let mut store = WasmEnv::store();
    let store = store.as_mut().unwrap();

    // {
    //     let f_hello: TypedFunction<(), i32> = runtime.exports.get_typed_function(store, "hello")?;
    //     let f_hello_ret = f_hello.call(store)?;

    //     println!("\nReturned from `hello()` call: {f_hello_ret}");
    // }
    {
        let f_fib: TypedFunction<i32, i32> =
            runtime.exports.get_typed_function(store, "fibonacci")?;
        let f_fib = f_fib.call(store, 15)?;

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
