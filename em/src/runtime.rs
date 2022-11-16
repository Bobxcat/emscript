use std::{
    collections::HashMap,
    fmt::Display,
    fs::{canonicalize, DirBuilder, File},
    io::{Read, Write},
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::bail;
use wasmer::{
    imports, Engine, Exports, Function, ImportObject, Instance, Memory, MemoryType, Module, Store,
    Universal,
};

use crate::{
    ast::{ASTNode, StringContext},
    c_ast::{ast_to_cast, cast_to_string},
    interface::Interface,
    tree::{NodeId, Tree},
    value::Type,
};
/// An error was ecountered during runtime. These should be few and far between, as emscript is designed to avoid a heavy runtime
///
/// In future, can be used to check for things like overflowing arithmetic (for debug builds)
#[derive(Debug, Clone)]
pub enum RuntimeErr {
    UnableToFindASTHead,
    /// An unknown error was encountered during runtime, possibly due to not properly using `verify`
    Unchecked(StringContext),
    /// A variable was referenced which was not declared
    UnknownVarRef(StringContext, String),
    /// A binary operation was used which failed
    BadOp(StringContext),
    /// A generic panic was encountered (user-supplied)
    Panic(StringContext, String),
    ///
    DependencyNotInstalled(Dependency),
}

impl std::error::Error for RuntimeErr {}

#[derive(Debug, Clone, Copy)]
pub enum Dependency {
    Clang,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum OptLevel {
    NoOpt,
    Debug,
    Release,
}

#[derive(Debug, Clone)]
pub struct RuntimeCfg {
    /// If `true`, the runtime will print a copy of the `C` AST generated by the compiler
    pub print_cast: bool,
    pub verbose_compile: bool,
    pub opt_level: OptLevel,
}

impl Display for RuntimeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            RuntimeErr::UnableToFindASTHead => format!("Unable to find AST head"),
            RuntimeErr::Unchecked(ctx) => format!("Unchecked Runtime Err:\n{ctx}"),
            RuntimeErr::UnknownVarRef(ctx, name) => {
                format!("Unknown Variable Referenced: `{name}`\n{ctx}")
            }
            RuntimeErr::BadOp(ctx) => format!("Bad Operation Performed:\n{ctx}"),
            RuntimeErr::Panic(ctx, text) => format!("Panic Encountered:\n`{text}`\n{ctx}"),
            //Non-installed dependencies
            RuntimeErr::DependencyNotInstalled(Dependency::Clang) => {
                format!("`clang` not installed")
            }
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MethodInfo {
    /// The node which contains the `MethodDef` definining the method
    pub declaration: NodeId,
    /// The node which contains the actual code of the method, the child of `declaration`
    pub body: NodeId,
    /// All the inputs to a method
    pub inputs: Vec<(Type, String)>,
    ///
    pub return_type: Type,
}

impl MethodInfo {
    pub fn new(
        declaration: NodeId,
        ast: &Tree<ASTNode>,
        inputs: Vec<(Type, String)>,
        return_type: Type,
    ) -> Self {
        MethodInfo {
            declaration,
            body: ast[declaration].children[0],
            inputs,
            return_type,
        }
    }
}

/// Defines a runtime used to interpret from an AST
#[derive(Debug, Clone)]
pub struct Runtime {
    pub cfg: RuntimeCfg,
    interface: Interface,
    /// Each method declaration stored as a pair of (method_name, (method_declaration, method_body))
    // pub(crate) method_declarations: HashMap<String, MethodInfo>, //TODO: Define external methods, Rc<state>?
    target_path: PathBuf,
    c_dir: PathBuf,
    wasm_dir: PathBuf,
}

impl Runtime {
    fn new(cfg: RuntimeCfg, interface: Interface) -> Self {
        Self {
            cfg,
            interface,
            // method_declarations: HashMap::new(),
            target_path: "".into(),
            c_dir: "".into(),
            wasm_dir: "".into(),
        }
    }
    pub fn new_init(
        ast: &Tree<ASTNode>,
        cfg: RuntimeCfg,
        interface: Interface,
    ) -> Result<Self, RuntimeErr> {
        let mut r = Self::new(cfg, interface);
        // r.init(ast)?;
        Ok(r)
    }
    /// Same as [setup_target_dir], except that `target_dir` is a relative path
    ///
    /// If `target_dir` does not exist, it will be created by this method
    pub fn setup_target_dir_relative<P>(&mut self, target_dir: P) -> Result<(), std::io::Error>
    where
        P: AsRef<Path>,
    {
        let target_dir: &Path = target_dir.as_ref();
        //Create `target_dir` if needed
        {
            let mut dir_builder = DirBuilder::new();
            dir_builder.recursive(true);
            dir_builder.create(target_dir)?;
        }

        let target_dir = &std::fs::canonicalize(target_dir).unwrap();
        self.setup_target_dir(target_dir)
    }
    /// Sets up a target directory, modifying the settings of `self` to reflect the changes.
    ///
    /// `target_dir` should be an absolute path, likely using [std::fs::canonicalize].
    /// If a relative path would be preferred, use [setup_target_dir_relative] instead
    pub fn setup_target_dir<P>(&mut self, target_dir: P) -> Result<(), std::io::Error>
    where
        P: AsRef<Path>,
    {
        let target_dir: &Path = target_dir.as_ref();
        self.target_path = target_dir.to_path_buf();
        self.c_dir = self.target_path.join("c/");
        self.wasm_dir = self.target_path.join("wasm/");

        let mut dir_builder = DirBuilder::new();
        dir_builder.recursive(true);
        dir_builder.create(target_dir)?;

        macro_rules! create_dir {
            ($path:expr) => {
                dir_builder.create($path)?
            };
        }
        //C
        {
            //The dir_builder is recursive, but still do each directory individually to make sure
            create_dir!(&self.c_dir)
        }
        //Wasm
        {
            create_dir!(&self.wasm_dir)
        }
        if self.cfg.verbose_compile {
            println!(
                "Setup target directories:\nTarget dir: `{}`\nC dir: `{}`\nWASM dir: `{}`\n\n",
                self.target_path.display(),
                self.c_dir.display(),
                self.wasm_dir.display()
            );
        }

        Ok(())
    }

    /// Compiles the given `ast` into a c project. The root of that project is returned (which contains a `main.c`)
    ///
    /// Note that the `ast` will likely be mutated
    pub fn compile_to_c(&self, ast: &mut Tree<ASTNode>) -> Result<PathBuf, anyhow::Error> {
        /// A macro which lets you create files more easily (from the root of the generated proj)
        macro_rules! create_file_root {
            ($path:expr) => {
                File::create(self.c_dir.join($path))?
            };
            ($path:expr, $text:expr) => {{
                let mut f = File::create(self.c_dir.join($path))?;
                f.write_all($text)?;
                f
            }};
        }

        //main.c
        {
            let main_cast = ast_to_cast(ast, &self.interface)?;

            if self.cfg.print_cast {
                println!("==CAST==\n{}\n========", main_cast);
            }

            let main_txt = format!("{}", cast_to_string(&main_cast));
            create_file_root!("main.c", main_txt.as_bytes());
        }

        Ok(self.c_dir.to_path_buf())
    }

    pub fn compile_c(&self) -> Result<PathBuf, anyhow::Error> {
        //Call 'clang'
        {
            let opt_flag = match self.cfg.opt_level {
                OptLevel::NoOpt => "-O0",
                OptLevel::Debug => "-O1",
                OptLevel::Release => "-O3",
            };
            if self.cfg.verbose_compile {
                println!("Running `clang main.c --target=wasm32 -nostdlib -Wl,--no-entry -Wl,--export-all -o main.wasm {opt_flag}`");
            }
            let mut clang = Command::new("clang");
            let absolute_c_target_path = canonicalize(&PathBuf::from("../").join(&self.c_dir))?;
            if self.cfg.verbose_compile {
                println!(
                    "Setting `clang` running directory to: {}\n",
                    absolute_c_target_path.display()
                );
            }
            clang.current_dir(absolute_c_target_path);
            clang.args([
                "main.c",
                "--target=wasm32",
                "-nostdlib",
                "-Wl,--no-entry",
                "-Wl,--export-all", //As of now, all methods are exported
                "-o",
                "main.wasm",
                opt_flag,
            ]);
            let mut clang_handle = match clang.spawn() {
                Ok(handle) => handle,
                Err(e) => match e.kind() {
                    std::io::ErrorKind::NotFound => {
                        bail!(RuntimeErr::DependencyNotInstalled(Dependency::Clang))
                    }
                    _ => bail!(e),
                },
            };
            clang_handle.wait()?;
        }

        //Clone the generated `c/main.wasm` file into `wasm/main.wasm`
        {
            let generated_wasm_path = self.c_dir.join(format!("main.wasm"));
            let wasm_new_path = self.wasm_dir.join(format!("main.wasm"));
            std::fs::copy(&generated_wasm_path, &wasm_new_path)?;

            Ok(wasm_new_path)
        }
    }

    pub fn load_wasm<P>(self, wasm_path: P, store: Store) -> anyhow::Result<Instance>
    where
        P: AsRef<Path>,
    {
        //Define the WASM environment
        let import_obj = {
            let mut import_obj = ImportObject::new();

            //Create the namespace "env" and populate it using `interface`
            let mut nm = Exports::new();
            for (_, imp) in self.interface.wasm_imports {
                nm.insert(imp.method_name, imp.f);
            }

            import_obj.register("env", nm);

            import_obj
        };

        //Load the WASM
        let wasm_bytes = {
            let mut buf = Vec::new();
            File::open(wasm_path)?.read_to_end(&mut buf)?;
            buf
        };

        //Define WASM runtime
        let module = Module::new(&store, &wasm_bytes)?;

        //Finally, create the instance itself
        let instance = Instance::new(&module, &import_obj)?;

        Ok(instance)
    }
}