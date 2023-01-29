use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    sync::Mutex,
};

use once_cell::sync::Lazy;

use crate::{
    ir::{IRNodeType, IdentInfo, IRAST},
    tree::{NodeId, Tree},
    utils::PREFIX_TMP,
    value::{self, Value},
};

use em_core::memory::MemoryIndex;

/// The number of variables generated using `generate_tmp(..)`
static TMP_VAR_COUNT: Lazy<Mutex<usize>> = Lazy::new(|| Mutex::new(0));

fn generate_tmp() -> String {
    let mut count = TMP_VAR_COUNT.lock().unwrap();
    let s = format!("{}{}", PREFIX_TMP, count);
    *count += 1;

    s
}

/// The number of variables generated using `generate_breakpoint_name(..)`
static BREAKPOINT_COUNT: Lazy<Mutex<usize>> = Lazy::new(|| Mutex::new(0));

fn generate_breakpoint_name() -> String {
    let mut count = BREAKPOINT_COUNT.lock().unwrap();
    let s = format!("{}{}", "br_", count);
    *count += 1;

    s
}

fn generate_prev_breakpoint_name() -> String {
    let count = BREAKPOINT_COUNT.lock().unwrap();
    let s = format!("{}{}", "br_", *count - 1);

    s
}

pub struct WasmAST {
    tree: Tree<WasmASTNode>,
    mem_size: usize,
    // custom_types: HashMap<WasmCustomTypeIdent, WasmCustomType>,
}

impl WasmAST {
    pub fn new() -> Self {
        // let custom_types = HashMap::new();

        Self {
            tree: Default::default(),
            mem_size: 1,
        }
    }
}

impl Debug for WasmAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "tree:\n{}\nmem_size: {}", self.tree, self.mem_size)
    }
}

impl Display for WasmAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = wasm_ast_display_recurse(&self, self.tree.find_head().unwrap());
        // format_wat(&mut s);
        write!(f, "{}", s)
    }
}

fn wasm_ast_display_recurse(ast: &WasmAST, curr: NodeId) -> String {
    use WasmASTNode::*;

    macro_rules! child {
        ($idx:expr) => {{
            wasm_ast_display_recurse(ast, ast.tree[curr].children[$idx])
        }};
    }

    macro_rules! children {
        () => {{
            let mut s = Vec::new();
            for i in 0..ast.tree[curr].children.len() {
                s.push(format!("{}", child!(i)))
            }
            s.join(" ")
        }};
    }

    match &ast.tree[curr].data {
        Module => format!("(module\n{})", children!()),
        Memory => format!("(memory $memory {})\n", ast.mem_size),

        Import { env, imp_name, t } => format!("(import \"{env}\" \"{imp_name}\" {t})\n"),
        Export(t, exp_name) => format!("(export \"{exp_name}\" {t})\n"),

        FuncDef {
            name,
            name_export,
            params,
            ret,
            used_variables,
        } => {
            let res_str = format!(
                "{}",
                ret.iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            let res_str = if res_str.is_empty() {
                res_str
            } else {
                format!("(result {res_str})")
            };

            let params_str = format!(
                "{}",
                params
                    .iter()
                    .map(|(name, t)| format!("(param ${name} {t})"))
                    .collect::<Vec<_>>()
                    .join(" ")
            );

            let dec_str = format!(
                "{}",
                used_variables
                    .iter()
                    .map(|(name, t)| format!("(local ${name} {t})"))
                    .collect::<Vec<_>>()
                    .join(" ")
            );

            format!(
                "(func ${name} (export \"{name_export}\") {params_str} {res_str}\n{dec_str}\n{})\n",
                children!()
            )
        }
        FuncCall(s) => format!("(call ${s})"),

        If => match ast.tree[curr].children.len() {
            2 => format!("(if\n{}\n{})", child!(0), child!(1)),
            _ => format!("(if\n{})", children!()),
        },

        Then => format!("(then {})", children!()),
        Else => format!("(else {})", children!()),

        Loop(n) => format!("(loop ${n} {})", children!()),
        Block(n) => format!("(block ${n} {})", children!()),
        End => format!("(end)"),
        Br(n) => format!("(br ${n})"),

        MemAlloc(_size) => format!("()"),
        // Get(t, s) => format!("({t}.load (global.get ${s}))\n"),
        // Set(t, s) => format!("({t}.store (global.get ${s}) {})\n", children!()),
        Load(t) => format!("({t}.load {})", children!()),
        Store(t) => format!("({t}.store {})", children!()),

        // Load(t, s) => format!(),
        // Store(t, s) => format!("()"),
        Global(s, t) => format!("(global ${s} (mut {t}) {})\n", children!()),
        Local(s, t) => format!("(local ${s} {t} {})\n", children!()),

        // GlobalGet(s) => format!("(global.get ${s}"),
        LocalGet(s) => format!("(local.get ${s})\n"),
        LocalSet(s) => format!("(local.set ${s} {})\n", children!()),

        // Local(s, t) => format!("(local ${s} {t})"),
        Const(val) => format!("({}.const {val})", val.t()),

        Add(t) => format!("({t}.add \n{})", children!()),
        Sub(t) => format!("({t}.sub \n{})", children!()),
        Mul(t) => format!("({t}.mul \n{})", children!()),
        Div(t) => format!("({t}.div \n{})", children!()),

        // For now (until better type system), all operations are signed
        Eq(t) => format!("({t}.eq \n{})", children!()),
        Ne(t) => format!("({t}.ne \n{})", children!()),
        Lt(t) => format!("({t}.lt_s \n{})", children!()),
        Gt(t) => format!("({t}.gt_s \n{})", children!()),
        Le(t) => format!("({t}.le_s \n{})", children!()),
        Ge(t) => format!("({t}.ge_s \n{})", children!()),

        Empty => format!("({})", children!()),
        //
        // Empty => {
        //     if ast.tree[curr].children.len() > 1 {
        //         format!("({})", children!())
        //     } else {
        //         format!("{}", children!())
        //     }
        // }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum WasmType {
    Builtin(WasmBuiltinType),
    None,
    // Custom(WasmCustomTypeIdent),
}

impl WasmType {
    fn from_type(t: &value::Type) -> Self {
        use WasmBuiltinType::*;
        match t {
            value::Type::Bool => WasmType::Builtin(I32),
            value::Type::Int => WasmType::Builtin(I32),
            value::Type::Int32 => WasmType::Builtin(I32),
            value::Type::Int64 => WasmType::Builtin(I64),
            value::Type::Void => WasmType::None,
            _ => todo!("{t}"),
        }
    }
    fn zero_value(self) -> WasmValue {
        use WasmBuiltinType::*;
        match self {
            WasmType::Builtin(I32) => WasmValue::I32(0),
            WasmType::Builtin(I64) => WasmValue::I64(0),
            WasmType::Builtin(F32) => WasmValue::F32(0.),
            WasmType::Builtin(F64) => WasmValue::F64(0.),
            _ => unimplemented!(),
        }
    }
    fn size(self) -> usize {
        use WasmBuiltinType::*;
        use WasmType::*;

        match self {
            Builtin(I32) | Builtin(F32) => 4,
            Builtin(I64) | Builtin(F64) => 8,
            None => 0,
        }
    }
}

impl Display for WasmType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                WasmType::Builtin(t) => format!("{t}"),
                WasmType::None => unimplemented!(),
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum WasmBuiltinType {
    I32,
    I64,
    F32,
    F64,
}

impl Display for WasmBuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use WasmBuiltinType::*;

        write!(
            f,
            "{}",
            match self {
                I32 => "i32",
                I64 => "i64",
                F32 => "f32",
                F64 => "f64",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum WasmValue {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl WasmValue {
    fn t(self) -> WasmType {
        use WasmBuiltinType::*;
        match self {
            WasmValue::I32(_) => WasmType::Builtin(I32),
            WasmValue::I64(_) => WasmType::Builtin(I64),
            WasmValue::F32(_) => WasmType::Builtin(F32),
            WasmValue::F64(_) => WasmType::Builtin(F64),
        }
    }
}

impl Display for WasmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                WasmValue::I32(n) => format!("{n}"),
                WasmValue::I64(n) => format!("{n}"),
                WasmValue::F32(n) => format!("{n}"),
                WasmValue::F64(n) => format!("{n}"),
            }
        )
    }
}

#[derive(Debug, Clone)]
enum ImportObjType {
    Func {
        wasm_name: String,
        parameters: Vec<WasmType>,
        ret: WasmType,
    },
}

impl Display for ImportObjType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ImportObjType::Func {
                    wasm_name,
                    parameters,
                    ret,
                } => {
                    let params_str = parameters
                        .iter()
                        .map(|t| format!("(param {t})"))
                        .collect::<Vec<_>>()
                        .join(" ");
                    let ret_str = match ret {
                        WasmType::None => "".into(),
                        t => format!("(result {t})"),
                    };
                    format!("(func ${wasm_name} {params_str} {ret_str})")
                }
            }
        )
    }
}
#[derive(Debug, Clone)]
enum ExportObjType {
    Func(String),
    Memory(String),
}

impl Display for ExportObjType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ExportObjType::Func(s) => format!("(func ${})", s),
                ExportObjType::Memory(s) => format!("(memory ${})", s),
            }
        )
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
enum WasmASTNode {
    Module,
    Memory,

    Import {
        env: String,
        imp_name: String,
        t: ImportObjType,
    },
    /// The reference to the exported object and the name which it presents to the environment
    Export(ExportObjType, String),

    FuncDef {
        name: String,
        name_export: String,
        params: Vec<(String, WasmType)>,
        ret: Vec<WasmType>,
        /// The list of variables used by this method, excluding parameters.
        used_variables: HashSet<(String, WasmType)>,
    },
    FuncCall(String),

    /// `(if {child_0} {child_1})`
    If,

    /// `(then ...)`
    Then,
    /// `(else ...)`
    Else,

    /// `(loop ${name} ...)`
    Loop(String),
    /// `(block ${name} ...)`
    Block(String),
    /// `(end)`
    End,
    /// `(br ${name})`
    Br(String),

    /// Shorthand for calling the memory allocation method with the given memory size.
    ///
    /// Based on the given size of memory, this returns the
    MemAlloc(MemoryIndex),

    // /// Shorthand for getting a variable with the given name and type
    // ///
    // /// `({t}.load (global.get ${name}))`
    // Get(WasmType, String),
    // /// Shorthand for setting a variable with the given name and type
    // ///
    // /// `({t}.store (global.get ${name}) ({child}))`
    // Set(WasmType, String),
    Load(WasmType),
    Store(WasmType),

    // Load(WasmType, String),
    // /// 1 child (memory location)
    // Store(WasmType),
    Global(String, WasmType),
    Local(String, WasmType),
    // GlobalGet(String),
    /// `(local.get ${name})`
    LocalGet(String),
    /// `(local.set ${name})`
    LocalSet(String),
    // /// `(local ${name} {t})`
    // Local(String, WasmType),
    Const(WasmValue),

    Eq(WasmType),
    Ne(WasmType),

    Lt(WasmType),
    Gt(WasmType),
    Le(WasmType),
    Ge(WasmType),

    Add(WasmType),
    Sub(WasmType),
    Mul(WasmType),
    Div(WasmType),

    /// `({child_0} {child_1} ... {child_n})`
    ///
    /// Useful to ensure a list of commands is treated as one
    Empty,
}

impl Display for WasmASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub fn compile_irast(irast: &IRAST) -> anyhow::Result<WasmAST> {
    let mut ast = WasmAST::new();
    let ast_head = ast.tree.new_node(WasmASTNode::Module);

    //Add all imported methods
    for (_, info) in &irast.idents {
        match info {
            IdentInfo::ExternMethod {
                mod_name,
                imp_name,
                name,
                params,
                return_type,
            } => {
                let n = ast.tree.new_node(WasmASTNode::Import {
                    env: mod_name.clone(),
                    imp_name: imp_name.clone(),

                    t: ImportObjType::Func {
                        wasm_name: name.clone(),
                        parameters: params.iter().map(|(_, t)| WasmType::from_type(t)).collect(),
                        ret: WasmType::from_type(&return_type),
                    },
                });
                ast.tree.append_to(ast_head, n)?;
            }
            _ => (),
        }
    }

    //Add the memory and export it
    {
        let mem = ast.tree.new_node(WasmASTNode::Memory);
        ast.tree.append_to(ast_head, mem)?;

        let exp = ast.tree.new_node(WasmASTNode::Export(
            ExportObjType::Memory("memory".into()),
            "memory".into(),
        ));
        ast.tree.append_to(ast_head, exp)?;
    }

    // vvvNow done during string flattening
    // //For each method, figure out which local variables need declaration and add them to the start of the method
    // {
    //     //
    //     for c in ast.tree[ast_head].children.clone() {
    //         let vars = &mut HashSet::new();
    //         match &ast.tree[c].data {
    //             WasmASTNode::FuncDef { .. } => {
    //                 //
    //                 get_method_variables(&mut ast, c, vars);
    //             }
    //             _ => (),
    //         }

    //         //For each var, insert it
    //         for v in vars.iter() {
    //             //
    //             let n = ast.tree.new_node(WasmASTNode::Local((), ()));
    //         }
    //     }
    // }

    compile_irast_recurse(
        irast,
        irast.tree.find_head().unwrap(),
        &mut ast,
        &mut HashSet::new(),
        ast_head,
    )?;

    // println!("\n\n{ast:#?}\n\n");

    Ok(ast)
}

// /// Given a node representing a method declaration, returns a map of all the local variables which are used in the method body
// fn get_method_variables(wasm: &WasmAST, curr: NodeId, vars: &mut HashSet<String>) {
//     match &wasm.tree[curr].data {
//         WasmASTNode::LocalSet(name) => {
//             vars.insert(name.clone());
//         }
//         _ => (),
//     }

//     for c in wasm.tree[curr].children.iter().cloned() {
//         get_method_variables(wasm, c, vars);
//     }
// }

/// * `irast` - The IRAST which is being compiled from
/// * `curr` - The current node in the IRAST
/// * `wasm` - The tree representing the generated compiled wasm code
/// * `used_vars` - The set of variables used within this function
/// * `parent_wasm` - The id of the node which called this method recursively
///
/// Each subtree is expected to leave its return value on the top of the stack
fn compile_irast_recurse(
    irast: &IRAST,
    curr: NodeId,
    wasm: &mut WasmAST,
    used_vars: &mut HashSet<(String, WasmType)>,
    parent_wasm: NodeId,
) -> anyhow::Result<()> {
    let children = irast.tree[curr].children.clone();

    match irast.tree[curr].data.t.clone() {
        IRNodeType::Literal(v) => {
            let n = wasm.tree.new_node(WasmASTNode::Const(match v {
                Value::Bool(n) => WasmValue::I32(n as u8 as i32),
                Value::Int(n) => WasmValue::I32(n as i32),
                Value::Int32(n) => WasmValue::I32(n as i32),
                _ => todo!(),
            }));
            wasm.tree.append_to(parent_wasm, n)?;
        }
        IRNodeType::VarRef(id) => {
            let n = wasm.tree.new_node(WasmASTNode::LocalGet(
                // WasmType::from_type(&irast[id].return_type()),
                format!("{}", irast[id].name()),
            ));
            wasm.tree.append_to(parent_wasm, n)?;
        }
        IRNodeType::FieldRef(_) => todo!(),

        //Assignments:(
        //  (i32.store (global.get ${var_name})
        //      ({child_0})
        //  )
        //)
        IRNodeType::VarDef(id) => {
            let var_name = irast[id].name();
            //Declaration is handled seperately, since it must be done at the start of the method

            //Insert this variable as one which is used in the method
            {
                let t = WasmType::from_type(&irast[id].return_type());
                used_vars.insert((var_name.clone(), t));
            }

            // //Declare the variable
            // let dec = wasm.tree.new_node(WasmASTNode::Local(
            //     format!("{var_name}"),
            //     WasmType::from_type(&irast[id].return_type()),
            // ));
            // wasm.tree.append_to(parent_wasm, dec)?;

            //Do the assignment part
            let parent = wasm
                .tree
                .new_node(WasmASTNode::LocalSet(format!("{var_name}")));

            wasm.tree.append_to(parent_wasm, parent)?;

            compile_irast_recurse(irast, children[0], wasm, used_vars, parent)?;
        }
        IRNodeType::Assign(id) => {
            let var_name = irast[id].name();
            let parent = wasm
                .tree
                .new_node(WasmASTNode::LocalSet(format!("{var_name}")));
            wasm.tree.append_to(parent_wasm, parent)?;

            compile_irast_recurse(irast, children[0], wasm, used_vars, parent)?;
        }

        IRNodeType::MethodDef(id) => {
            let method_name = irast[id].name();

            let params: Vec<_> = {
                use IdentInfo::*;
                match &irast[id] {
                    Method { params, .. } => params
                        .iter()
                        .map(|id| {
                            let (ret, name) = irast[*id].name_and_return_type();
                            (format!("{}", name), WasmType::from_type(&ret))
                        })
                        .collect(),
                    ExternMethod { params, .. } => params
                        .iter()
                        .map(|(name, ret)| (format!("{}", name), WasmType::from_type(&ret)))
                        .collect(),
                    _ => unreachable!(),
                }
            };

            //Create standin for the real function def
            let func = WasmASTNode::Empty;

            let parent = wasm.tree.new_node(func);
            wasm.tree.append_to(parent_wasm, parent)?;

            let mut used_vars = HashSet::new();

            compile_irast_recurse(irast, children[0], wasm, &mut used_vars, parent)?;

            let func = WasmASTNode::FuncDef {
                name: format!("{method_name}"),
                name_export: format!(
                    "{}",
                    match &irast[id] {
                        IdentInfo::Method { name, .. } => name,
                        IdentInfo::ExternMethod { imp_name, .. } => imp_name,
                        _ => unreachable!(),
                    }
                ),
                used_variables: used_vars,
                params: params.clone(),
                ret: vec![WasmType::from_type(&irast[id].return_type())],
            };

            wasm.tree[parent].data = func;
        }
        IRNodeType::MethodCall(id) => {
            //Get each param in order, then call
            for c in children {
                compile_irast_recurse(irast, c, wasm, used_vars, parent_wasm)?;
            }

            //Call
            let call = wasm
                .tree
                .new_node(WasmASTNode::FuncCall(irast[id].name().clone()));
            wasm.tree.append_to(parent_wasm, call)?;
        }
        // A reference is just:
        // `(t.load
        //      {child}
        //  )`
        //
        //
        // These both treat this reference as a location in memory, but in effect they are both:
        // `(t.load )`
        IRNodeType::Reference => {
            todo!()
        }
        IRNodeType::IfCondition => {
            let t = WasmType::from_type(&irast[children[1]].data.return_type);

            //Return value temporary var (only if there is a return value)
            let ret = if t != WasmType::None {
                let s = generate_tmp();
                let id = wasm.tree.new_node(WasmASTNode::Local(s.clone(), t));
                wasm.tree.append_to(parent_wasm, id)?;
                Some(s)
            } else {
                None
            };

            // First, generate the conditional (note that bools are i32s in wasm)
            {
                compile_irast_recurse(irast, children[0], wasm, used_vars, parent_wasm)?;
            }

            let parent = wasm.tree.new_node(WasmASTNode::If);
            wasm.tree.append_to(parent_wasm, parent)?;

            let then = wasm.tree.new_node(WasmASTNode::Then);
            wasm.tree.append_to(parent, then)?;

            compile_irast_recurse(irast, children[1], wasm, used_vars, then)?;

            //Set the return var
            if let Some(ret) = ret.clone() {
                let set = wasm.tree.new_node(WasmASTNode::LocalSet(ret));
                wasm.tree.append_to(then, set)?;
            }

            //Compile a right hand side *iff* there is an else statement
            if children.len() > 2 {
                let el = wasm.tree.new_node(WasmASTNode::Else);
                compile_irast_recurse(irast, children[2], wasm, used_vars, el)?;

                //Set the return var
                if let Some(ret) = ret.clone() {
                    let set = wasm.tree.new_node(WasmASTNode::LocalSet(ret));
                    wasm.tree.append_to(el, set)?;
                }
            }

            //If there's a return type, push its value to the stack after exiting the if statement block
            if let Some(ret) = ret {
                let ret = wasm.tree.new_node(WasmASTNode::LocalGet(ret));
                wasm.tree.append_to(parent_wasm, ret)?;
            }
        }
        // `(block $b
        //     (loop $a
        //        (..)
        //        (br $a)
        //     )
        // )`
        //
        // such that a call to `generate_prev_breakpoint_name` returns `$b`
        IRNodeType::Loop => {
            let loop_name = generate_breakpoint_name();
            let block_name = generate_breakpoint_name();

            let bl = wasm.tree.new_node(WasmASTNode::Block(block_name));
            let lo = wasm.tree.new_node(WasmASTNode::Loop(loop_name.clone()));
            wasm.tree.append_to(bl, lo)?;
            wasm.tree.append_to(parent_wasm, bl)?;

            compile_irast_recurse(irast, children[0], wasm, used_vars, lo)?;

            //For the loop, always jump back to the start (this is what changes for a while loop)
            let jump_back = wasm.tree.new_node(WasmASTNode::Br(loop_name));
            wasm.tree.append_to(lo, jump_back)?;
        }
        IRNodeType::Break => {
            let parent = wasm
                .tree
                .new_node(WasmASTNode::Br(generate_prev_breakpoint_name()));
            wasm.tree.append_to(parent_wasm, parent)?;
        }
        IRNodeType::LastValueReturn => {
            //Execute each expression -- all are guaranteed to return the correct types
            // (either nothing for all but last or some value for the last), so no checking is necessary

            for c in children {
                // println!("{c:?}");
                compile_irast_recurse(irast, c, wasm, used_vars, parent_wasm)?;
            }
        }
        IRNodeType::ValueConsume => {
            compile_irast_recurse(irast, children[0], wasm, used_vars, parent_wasm)?;
        }
        //Binary ops:
        //({t}.{op}
        //  ({child_0})
        //  ({child_1})
        //)
        IRNodeType::Eq
        | IRNodeType::Ne
        | IRNodeType::Lt
        | IRNodeType::Gt
        | IRNodeType::Le
        | IRNodeType::Ge
        | IRNodeType::Add
        | IRNodeType::Sub
        | IRNodeType::Mul
        | IRNodeType::Div => {
            let parent = wasm.tree.new_node(WasmASTNode::Module);
            wasm.tree.append_to(parent_wasm, parent)?;

            //Have lhs and rhs append themselves to `parent`
            compile_irast_recurse(irast, children[0], wasm, used_vars, parent)?;
            compile_irast_recurse(irast, children[1], wasm, used_vars, parent)?;

            //The type of the wasm operation
            let t = WasmType::from_type(&irast.tree[children[0]].data.return_type.clone());

            wasm.tree[parent].data = match irast.tree[curr].data.t {
                IRNodeType::Eq => WasmASTNode::Eq(t),
                IRNodeType::Ne => WasmASTNode::Ne(t),
                IRNodeType::Lt => WasmASTNode::Lt(t),
                IRNodeType::Gt => WasmASTNode::Gt(t),
                IRNodeType::Le => WasmASTNode::Le(t),
                IRNodeType::Ge => WasmASTNode::Ge(t),
                //
                IRNodeType::Add => WasmASTNode::Add(t),
                IRNodeType::Sub => WasmASTNode::Sub(t),
                IRNodeType::Mul => WasmASTNode::Mul(t),
                IRNodeType::Div => WasmASTNode::Div(t),
                _ => unreachable!(),
            };
        }
    }

    Ok(())
}
