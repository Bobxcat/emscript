use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    sync::Mutex,
};

use once_cell::sync::Lazy;

use crate::{
    ir::{IRNodeType, IdentInfo, IRAST},
    tree::{NodeId, Tree},
    utils::{MEM_ALLOC_NAME, PREFIX_TMP, STACK_ALLOC_NAME, STACK_POP_MULTIPLE_NAME},
    value::{self, Value},
    wir::{self, WIRNode, WIRAST},
};

use em_core::memory::MemoryIndex;

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
        let mut s = wasm_ast_display_recurse(&self, self.tree.find_head().unwrap());
        format_wat(&mut s);
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
        Module => format!("(module{})", children!()),
        Memory => format!("(memory $memory {})", ast.mem_size),

        Import { env, imp_name, t } => format!("(import \"{env}\" \"{imp_name}\" {t})"),
        Export(t, exp_name) => format!("(export \"{exp_name}\" {t})"),

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
                "(func ${name} (export \"{name_export}\") {params_str} {res_str} {dec_str} {})",
                children!()
            )
        }
        FuncCall(s) => format!("(call ${s})"),

        If => match ast.tree[curr].children.len() {
            2 => format!("(if {} {})", child!(0), child!(1)),
            _ => format!("(if {})", children!()),
        },

        Then => format!("(then {})", children!()),
        Else => format!("(else {})", children!()),

        Loop(n) => format!("(loop ${n} {})", children!()),
        Block(n) => format!("(block ${n} {})", children!()),
        End => format!("(end)"),
        Br(n) => format!("(br ${n})"),

        MemAlloc(size, align) => format!(
            "(call ${} ({}.const {size}) ({}.const {align}))",
            *MEM_ALLOC_NAME,
            WasmType::ptr_type(),
            WasmType::ptr_type(),
        ),

        StackAlloc(size, align) => format!(
            "(call ${} ({}.const {size}) ({}.const {align}))",
            *STACK_ALLOC_NAME,
            WasmType::ptr_type(),
            WasmType::ptr_type(),
        ),

        StackPop(n) => format!(
            "(call ${} ({}.const {n}))",
            *STACK_POP_MULTIPLE_NAME,
            WasmType::ptr_type(),
        ),

        // Get(t, s) => format!("({t}.load (global.get ${s}))"),
        // Set(t, s) => format!("({t}.store (global.get ${s}) {})", children!()),
        Load(t) => format!("({t}.load {})", children!()),
        Store(t) => format!("({t}.store {})", children!()),

        // Load(t, s) => format!(),
        // Store(t, s) => format!("()"),
        Global(s, t) => format!("(global ${s} (mut {t}) {})", children!()),
        Local(s, t) => format!("(local ${s} {t} {})", children!()),

        // GlobalGet(s) => format!("(global.get ${s}"),
        LocalGet(s) => format!("(local.get ${s})"),
        LocalSet(s) => format!("(local.set ${s})"),

        // Local(s, t) => format!("(local ${s} {t})"),
        Const(val) => format!("({}.const {val})", val.t()),

        Add(t) => format!("({t}.add {})", children!()),
        Sub(t) => format!("({t}.sub {})", children!()),
        Mul(t) => format!("({t}.mul {})", children!()),
        Div(t) => format!("({t}.div {})", children!()),

        // For now (until better type system), all operations are signed
        Eq(t) => format!("({t}.eq {})", children!()),
        Ne(t) => format!("({t}.ne {})", children!()),
        Lt(t) => format!("({t}.lt_s {})", children!()),
        Gt(t) => format!("({t}.gt_s {})", children!()),
        Le(t) => format!("({t}.le_s {})", children!()),
        Ge(t) => format!("({t}.ge_s {})", children!()),

        Empty => format!("{}", children!()),
    }
}

fn format_wat(s: &mut String) {
    const INDENT: usize = 2;
    let mut curr_indent = 0;

    // Indices where a newline should be inserted followed by some number of indents.
    // Comes in pairs of `(index, num_indents)`
    let mut newline_insertions = Vec::new();

    for (i, c) in s.char_indices() {
        match c {
            '(' => {
                newline_insertions.push((i, curr_indent));
                curr_indent += 1;
            }
            ')' => {
                // newline_insertions.push((i + 1, curr_indent));
                curr_indent -= 1;
            }
            _ => (),
        }
    }

    // Pop off of `newline_insertions`
    while let Some((idx, indent)) = newline_insertions.pop() {
        let sl = format!("\n{}", " ".repeat(indent * INDENT));
        if idx >= s.len() {
            continue;
        }
        s.insert_str(idx, &sl);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum WasmType {
    Builtin(WasmBuiltinType),
    None,
    // Custom(WasmCustomTypeIdent),
}

impl WasmType {
    fn ptr_type() -> Self {
        Self::from_type(&value::Type::ptr_type())
    }
    fn from_type(t: &value::Type) -> Self {
        use WasmBuiltinType::*;
        match t {
            value::Type::Bool => WasmType::Builtin(I32),
            value::Type::Int => WasmType::Builtin(I32),
            value::Type::Int32 => WasmType::Builtin(I32),
            value::Type::Int64 => WasmType::Builtin(I64),
            value::Type::Void => WasmType::None,

            value::Type::Ref(_) => WasmType::Builtin(I32),
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
    /// Gets the size (in bytes) of the given type
    fn size(self) -> MemoryIndex {
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
    fn from_val(v: Value) -> Self {
        match v {
            Value::Bool(n) => Self::I32(n as u8 as i32),
            Value::Int(n) => Self::I32(n as i32),
            Value::Int32(n) => Self::I32(n as i32),
            _ => todo!(),
        }
    }
    fn ptr_val(n: MemoryIndex) -> Self {
        #[cfg(not(feature = "mem_64bit"))]
        {
            Self::I32(unsafe { std::mem::transmute(n) })
        }
        #[cfg(feature = "mem_64bit")]
        {
            Self::I64(unsafe { std::mem::transmute(n) })
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
    Memory(String),
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
                ImportObjType::Memory(wasm_name) => format!("(memory ${wasm_name} 0)"),
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

    /// Shorthand for calling the memory allocation method with the given memory size and align.
    ///
    /// Returns a pointer to the start of the created allocation
    MemAlloc(MemoryIndex, MemoryIndex),

    /// Allocates memory with the given size and align.
    ///
    /// Returns a pointer to the start of the created allocation
    StackAlloc(MemoryIndex, MemoryIndex),

    /// Shortcut for popping the given number of allocations from the stack
    StackPop(MemoryIndex),

    // /// Shorthand for:
    // /// 1. calling `stack_alloc` with the given size
    // /// 2. storing `{child}` into memory at the location returned by `stack_alloc`
    // /// 3. returning a pointer to the allocation
    // LoadToStack(MemoryIndex),

    // /// Shorthand for getting a variable with the given name and type
    // ///
    // /// `({t}.load (global.get ${name}))`
    // Get(WasmType, String),
    // /// Shorthand for setting a variable with the given name and type
    // ///
    // /// `({t}.store (global.get ${name}) ({child}))`
    // Set(WasmType, String),
    /// `1` child, memory location
    Load(WasmType),
    /// `2` children, memory location and value
    Store(WasmType),

    Global(String, WasmType),
    Local(String, WasmType),
    // GlobalGet(String),
    /// `(local.get ${name})`
    LocalGet(String),
    /// `(local.set ${name})`
    ///
    /// Note that `LocalSet` does *not* accept children, since the
    /// new value must be loaded prior to this instruction
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

    /// `{child_0} {child_1} ... {child_n}`
    ///
    /// Note that this does not group the children in parentheses
    Empty,
}

impl Display for WasmASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub fn compile_irast(irast: &IRAST) -> anyhow::Result<WasmAST> {
    let wir = wir::compile_irast(irast)?;

    compile_wir(&wir, irast)
}

fn compile_wir(wir: &WIRAST, irast: &IRAST) -> anyhow::Result<WasmAST> {
    let mut wasm = WasmAST::new();

    let wasm_head = wasm.tree.new_node(WasmASTNode::Empty);

    compile_wir_recurse(
        wir,
        wir.tree.find_head().unwrap(),
        &mut wasm,
        wasm_head,
        &mut HashSet::new(),
    )?;

    // Get every module
    let modules = wasm.tree[wasm_head].children.clone();

    // Add stuff at the beginning of each module (i.e. imports from the host)

    for parent in modules {
        // Export "memory"
        {
            let mem = wasm.tree.new_node(WasmASTNode::Export(
                ExportObjType::Memory("memory".into()),
                "memory".into(),
            ));

            wasm.tree.prepend_to(parent, mem)?;

            let mem = wasm.tree.new_node(WasmASTNode::Memory);
            wasm.tree.prepend_to(parent, mem)?;
        }

        // Import everything else
        for (_, info) in irast.idents.iter().map(|(a, b)| (*a, b.clone())) {
            // let (ret, name) = imp.name_and_return_type();

            match info {
                IdentInfo::ExternMethod {
                    mod_name,
                    imp_name,
                    name,
                    params,
                    return_type,
                } => {
                    let n = wasm.tree.new_node(WasmASTNode::Import {
                        env: mod_name.clone(),
                        imp_name: imp_name.clone(),

                        t: ImportObjType::Func {
                            wasm_name: name.clone(),
                            parameters: params
                                .iter()
                                .map(|(_, t)| WasmType::from_type(t))
                                .collect(),
                            ret: WasmType::from_type(&return_type),
                        },
                    });
                    wasm.tree.prepend_to(parent, n)?;
                }
                _ => (),
            }
        }
    }

    Ok(wasm)
}

fn compile_wir_recurse(
    wir: &WIRAST,
    curr: NodeId,
    wasm: &mut WasmAST,
    parent_wasm: NodeId,
    used_variables: &mut HashSet<(String, WasmType)>,
) -> anyhow::Result<()> {
    let children = wir.tree[curr].children.clone();
    match &wir.tree[curr].data {
        WIRNode::Module => {
            let parent = wasm.tree.new_node(WasmASTNode::Module);
            wasm.tree.append_to(parent_wasm, parent)?;
            for c in children {
                compile_wir_recurse(wir, c, wasm, parent, used_variables)?;
            }
        }
        WIRNode::FuncDef {
            name,
            name_export,
            params,
            ret,
        } => {
            let mut used_variables = HashSet::new();

            let f = wasm.tree.new_node(WasmASTNode::Empty);
            wasm.tree.append_to(parent_wasm, f)?;

            // At the beginning of the method body, move each parameter into memory

            let params: Vec<_> = params
                .iter()
                .map(|(name, t)| (name.clone(), WasmType::from_type(t)))
                .collect();

            for (name, t) in params.iter() {
                let param_name = format!("param_{name}");

                // Steps:
                // 1- Register the local variable in `used_variables` to guarantee instantiation
                // 2- Allocate the local variable in stack
                // 3- Copy the param variable to stack at location given by local variable

                // 1
                used_variables.insert((name.clone(), t.clone()));

                // 2
                let var_dec = wasm.tree.new_node(WasmASTNode::LocalSet(name.clone()));
                let alloc = wasm.tree.new_node(WasmASTNode::StackAlloc(t.size(), 1));
                wasm.tree.append_to(f, alloc)?;
                wasm.tree.append_to(f, var_dec)?;

                // 3
                let store = wasm.tree.new_node(WasmASTNode::Store(t.clone()));
                let loc = wasm.tree.new_node(WasmASTNode::LocalGet(name.clone()));
                let val = wasm.tree.new_node(WasmASTNode::LocalGet(param_name));
                wasm.tree.append_to(store, loc)?;
                wasm.tree.append_to(store, val)?;
                wasm.tree.append_to(f, store)?;
            }

            compile_wir_recurse(wir, children[0], wasm, f, &mut used_variables)?;

            let f_dat = WasmASTNode::FuncDef {
                name: name.clone(),
                name_export: name_export.clone(),
                params: params
                    .into_iter()
                    .map(|(name, t)| (format!("param_{name}"), t))
                    .collect(),
                ret: vec![WasmType::from_type(ret)],
                used_variables,
            };
            wasm.tree[f].data = f_dat;
        }

        // Things which translate more or less directly from WIR

        // Loading params is dealt with in `WIR`
        WIRNode::FuncCall(name) => {
            let n = wasm.tree.new_node(WasmASTNode::FuncCall(name.clone()));
            wasm.tree.append_to(parent_wasm, n)?;
        }

        // In general, control flow is wasmified in `WIR`
        WIRNode::Loop(name) => {
            let n = wasm.tree.new_node(WasmASTNode::Loop(name.clone()));
            wasm.tree.append_to(parent_wasm, n)?;

            for c in children {
                compile_wir_recurse(wir, c, wasm, n, used_variables)?;
            }
        }
        WIRNode::Block(name) => {
            let n = wasm.tree.new_node(WasmASTNode::Block(name.clone()));
            wasm.tree.append_to(parent_wasm, n)?;

            for c in children {
                compile_wir_recurse(wir, c, wasm, n, used_variables)?;
            }
        }
        WIRNode::Br(name) => {
            let n = wasm.tree.new_node(WasmASTNode::Br(name.clone()));
            wasm.tree.append_to(parent_wasm, n)?;
        }
        WIRNode::End => {
            let n = wasm.tree.new_node(WasmASTNode::End);
            wasm.tree.append_to(parent_wasm, n)?;
        }

        WIRNode::If => {
            let n = wasm.tree.new_node(WasmASTNode::If);
            wasm.tree.append_to(parent_wasm, n)?;

            for c in children {
                compile_wir_recurse(wir, c, wasm, n, used_variables)?;
            }
        }
        WIRNode::Then => {
            let n = wasm.tree.new_node(WasmASTNode::Then);
            wasm.tree.append_to(parent_wasm, n)?;

            for c in children {
                compile_wir_recurse(wir, c, wasm, n, used_variables)?;
            }
        }
        WIRNode::Else => {
            let n = wasm.tree.new_node(WasmASTNode::Else);
            wasm.tree.append_to(parent_wasm, n)?;

            for c in children {
                compile_wir_recurse(wir, c, wasm, n, used_variables)?;
            }
        }

        WIRNode::Const(v) => {
            let val = WasmValue::from_val(v.clone());
            let n = wasm.tree.new_node(WasmASTNode::Const(val));
            wasm.tree.append_to(parent_wasm, n)?;

            for c in children {
                compile_wir_recurse(wir, c, wasm, n, used_variables)?;
            }
        }

        /*
        (ptr_t.add
            (local.get ${name})
            (ptr_t.const {offset})
        )
        */
        WIRNode::GetRefLocal(name, offset) => {
            let add = wasm.tree.new_node(WasmASTNode::Add(WasmType::ptr_type()));
            let local_get = wasm.tree.new_node(WasmASTNode::LocalGet(name.clone()));
            let offset = wasm
                .tree
                .new_node(WasmASTNode::Const(WasmValue::ptr_val(*offset)));

            wasm.tree.append_to(parent_wasm, add)?;
            wasm.tree.append_to(add, local_get)?;
            wasm.tree.append_to(add, offset)?;
        }
        /*
        (t.load
            (local.get ${name})
        )
        */
        WIRNode::GetValueLocal(name, t) => {
            let t = WasmType::from_type(t);

            let load = wasm.tree.new_node(WasmASTNode::Load(t));
            let local_get = wasm.tree.new_node(WasmASTNode::LocalGet(name.clone()));

            wasm.tree.append_to(parent_wasm, load)?;
            wasm.tree.append_to(load, local_get)?;
        }
        /*
        (t.store
            (local.get ${name})
            {child}
        )

        OR

        {previous node, which puts the right value on the stack}
        (t.store
            (local.get ${name})
        )

        */
        WIRNode::SetLocal(name, t) => {
            let t = WasmType::from_type(t);
            // Register as a used variable
            {
                used_variables.insert((name.clone(), t.clone()));
            }

            let store = wasm.tree.new_node(WasmASTNode::Store(t));
            let local_get = wasm.tree.new_node(WasmASTNode::LocalGet(name.clone()));

            wasm.tree.append_to(store, local_get)?;
            wasm.tree.append_to(parent_wasm, store)?;

            if let Some(c) = children.get(0) {
                compile_wir_recurse(wir, *c, wasm, store, used_variables)?;
            }
        }
        /*
        (StackAlloc)
        (local.set ${name})
        */
        WIRNode::AllocLocal(name, t) => {
            let t = WasmType::from_type(t);

            let alloc = wasm.tree.new_node(WasmASTNode::StackAlloc(t.size(), 1));
            let set = wasm.tree.new_node(WasmASTNode::LocalSet(name.clone()));

            wasm.tree.append_to(parent_wasm, alloc)?;
            wasm.tree.append_to(parent_wasm, set)?;
        }
        WIRNode::DropPoint => {
            dbg!("TODO: DropPoint");
        }

        // Binary operations translate more-or-less directly, with the exception of
        // custom type implementations for ops (which are function calls)
        //
        // Luckily for me, traits aren't implemented yet. Lucky break
        WIRNode::Eq(t)
        | WIRNode::Ne(t)
        | WIRNode::Lt(t)
        | WIRNode::Gt(t)
        | WIRNode::Le(t)
        | WIRNode::Ge(t)
        | WIRNode::Add(t)
        | WIRNode::Sub(t)
        | WIRNode::Mul(t)
        | WIRNode::Div(t) => {
            let t_wasm = WasmType::from_type(t);
            let parent = wasm.tree.new_node(match wir.tree[curr].data {
                WIRNode::Eq(_) => WasmASTNode::Eq(t_wasm),
                WIRNode::Ne(_) => WasmASTNode::Ne(t_wasm),
                WIRNode::Lt(_) => WasmASTNode::Lt(t_wasm),
                WIRNode::Gt(_) => WasmASTNode::Gt(t_wasm),
                WIRNode::Le(_) => WasmASTNode::Le(t_wasm),
                WIRNode::Ge(_) => WasmASTNode::Ge(t_wasm),

                WIRNode::Add(_) => WasmASTNode::Add(t_wasm),
                WIRNode::Sub(_) => WasmASTNode::Sub(t_wasm),
                WIRNode::Mul(_) => WasmASTNode::Mul(t_wasm),
                WIRNode::Div(_) => WasmASTNode::Div(t_wasm),

                _ => unreachable!(),
            });
            wasm.tree.append_to(parent_wasm, parent)?;

            for c in children {
                compile_wir_recurse(wir, c, wasm, parent, used_variables)?;
            }
        }
        WIRNode::Empty => {
            let parent = wasm.tree.new_node(WasmASTNode::Empty);
            wasm.tree.append_to(parent_wasm, parent)?;
            for c in children {
                compile_wir_recurse(wir, c, wasm, parent, used_variables)?;
            }
        }
    }
    Ok(())
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

// /// * `irast` - The IRAST which is being compiled from
// /// * `curr` - The current node in the IRAST
// /// * `wasm` - The tree representing the generated compiled wasm code
// /// * `used_vars` - The set of variables used within this function
// /// * `num_allocations_in_scope` - The number of stack allocations which have been made within the same scope as `curr`
// /// * `parent_wasm` - The id of the node which called this method recursively
// ///
// /// Each subtree is expected to leave its return value on the top of the stack
// fn compile_irast_recurse(
//     irast: &IRAST,
//     curr: NodeId,
//     wasm: &mut WasmAST,
//     used_vars: &mut HashSet<(String, WasmType)>,
//     num_stack_allocations_in_scope: &mut MemoryIndex,
//     parent_wasm: NodeId,
// ) -> anyhow::Result<()> {
//     let children = irast.tree[curr].children.clone();

//     match irast.tree[curr].data.t.clone() {
//         IRNodeType::Literal(v) => {
//             let n = wasm.tree.new_node(WasmASTNode::Const(match v {
//                 Value::Bool(n) => WasmValue::I32(n as u8 as i32),
//                 Value::Int(n) => WasmValue::I32(n as i32),
//                 Value::Int32(n) => WasmValue::I32(n as i32),
//                 _ => todo!(),
//             }));
//             wasm.tree.append_to(parent_wasm, n)?;
//         }
//         IRNodeType::VarRef(id) => {
//             let n = wasm.tree.new_node(WasmASTNode::LocalGet(
//                 // WasmType::from_type(&irast[id].return_type()),
//                 format!("{}", irast[id].name()),
//             ));
//             wasm.tree.append_to(parent_wasm, n)?;
//         }
//         // Get the local representing the pointer into memory and then
//         // add the appropriate offset
//         IRNodeType::FieldRef(_) => todo!(),

//         /* Assignments:

//         `
//         Store({child_type})
//             LocalGet({lhs_name}) # get the actual memory location
//             {child}              # get the value to store
//         `

//         The following is **prepended** as a variable declaration:
//         `
//         LocalSet({lhs_name})   # allocate the local variable
//             StackAlloc({lhs_size}, {align})
//         `

//         */
//         IRNodeType::VarDef(id) => {
//             let var_name = irast[id].name();
//             //Declaration is handled seperately, since it must be done at the start of the method

//             //Insert this variable as one which is used in the method
//             let t = {
//                 let t = WasmType::from_type(&irast[id].return_type());
//                 used_vars.insert((var_name.clone(), t.clone()));
//                 t
//             };

//             // Allocation
//             let local_set = wasm
//                 .tree
//                 .new_node(WasmASTNode::LocalSet(format!("{var_name}")));

//             let stack_alloc = wasm.tree.new_node(WasmASTNode::StackAlloc(t.size(), 1));

//             wasm.tree.append_to(local_set, stack_alloc)?;
//             wasm.tree.append_to(parent_wasm, local_set)?;

//             // Loading value into memory
//             compile_irast_recurse(
//                 irast,
//                 children[0],
//                 wasm,
//                 used_vars,
//                 num_stack_allocations_in_scope,
//                 parent,
//             )?;
//         }
//         IRNodeType::Assign(id) => {
//             let var_name = irast[id].name();
//             let parent = wasm
//                 .tree
//                 .new_node(WasmASTNode::LocalSet(format!("{var_name}")));
//             wasm.tree.append_to(parent_wasm, parent)?;

//             compile_irast_recurse(
//                 irast,
//                 children[0],
//                 wasm,
//                 used_vars,
//                 num_stack_allocations_in_scope,
//                 parent,
//             )?;
//         }

//         IRNodeType::MethodDef(id) => {
//             let method_name = irast[id].name();

//             let params: Vec<_> = {
//                 use IdentInfo::*;
//                 match &irast[id] {
//                     Method { params, .. } => params
//                         .iter()
//                         .map(|id| {
//                             let (ret, name) = irast[*id].name_and_return_type();
//                             (format!("{}", name), WasmType::from_type(&ret))
//                         })
//                         .collect(),
//                     ExternMethod { params, .. } => params
//                         .iter()
//                         .map(|(name, ret)| (format!("{}", name), WasmType::from_type(&ret)))
//                         .collect(),
//                     _ => unreachable!(),
//                 }
//             };

//             //Create standin for the real function def
//             let func = WasmASTNode::Empty;

//             let parent = wasm.tree.new_node(func);
//             wasm.tree.append_to(parent_wasm, parent)?;

//             let mut used_vars = HashSet::new();

//             let mut stack_allocs = 0;
//             compile_irast_recurse(
//                 irast,
//                 children[0],
//                 wasm,
//                 &mut used_vars,
//                 &mut stack_allocs,
//                 parent,
//             )?;

//             let func = WasmASTNode::FuncDef {
//                 name: format!("{method_name}"),
//                 name_export: format!(
//                     "{}",
//                     match &irast[id] {
//                         IdentInfo::Method { name, .. } => name,
//                         IdentInfo::ExternMethod { imp_name, .. } => imp_name,
//                         _ => unreachable!(),
//                     }
//                 ),
//                 used_variables: used_vars,
//                 params: params.clone(),
//                 ret: vec![WasmType::from_type(&irast[id].return_type())],
//             };

//             wasm.tree[parent].data = func;

//             //Deallocate all the stack allocations
//             let pop = wasm.tree.new_node(WasmASTNode::StackPop(stack_allocs));
//             wasm.tree.append_to(parent, pop)?;
//         }
//         IRNodeType::MethodCall(id) => {
//             //Get each param in order, then call
//             for c in children {
//                 compile_irast_recurse(
//                     irast,
//                     c,
//                     wasm,
//                     used_vars,
//                     num_stack_allocations_in_scope,
//                     parent_wasm,
//                 )?;
//             }

//             //Call
//             let call = wasm
//                 .tree
//                 .new_node(WasmASTNode::FuncCall(irast[id].name().clone()));
//             wasm.tree.append_to(parent_wasm, call)?;
//         }
//         /*
//         Since local variables store references into memory, getting a (raw) reference is just:
//         `
//         (
//             (local.set ${tmp_addr}
//                 (call $stack_alloc {ptr_size} {align})
//             )
//             (t.store
//                 (local.get ${tmp_addr})
//                 {child}
//             )
//             (local.get ${tmp_addr})
//         )
//         `
//         In terms of the AST, this is:
//         `
//             LocalSet({tmp_var})    // create a tmp variable which stores the pointer to allocation
//                 StackAlloc(..)
//             Store(..)              // store value of the child in the address `{tmp_var}`
//                 {child}
//             LocalGet({tmp_var})    // return the allocation pointer
//         `
//         Things will change when starting to use reference counting

//         A dereference is just:
//         `
//         (t.load
//             {child}
//         )
//         `
//         */
//         IRNodeType::Reference => {
//             let ptr_type = WasmType::from_type(&value::Type::ptr_type());

//             // let parent = wasm.tree.new_node(WasmASTNode::Load(ptr_type));
//             let tmp_var_name = generate_tmp();

//             // Create a tmp var which holds the pointer to memory
//             let tmp_var_dec = wasm
//                 .tree
//                 .new_node(WasmASTNode::LocalSet(tmp_var_name.clone()));
//             let stack_alloc = wasm
//                 .tree
//                 .new_node(WasmASTNode::StackAlloc(ptr_type.size(), 1));
//             wasm.tree.append_to(stack_alloc, tmp_var_dec)?;
//             wasm.tree.append_to(parent_wasm, stack_alloc)?;

//             // Register the tmp var for this method
//             {
//                 let t = ptr_type.clone();
//                 used_vars.insert((tmp_var_name.clone(), t));
//             }

//             let store = wasm.tree.new_node(WasmASTNode::Store(ptr_type.clone()));
//             compile_irast_recurse(
//                 irast,
//                 children[0],
//                 wasm,
//                 used_vars,
//                 num_stack_allocations_in_scope,
//                 store,
//             )?;
//             wasm.tree.append_to(parent_wasm, store)?;

//             let get = wasm.tree.new_node(WasmASTNode::LocalGet(tmp_var_name));
//             wasm.tree.append_to(parent_wasm, get)?;
//         }
//         IRNodeType::IfCondition => {
//             let t = WasmType::from_type(&irast[children[1]].data.return_type);

//             //Return value temporary var (only if there is a return value)
//             let ret = if t != WasmType::None {
//                 let s = generate_tmp();
//                 let id = wasm.tree.new_node(WasmASTNode::Local(s.clone(), t));
//                 wasm.tree.append_to(parent_wasm, id)?;
//                 Some(s)
//             } else {
//                 None
//             };

//             // First, generate the conditional (note that bools are i32s in wasm)
//             {
//                 compile_irast_recurse(
//                     irast,
//                     children[0],
//                     wasm,
//                     used_vars,
//                     num_stack_allocations_in_scope,
//                     parent_wasm,
//                 )?;
//             }

//             let parent = wasm.tree.new_node(WasmASTNode::If);
//             wasm.tree.append_to(parent_wasm, parent)?;

//             let then = wasm.tree.new_node(WasmASTNode::Then);
//             wasm.tree.append_to(parent, then)?;

//             let mut stack_allocs = 0;

//             compile_irast_recurse(irast, children[1], wasm, used_vars, &mut stack_allocs, then)?;

//             //Set the return var
//             if let Some(ret) = ret.clone() {
//                 let set = wasm.tree.new_node(WasmASTNode::LocalSet(ret));
//                 wasm.tree.append_to(then, set)?;
//             }

//             //Pop from stack
//             wasm.tree.append_to(
//                 then,
//                 wasm.tree.new_node(WasmASTNode::StackPop(stack_allocs)),
//             );

//             //Compile a right hand side *iff* there is an else statement
//             if children.len() > 2 {
//                 let mut stack_allocs = 0;

//                 let el = wasm.tree.new_node(WasmASTNode::Else);
//                 compile_irast_recurse(irast, children[2], wasm, used_vars, &mut stack_allocs, el)?;

//                 //Set the return var
//                 if let Some(ret) = ret.clone() {
//                     let set = wasm.tree.new_node(WasmASTNode::LocalSet(ret));
//                     wasm.tree.append_to(el, set)?;
//                 }

//                 //Pop from stack
//                 wasm.tree
//                     .append_to(el, wasm.tree.new_node(WasmASTNode::StackPop(stack_allocs)));
//             }

//             //If there's a return type, push its value to the wasm stack after exiting the if statement block
//             if let Some(ret) = ret {
//                 let ret = wasm.tree.new_node(WasmASTNode::LocalGet(ret));
//                 wasm.tree.append_to(parent_wasm, ret)?;
//             }
//         }
//         // `(block $b
//         //     (loop $a
//         //        (..)
//         //        (br $a)
//         //     )
//         // )`
//         //
//         // such that a call to `generate_prev_breakpoint_name` returns `$b`
//         IRNodeType::Loop => {
//             let loop_name = generate_breakpoint_name();
//             let block_name = generate_breakpoint_name();

//             let bl = wasm.tree.new_node(WasmASTNode::Block(block_name));
//             let lo = wasm.tree.new_node(WasmASTNode::Loop(loop_name.clone()));
//             wasm.tree.append_to(bl, lo)?;
//             wasm.tree.append_to(parent_wasm, bl)?;

//             compile_irast_recurse(
//                 irast,
//                 children[0],
//                 wasm,
//                 used_vars,
//                 num_stack_allocations_in_scope,
//                 lo,
//             )?;

//             //For the loop, always jump back to the start (this is what changes for a while loop)
//             let jump_back = wasm.tree.new_node(WasmASTNode::Br(loop_name));
//             wasm.tree.append_to(lo, jump_back)?;
//         }
//         IRNodeType::Break => {
//             let parent = wasm
//                 .tree
//                 .new_node(WasmASTNode::Br(generate_prev_breakpoint_name()));
//             wasm.tree.append_to(parent_wasm, parent)?;
//         }
//         IRNodeType::LastValueReturn => {
//             //Execute each expression -- all are guaranteed to return the correct types
//             // (either nothing for all but last or some value for the last), so no checking is necessary

//             for c in children {
//                 // println!("{c:?}");
//                 compile_irast_recurse(
//                     irast,
//                     c,
//                     wasm,
//                     used_vars,
//                     num_stack_allocations_in_scope,
//                     parent_wasm,
//                 )?;
//             }
//         }
//         IRNodeType::ValueConsume => {
//             compile_irast_recurse(
//                 irast,
//                 children[0],
//                 wasm,
//                 used_vars,
//                 num_stack_allocations_in_scope,
//                 parent_wasm,
//             )?;
//         }
//         //Binary ops:
//         //({t}.{op}
//         //  ({child_0})
//         //  ({child_1})
//         //)
//         IRNodeType::Eq
//         | IRNodeType::Ne
//         | IRNodeType::Lt
//         | IRNodeType::Gt
//         | IRNodeType::Le
//         | IRNodeType::Ge
//         | IRNodeType::Add
//         | IRNodeType::Sub
//         | IRNodeType::Mul
//         | IRNodeType::Div => {
//             let parent = wasm.tree.new_node(WasmASTNode::Module);
//             wasm.tree.append_to(parent_wasm, parent)?;

//             //Have lhs and rhs append themselves to `parent`
//             compile_irast_recurse(
//                 irast,
//                 children[0],
//                 wasm,
//                 used_vars,
//                 num_stack_allocations_in_scope,
//                 parent,
//             )?;
//             compile_irast_recurse(
//                 irast,
//                 children[1],
//                 wasm,
//                 used_vars,
//                 num_stack_allocations_in_scope,
//                 parent,
//             )?;

//             //The type of the wasm operation
//             let t = WasmType::from_type(&irast.tree[children[0]].data.return_type.clone());

//             wasm.tree[parent].data = match irast.tree[curr].data.t {
//                 IRNodeType::Eq => WasmASTNode::Eq(t),
//                 IRNodeType::Ne => WasmASTNode::Ne(t),
//                 IRNodeType::Lt => WasmASTNode::Lt(t),
//                 IRNodeType::Gt => WasmASTNode::Gt(t),
//                 IRNodeType::Le => WasmASTNode::Le(t),
//                 IRNodeType::Ge => WasmASTNode::Ge(t),
//                 //
//                 IRNodeType::Add => WasmASTNode::Add(t),
//                 IRNodeType::Sub => WasmASTNode::Sub(t),
//                 IRNodeType::Mul => WasmASTNode::Mul(t),
//                 IRNodeType::Div => WasmASTNode::Div(t),
//                 _ => unreachable!(),
//             };
//         }
//     }

//     Ok(())
// }
