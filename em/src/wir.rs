use std::{collections::HashSet, sync::Mutex};

use em_core::memory::MemoryIndex;
use once_cell::sync::Lazy;

use crate::{
    ir::{IRNodeType, IdentInfo, IRAST},
    tree::{NodeId, Tree},
    utils::{PREFIX_BREAKPOINT, PREFIX_TMP},
    value::{Type, Value},
};

/// The number of variables generated using `generate_tmp(..)`
static TMP_VAR_COUNT: Lazy<Mutex<usize>> = Lazy::new(|| Mutex::new(0));

fn generate_tmp() -> String {
    let mut count = TMP_VAR_COUNT.lock().unwrap();
    let s = format!("{}{}", PREFIX_TMP, count);
    *count += 1;

    s
}

/// The number of variables generated so far using `generate_breakpoint_name(..)`
static BREAKPOINT_COUNT: Lazy<Mutex<usize>> = Lazy::new(|| Mutex::new(0));

fn generate_breakpoint_name() -> String {
    let mut count = BREAKPOINT_COUNT.lock().unwrap();
    let s = format!("{}{}", PREFIX_BREAKPOINT, count);
    *count += 1;

    s
}

fn curr_breakpoint_name() -> String {
    prev_nth_breakpoint_name(0)
}

fn prev_breakpoint_name() -> String {
    prev_nth_breakpoint_name(1)
}

fn prev_nth_breakpoint_name(n: usize) -> String {
    let count = BREAKPOINT_COUNT.lock().unwrap();
    let s = format!("{}{}", PREFIX_BREAKPOINT, *count - n);

    s
}

pub struct WIRAST {
    pub tree: Tree<WIRNode>,
}

impl WIRAST {
    pub fn new() -> Self {
        Self { tree: Tree::new() }
    }
}

#[derive(Debug, Clone)]
pub enum WIRNode {
    Module,
    FuncDef {
        name: String,
        name_export: String,
        params: Vec<(String, Type)>,
        ret: Type,
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

    /// Gets the value of local variable `name` and `type`
    GetValueLocal(String, Type),
    /// Gets a reference to the local variable `name`, adding some offset
    GetRefLocal(String, MemoryIndex),
    /// Set the value of local variable `name` and `type` to `{child}`
    SetLocal(String, Type),

    /// When encountering a `DropPoint`, every local value
    /// in the same scope as the drop point is dropped.
    ///
    /// Variables declared outside of but accessable to this scope are not dropped
    DropPoint,

    Const(Value),

    Eq(Type),
    Ne(Type),

    Lt(Type),
    Gt(Type),
    Le(Type),
    Ge(Type),

    Add(Type),
    Sub(Type),
    Mul(Type),
    Div(Type),

    /// `{child_0} {child_1} ... {child_n}`
    ///
    /// Note that the children are grouped together into parentheses
    Empty,
}

pub fn compile_irast(irast: &IRAST) -> anyhow::Result<WIRAST> {
    let mut wir = WIRAST::new();
    let wir_parent = wir.tree.new_node(WIRNode::Module);
    compile_irast_recurse(irast, irast.tree.find_head().unwrap(), &mut wir, wir_parent)?;

    Ok(wir)
}

fn compile_irast_recurse(
    irast: &IRAST,
    curr: NodeId,
    wir: &mut WIRAST,
    parent_wir: NodeId,
) -> anyhow::Result<()> {
    let children = irast[curr].children.clone();

    match irast[curr].data.t.clone() {
        IRNodeType::Literal(v) => {
            let n = wir.tree.new_node(WIRNode::Const(v));
            wir.tree.append_to(parent_wir, n)?;
        }
        IRNodeType::VarRef(id) => {
            let name = irast[id].name().clone();
            let t = irast[id].return_type().clone();
            let n = wir.tree.new_node(WIRNode::GetValueLocal(name, t));
            wir.tree.append_to(parent_wir, n)?;
        }
        IRNodeType::FieldRef(_) => todo!("Fields are :("),
        // Var definitions and assignments are actually the *same* in WIR, since
        // local variable declaration is implicit (might change)
        IRNodeType::VarDef(id) | IRNodeType::Assign(id) => {
            let (lhs_t, lhs_name) = irast[id].name_and_return_type().clone();

            let set = wir
                .tree
                .new_node(WIRNode::SetLocal(lhs_name.clone(), lhs_t));

            wir.tree.append_to(parent_wir, set)?;

            compile_irast_recurse(irast, children[0], wir, set)?;
        }

        IRNodeType::MethodDef(id) => {
            let func = wir.tree.new_node(WIRNode::FuncDef {
                name: irast[id].name().clone(),
                name_export: format!(
                    "{}",
                    match &irast[id] {
                        IdentInfo::Method { name, .. } => name,
                        IdentInfo::ExternMethod { imp_name, .. } => imp_name,
                        _ => unreachable!(),
                    }
                ),
                params: match &irast[id] {
                    IdentInfo::Method {
                        params,
                        return_type,
                        ..
                    } => params
                        .iter()
                        .map(|id| {
                            let (t, name) = irast[*id].name_and_return_type();
                            (name.clone(), t)
                        })
                        .collect(),
                    IdentInfo::ExternMethod {
                        params,
                        return_type,
                        ..
                    } => params.clone(),
                    _ => unreachable!(),
                },
                ret: irast[id].return_type().clone(),
            });

            wir.tree.append_to(parent_wir, func)?;

            compile_irast_recurse(irast, children[0], wir, func)?;
        }
        IRNodeType::MethodCall(id) => {
            //Get each param in order, then call
            for c in children {
                compile_irast_recurse(irast, c, wir, parent_wir)?;
            }

            //Call
            let call = wir
                .tree
                .new_node(WIRNode::FuncCall(irast[id].name().clone()));
            wir.tree.append_to(parent_wir, call)?;
        }
        IRNodeType::Reference => {
            match &irast[children[0]].data.t {
                // When creating a reference to some variable which already exists,
                // referencing is fairly trivial
                IRNodeType::VarRef(id) => {
                    let var = irast[*id].name().clone();

                    let get_ref = wir.tree.new_node(WIRNode::GetRefLocal(var, 0));
                    wir.tree.append_to(parent_wir, get_ref)?;
                }
                IRNodeType::FieldRef(_) => todo!("References to fields are :("),

                // Otherwise, a temporary variable needs to be created which stores the inner *value*
                // while creating a reference to that value. Note that the temporary value is created in the same scope as the reference,
                // meaning that they will be dropped at the same time
                _ => {
                    // Create the temp var holding the value
                    let tmp_var_name = generate_tmp();
                    let tmp_t = irast[children[0]].data.return_type.clone();
                    let set_tmp = wir
                        .tree
                        .new_node(WIRNode::SetLocal(tmp_var_name.clone(), tmp_t));
                    wir.tree.append_to(parent_wir, set_tmp)?;

                    // Set the temp var value to the child of this node
                    compile_irast_recurse(irast, children[0], wir, set_tmp)?;

                    // Create a reference to the temp var
                    let get_ref = wir.tree.new_node(WIRNode::GetRefLocal(tmp_var_name, 0));
                    wir.tree.append_to(parent_wir, get_ref)?;
                }
            }
        }
        IRNodeType::IfCondition => {
            let t = irast[children[1]].data.return_type.clone();

            // Return value temporary var (only if there is a return value)
            // Note that, since this is only a declaration (the only assignment happens later), there is no assign here
            let ret = if t != Type::Void {
                let s = generate_tmp();
                // let id = wasm.tree.new_node(WIRNode::Local(s.clone(), t));
                // let id = wir.tree.new_node(WIRNode::SetLocal(s.clone(), t));
                // wir.tree.append_to(parent_wir, id)?;
                Some(s)
            } else {
                None
            };

            // First, generate the conditional (note that bools are i32s in wasm)
            {
                compile_irast_recurse(irast, children[0], wir, parent_wir)?;
            }

            let parent = wir.tree.new_node(WIRNode::If);
            wir.tree.append_to(parent_wir, parent)?;

            let then = wir.tree.new_node(WIRNode::Then);
            wir.tree.append_to(parent, then)?;

            compile_irast_recurse(irast, children[1], wir, then)?;

            //Set the return var
            if let Some(ret) = ret.clone() {
                let set = wir.tree.new_node(WIRNode::SetLocal(ret, t.clone()));
                wir.tree.append_to(then, set)?;
            }

            //Drop locals
            let d = wir.tree.new_node(WIRNode::DropPoint);
            wir.tree.append_to(then, d)?;

            //Compile a "else" side *iff* there is an else statement
            if children.len() > 2 {
                let mut stack_allocs = 0;

                let el = wir.tree.new_node(WIRNode::Else);
                compile_irast_recurse(irast, children[2], wir, el)?;

                //Set the return var
                if let Some(ret) = ret.clone() {
                    let set = wir.tree.new_node(WIRNode::SetLocal(ret, t.clone()));
                    wir.tree.append_to(el, set)?;
                }

                //Drop locals
                let d = wir.tree.new_node(WIRNode::DropPoint);
                wir.tree.append_to(el, d)?;
            }

            //If there's a return type, push its value to the wasm stack after exiting the if statement block
            if let Some(ret) = ret {
                // let ret = wir.tree.new_node(WIRNode::LocalGet(ret));
                let ret = wir.tree.new_node(WIRNode::GetValueLocal(ret, t.clone()));
                wir.tree.append_to(parent_wir, ret)?;
            }
        }
        IRNodeType::Loop => {
            let loop_name = generate_breakpoint_name();
            let block_name = generate_breakpoint_name();

            let bl = wir.tree.new_node(WIRNode::Block(block_name));
            let lo = wir.tree.new_node(WIRNode::Loop(loop_name.clone()));
            wir.tree.append_to(bl, lo)?;
            wir.tree.append_to(parent_wir, bl)?;

            compile_irast_recurse(irast, children[0], wir, lo)?;

            //For the loop, always jump back to the start (this is what changes for a while loop)
            let jump_back = wir.tree.new_node(WIRNode::Br(loop_name));
            wir.tree.append_to(lo, jump_back)?;
        }
        IRNodeType::Break => {
            let name = prev_breakpoint_name();
            let br = wir.tree.new_node(WIRNode::Br(name));
            wir.tree.append_to(parent_wir, br)?;
        }
        IRNodeType::LastValueReturn => {
            //Execute each expression -- all are guaranteed to return the correct types
            // (no type for all but last, since last can be some value), so no checking is necessary

            let p = wir.tree.new_node(WIRNode::Empty);
            wir.tree.append_to(parent_wir, p)?;

            for c in children {
                compile_irast_recurse(irast, c, wir, p)?;
            }
        }
        IRNodeType::ValueConsume => {
            //As with `LastValueReturn`, there is no need to check `ValueConsume`
            let p = wir.tree.new_node(WIRNode::Empty);
            wir.tree.append_to(parent_wir, p)?;

            compile_irast_recurse(irast, children[0], wir, p)?;
        }

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
            // Create the parent with a temporary value
            let parent = wir.tree.new_node(WIRNode::Empty);
            wir.tree.append_to(parent_wir, parent)?;

            // Have lhs and rhs append themselves to `parent`
            compile_irast_recurse(irast, children[0], wir, parent)?;
            compile_irast_recurse(irast, children[1], wir, parent)?;

            // Get type of the operation
            let t = irast.tree[children[0]].data.return_type.clone();

            // Replace the parent's temporary value with a real one
            wir.tree[parent].data = match irast.tree[curr].data.t {
                IRNodeType::Eq => WIRNode::Eq(t),
                IRNodeType::Ne => WIRNode::Ne(t),
                IRNodeType::Lt => WIRNode::Lt(t),
                IRNodeType::Gt => WIRNode::Gt(t),
                IRNodeType::Le => WIRNode::Le(t),
                IRNodeType::Ge => WIRNode::Ge(t),
                //
                IRNodeType::Add => WIRNode::Add(t),
                IRNodeType::Sub => WIRNode::Sub(t),
                IRNodeType::Mul => WIRNode::Mul(t),
                IRNodeType::Div => WIRNode::Div(t),
                _ => unreachable!(),
            };
        }
    }

    Ok(())
}
