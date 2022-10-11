use std::{
    collections::{HashMap, HashSet},
    sync::Mutex,
    vec,
};

use crate::{
    ast::{ASTNode, ASTNodeType, Value},
    ir::{IRNode, IdentInfo, IRAST},
    tree::{NodeId, Tree},
    verify::Type,
};

lazy_static! {
    static ref C_TMP_VAR_COUNT: Mutex<usize> = Mutex::new(0);
}

/// Generates a globally unique tmp variable name
fn generate_tmp() -> String {
    let mut count = C_TMP_VAR_COUNT.lock().unwrap();
    //Prefix is five underscores
    let s = format!("_____tmp_{}", count);
    *count += 1;
    s
}

#[derive(Debug, Clone)]
pub enum CASTNode {
    /// This node is ignored in evaluation.
    /// Can be used, for example, to have a sequence of statements without enclosing brackets or parens
    Ignore,
    Literal(Value),
    MethodDef {
        name: String,
        inputs: Vec<(Type, String)>,
        return_type: Type,
    },
    /// Represents an if statement
    ///
    /// `2` children, first is conditional, second is body
    If,
    /// Represents an assignment of a variable with the given name to the value returned by this node's child
    ///
    /// `1` child
    Assign(String),
    /// Represents a variable being declared, but *not* assigned, i.e.:
    /// `{type} {name}`. For example, `int my_num`
    ///
    /// `0` children
    VarDec {
        name: String,
        t: Type,
    },
    /// Represents a comma-seperated list of variable declarations, with no assignments. For example:
    /// `{type} {name0}, {name1}, {name2};`
    ///
    /// `0` children
    VarDecList {
        names: Vec<String>,
        t: Type,
    },
    /// Represents a variable being declared and assigned on one line, i.e.:
    /// `{type} {name} = {child}`. For example, `int my_num = (3 + (2-1))`
    ///
    /// `1` child
    VarDecAssign {
        name: String,
        t: Type,
    },
    /// Represents a variable name in an expression
    ///
    /// `0` children
    VarRef(String),
    /// `return {child};`
    ///
    /// `1` child
    Return,
    /// Represents putting a sequence of statements in curly braces, i.e.:
    /// `{{child_0}\n{child_1}\n...}`
    ///
    /// `1+` children
    Brackets,
    /// `{child};`
    ///
    /// `1` child
    Semicolon,
    //Basic ops
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl std::fmt::Display for CASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CASTNode::*;
        let s = match self {
            Ignore => format!("Ignore"),
            Literal(val) => format!("{val}"),
            MethodDef {
                name,
                inputs,
                return_type,
            } => {
                let inputs_string = inputs
                    .clone()
                    .into_iter()
                    .map(|(t, name)| format!("{} {name}", cast_type_to_string(t)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{} {name}({inputs_string})",
                    cast_type_to_string(*return_type)
                )
            }
            Assign(name) => format!("Assign `{name}`"),
            If => format!("If"),

            VarDec { name, t } => format!("Declare `{} {name}`", cast_type_to_string(*t)),
            VarDecList { names, t } => {
                let list = names.join(", ");
                format!("DeclareList `{} {list}`", cast_type_to_string(*t))
            }
            Brackets => format!("Brackets"),
            Semicolon => format!("Semicolon"),
            Add => format!("Add"),
            Sub => format!("Sub"),
            Return => format!("Return"),
            VarRef(name) => format!("VarRef `{name}`"),
            VarDecAssign { name, t } => {
                format!("DeclareAssign `{} {name}`", cast_type_to_string(*t))
            }
            Mul => format!("Mul"),
            Div => format!("Div"),
            Eq => format!("Eq"),
            Ne => format!("Ne"),
            Lt => format!("Lt"),
            Gt => format!("Gt"),
            Le => format!("Le"),
            Ge => format!("Ge"),
        };
        write!(f, "{s}")
    }
}

fn cast_literal_to_string(val: &Value) -> String {
    match val {
        Value::Void => format!("void"),
        Value::Bool(v) => format!("{v}"),
        Value::Int(v) => format!("{v}"),
        Value::Int32(v) => format!("{v}"),
        Value::String(_) => todo!(),
    }
}

fn cast_type_to_string(t: Type) -> String {
    match t {
        Type::Void => format!("void"),
        Type::Bool => format!("bool"),
        Type::Int => {
            println!(
                "Warning: `Type::Int` was passed to `cast_type_to_string`. This should be avoided but will default to `long`"
            );
            format!("long")
        }
        Type::Int32 => format!("int"),
        Type::String => todo!(),
    }
}

/// Takes in a `C` AST and outputs a string ready to be written to a `C` program file
pub fn cast_to_string(cast: &Tree<CASTNode>) -> String {
    //The items to be included at the top of the c file
    const INCLUDES: &'static [&str] = &["<stdbool.h>"];
    let includes_string = INCLUDES
        .clone()
        .into_iter()
        .map(|s| format!("#include {s}\n"))
        .collect::<Vec<_>>()
        .join("");
    let body_string =
        cast_to_string_recurse(cast, cast.find_head().expect("Couldn't find AST head"));

    format!("{includes_string}\n{body_string}")
}

fn cast_to_string_recurse(cast: &Tree<CASTNode>, curr: NodeId) -> String {
    let children = cast[curr].children.clone();

    /// Recursively calls `cast_to_string_recurse` on the provided node (given an index in `children`)
    macro_rules! child {
        ($idx:expr) => {
            cast_to_string_recurse(cast, children[$idx])
        };
    }
    /// Recursively calls `cast_to_string_recurse` on the provided node (given a `NodeId` in `cast`)
    macro_rules! child_id {
        ($idx:expr) => {
            cast_to_string_recurse(cast, c)
        };
    }

    /// Recursively calls `cast_to_string_recurse` on each child and joins their outputted strings with the given seperator
    macro_rules! join_all_children {
        ($sep:expr) => {
            children
                .into_iter()
                .map(|c| cast_to_string_recurse(cast, c))
                .collect::<Vec<_>>()
                .join($sep)
        };
    }

    match &cast[curr].data {
        CASTNode::Ignore => join_all_children!(""), //\n
        CASTNode::Literal(val) => cast_literal_to_string(val),
        CASTNode::MethodDef {
            name,
            inputs,
            return_type,
        } => {
            let inputs_string = inputs
                .into_iter()
                .map(|(t, name)| format!("{} {name}", cast_type_to_string(*t)))
                .collect::<Vec<_>>()
                .join(", ");
            let return_type_string = cast_type_to_string(*return_type);
            let body_string = join_all_children!(""); //\n
            format!("{return_type_string} {name}({inputs_string}) {{\n{body_string}\n}}")
        }
        CASTNode::If => format!("if ({}) {{{}}}", child!(0), child!(1)),

        //Basic bin ops
        CASTNode::Add => format!("({} + {})", child!(0), child!(1)),
        CASTNode::Sub => format!("({} - {})", child!(0), child!(1)),
        CASTNode::Mul => format!("({} * {})", child!(0), child!(1)),
        CASTNode::Div => format!("({} / {})", child!(0), child!(1)),
        CASTNode::Eq => format!("({} == {})", child!(0), child!(1)),
        CASTNode::Ne => format!("({} != {})", child!(0), child!(1)),
        CASTNode::Gt => format!("({} > {})", child!(0), child!(1)),
        CASTNode::Lt => format!("({} < {})", child!(0), child!(1)),
        CASTNode::Ge => format!("({} >= {})", child!(0), child!(1)),
        CASTNode::Le => format!("({} <= {})", child!(0), child!(1)),
        //
        CASTNode::Semicolon => format!("{};", child!(0)),
        CASTNode::Brackets => format!("{{{}}}", join_all_children!("")), //\n
        CASTNode::Assign(name) => format!("{name} = {}", child!(0)),
        CASTNode::VarDec { name, t } => format!("{} {name}", cast_type_to_string(*t)),
        CASTNode::VarDecList { names, t } => {
            let list = names.join(", ");
            format!("{} {list}", cast_type_to_string(*t))
        }
        CASTNode::Return => format!("return {};", child!(0)),
        CASTNode::VarRef(name) => format!("{name}"),
        CASTNode::VarDecAssign { name, t } => {
            format!("{} {name} = {}", cast_type_to_string(*t), child!(0))
        }
    }
}

pub fn ast_to_cast(ast: &Tree<ASTNode>) -> anyhow::Result<Tree<CASTNode>> {
    let mut ir_ast = IRAST::from_ast(ast)?;
    ir_ast.mangle(vec!["hello"].into_iter().collect());

    ir_ast_to_cast_recurse(
        &ir_ast,
        ast.find_head().expect("Couldn't find AST head"),
        None,
    )
}

// /// Mangles the input `ast` in such a way that all identifiers are globally unique
// ///
// /// For example:
// ///
// /// `let b = {let a = 3; let b = {let a = 4; a}; let c = {let a = 4; a}; b + c}`
// ///
// /// would be turned into something like:
// ///
// /// `let _0_b = {let _0_a = 3; let _1_b = {let _1_a = 4; _1_a}; let _1_c = {let _2_a = 4; _2_a} _1_b}`
// fn mangle_ast(ast: &mut Tree<ASTNode>) -> anyhow::Result<()> {
//     mangle_ast_recurse(ast, ast.find_head().unwrap(), &mut 0, &mut HashMap::new());
//     Ok(())
// }

// fn mangle_ast_recurse(
//     ast: &mut Tree<ASTNode>,
//     curr: NodeId,
//     ident_count: &mut usize,
//     var_replacements: &mut HashMap<String, String>,
// ) {
//     let parent = ast[curr].parent.unwrap();
//     /// Generates a new unique identifier using the supplied base name
//     macro_rules! gen_ident {
//         ($base_name:expr) => {{
//             let s = format!("_{}_{}", *ident_count, $base_name);
//             *ident_count += 1;
//             s
//         }};
//     }
//     //If the current scope is deeper than the parent, generate a new hashmap to be passed among the children
//     let vars = var_replacements;
//     if ast[curr].data.scope_depth > ast[parent].data.scope_depth {
//         //*vars = var_replacements.clone()
//     }

//     todo!()
// }

/// * `ast` - The input AST for EmScript code
/// * `curr_ast` - The current node to be evaluated in `ast`
/// * `expr_tmp_vars` - The name of the variable which should be assigned to in this
fn ir_ast_to_cast_recurse(
    ast: &IRAST,
    curr_ast: NodeId,
    return_var: Option<String>,
) -> anyhow::Result<Tree<CASTNode>> {
    let children = ast[curr_ast].children.clone();

    let mut cast = Tree::new();

    // //tmp
    // println!("{} -> {:?}", ast[curr_ast].data, return_var);

    /// Shorthand for adding a node to `cast` and appending all children of `curr_ast` to it
    ///
    /// `return_var` will be passed *only* to the last child. All others will receive `None`
    macro_rules! single_parent {
        ($child:expr) => {{
            let n = cast.new_node($child);
            if !children.is_empty() {
                for i in 0..children.len() - 1 {
                    let c = children[i];
                    let mut c_tree = ir_ast_to_cast_recurse(ast, c, None)?;
                    cast.append_tree(n, &mut c_tree)?;
                }
                let last_child = children.last().unwrap();
                let mut c_tree = ir_ast_to_cast_recurse(ast, *last_child, return_var)?;
                cast.append_tree(n, &mut c_tree)?;
            }
        }};
    }

    macro_rules! single_parent_no_return {
        ($child:expr) => {{
            let n = cast.new_node($child);
            for i in 0..children.len() {
                let c = children[i];
                let mut c_tree = ir_ast_to_cast_recurse(ast, c, None)?;
                cast.append_tree(n, &mut c_tree)?;
            }
        }};
    }

    /// * `child_index`
    /// * `return_var`
    macro_rules! recurse_child {
        ($child_index:expr, $return_var:expr) => {
            ir_ast_to_cast_recurse(ast, children[$child_index], $return_var)?
        };
    }

    match &ast[curr_ast].data {
        IRNode::Literal(val) => {
            //When a literal has a return var, set `{return_var} = {val};`
            //Otherwise, just return the literal itself
            if let Some(return_var) = return_var {
                let semicolon = cast.new_node(CASTNode::Semicolon);
                let assign = cast.new_node(CASTNode::Assign(return_var.clone()));
                let rhs = cast.new_node(CASTNode::Literal(val.clone()));
                cast.append_to(assign, rhs)?;
                cast.append_to(semicolon, assign)?;
            } else {
                single_parent!(CASTNode::Literal(val.clone()))
            }
        }
        IRNode::VarRef(id) => {
            let name = ast[*id].name();
            //When a var ref has a return var, set `{return_var} = {val};`
            //Otherwise, just return the literal itself
            if let Some(return_var) = return_var {
                let semicolon = cast.new_node(CASTNode::Semicolon);
                let assign = cast.new_node(CASTNode::Assign(return_var.clone()));
                let rhs = cast.new_node(CASTNode::VarRef(name.clone()));
                cast.append_to(assign, rhs)?;
                cast.append_to(semicolon, assign)?;
            } else {
                single_parent!(CASTNode::VarRef(name.clone()))
            }
        }
        IRNode::VarDef(id) => {
            let name = ast[*id].name();
            if let None = &return_var {
                let parent = cast.new_node(CASTNode::Ignore);

                //Create a variable declaration
                {
                    let semicolon = cast.new_node(CASTNode::Semicolon);
                    let var_dec = cast.new_node(CASTNode::VarDec {
                        name: name.clone(),
                        t: Type::Int32,
                    });
                    cast.append_to(semicolon, var_dec)?;
                    cast.append_to(parent, semicolon)?;
                }

                //Compile the rhs with `name` as return_var
                {
                    let brackets = cast.new_node(CASTNode::Brackets);
                    let mut rhs = recurse_child!(0, Some(name.clone()));

                    cast.append_tree(brackets, &mut rhs)?;
                    cast.append_to(parent, brackets)?;
                }
            } else {
                return Err(anyhow::format_err!(
                    "An assignment [VarDef] occured with `return_var = {return_var:?}`, when assignment returns void"
                ));
            }
        }
        IRNode::MethodDef(id) => {
            // Gather the method's information
            let (name, inputs, return_type) = {
                let method_info = &ast[*id];

                if let IdentInfo::Method {
                    name,
                    params,
                    return_type,
                } = method_info
                {
                    (name, params, *return_type)
                } else {
                    return Err(anyhow::format_err!(
                    "Should never happen: attempted to parse `MethodDef` whose identifier referred to a `Var`, not `Method` ({})",
                    method_info.name()
                    ));
                }
            };
            //Create the parent, which will be a `MethodDef` node
            let inputs: Vec<_> = inputs
                .iter()
                .map(|id| {
                    let (t, s) = ast[*id].name_and_return_type();
                    (t, s.clone())
                })
                .collect();
            let parent = cast.new_node(CASTNode::MethodDef {
                name: name.clone(),
                inputs: inputs.clone(),
                return_type: return_type.clone(),
            });
            //Create a temporary variable to hold the return value
            let return_val_tmp = generate_tmp();
            {
                let semicolon = cast.new_node(CASTNode::Semicolon);
                let return_val_dec = cast.new_node(CASTNode::VarDec {
                    name: return_val_tmp.clone(),
                    t: Type::Int32,
                });
                cast.append_to(semicolon, return_val_dec)?;
                cast.append_to(parent, semicolon)?;
            }

            //Recurse on the body using the return value `return_val_tmp`
            {
                let child = &mut recurse_child!(0, Some(return_val_tmp.clone()));
                cast.append_tree(parent, child)?;
            }

            //Return the temporary return value
            {
                let return_statement = cast.new_node(CASTNode::Return);
                let return_val_ref = cast.new_node(CASTNode::VarRef(return_val_tmp));
                cast.append_to(return_statement, return_val_ref)?;
                cast.append_to(parent, return_statement)?;
            }
        }
        IRNode::MethodCall(id) => {
            // Gather the method's information
            let (name, inputs, return_type) = {
                let method_info = &ast[*id];

                if let IdentInfo::Method {
                    name,
                    params,
                    return_type,
                } = method_info
                {
                    (name, params, *return_type)
                } else {
                    return Err(anyhow::format_err!(
                    "Should never happen: attempted to parse `MethodCall` whose identifier referred to a `Var`, not `Method` ({})",
                    method_info.name()
                    ));
                }
            };
            // println!("Method call: {:?}", ast[*id].name_and_return_type());

            let parent = cast.new_node(CASTNode::Ignore);

            //A list of the names of all temporary variables for holding parameter values
            let mut params_tmp_vars = Vec::with_capacity(children.len());
            //For each parameter, create a temporary variable
            for param in children {
                //Generate the temporary variable
                let param_tmp_var = generate_tmp();

                let semicolon = cast.new_node(CASTNode::Semicolon);
                let param_tmp_dec = cast.new_node(CASTNode::VarDec {
                    name: param_tmp_var.clone(),
                    t: Type::Int32,
                });
                cast.append_to(semicolon, param_tmp_dec)?;
                cast.append_to(parent, semicolon)?;

                //Push the temporary variable to the string
                params_tmp_vars.push(param_tmp_var);
            }

            todo!();
        }
        IRNode::IfCondition => {
            let parent = cast.new_node(CASTNode::Ignore);
            let t = Type::Int32;
            let tmp_conditional = generate_tmp();
            //TODO: deal with the case where the body returns `void`
            // let tmp_body = generate_tmp();

            //Create declarations for the temp variables
            {
                let semicolon = cast.new_node(CASTNode::Semicolon);
                let var_dec_list = cast.new_node(CASTNode::VarDecList {
                    names: vec![tmp_conditional.clone()],
                    // names: vec![tmp_conditional.clone(), tmp_body.clone()],
                    t,
                });
                cast.append_to(semicolon, var_dec_list)?;
                cast.append_to(parent, semicolon)?;
            }

            //Compile conditional value
            {
                let brackets = cast.new_node(CASTNode::Brackets);
                cast.append_to(parent, brackets)?;

                let condition = &mut recurse_child!(0, Some(tmp_conditional.clone()));
                cast.append_tree(brackets, condition)?;
            }
            //Compile if-statement
            {
                let if_statement = cast.new_node(CASTNode::If);
                cast.append_to(parent, if_statement)?;

                //Append condition
                let condition = cast.new_node(CASTNode::VarRef(tmp_conditional.clone()));
                cast.append_to(if_statement, condition)?;

                //Append body
                let body = &mut recurse_child!(1, return_var.clone());
                cast.append_tree(if_statement, body)?;
            }

            // if let Some(n) = return_var {}
        }
        //Binary operations
        IRNode::Add
        | IRNode::Sub
        | IRNode::Mul
        | IRNode::Div
        | IRNode::Eq
        | IRNode::Ne
        | IRNode::Lt
        | IRNode::Gt
        | IRNode::Le
        | IRNode::Ge => {
            let parent = cast.new_node(CASTNode::Ignore);
            let t = Type::Int32;
            let tmp_1 = generate_tmp();
            let tmp_2 = generate_tmp();

            //Create declarations for the temp variables
            {
                let semicolon = cast.new_node(CASTNode::Semicolon);
                let var_dec_list = cast.new_node(CASTNode::VarDecList {
                    names: vec![tmp_1.clone(), tmp_2.clone()],
                    t,
                });
                cast.append_to(semicolon, var_dec_list)?;
                cast.append_to(parent, semicolon)?;
            }
            //Compile the `lhs`
            {
                let brackets = cast.new_node(CASTNode::Brackets);
                cast.append_to(parent, brackets)?;

                let lhs = &mut recurse_child!(0, Some(tmp_1.clone()));
                cast.append_tree(brackets, lhs)?;
            }
            //Compile the `rhs`
            {
                let brackets = cast.new_node(CASTNode::Brackets);
                cast.append_to(parent, brackets)?;

                let rhs = &mut recurse_child!(1, Some(tmp_2.clone()));
                cast.append_tree(brackets, rhs)?;
            }
            //Final assignment statement
            {
                let semicolon = cast.new_node(CASTNode::Semicolon);
                let assignment = cast.new_node(CASTNode::Assign(return_var.unwrap()));
                cast.append_to(semicolon, assignment)?;
                cast.append_to(parent, semicolon)?;

                let op = {
                    let op = match &ast[curr_ast].data {
                        IRNode::Add => CASTNode::Add,
                        IRNode::Sub => CASTNode::Sub,
                        IRNode::Mul => CASTNode::Mul,
                        IRNode::Div => CASTNode::Div,
                        IRNode::Eq => CASTNode::Eq,
                        IRNode::Ne => CASTNode::Ne,
                        IRNode::Lt => CASTNode::Lt,
                        IRNode::Gt => CASTNode::Gt,
                        IRNode::Le => CASTNode::Le,
                        IRNode::Ge => CASTNode::Ge,
                        _ => unreachable!(),
                    };
                    cast.new_node(op)
                };
                cast.append_to(assignment, op)?;

                let tmp_1 = cast.new_node(CASTNode::VarRef(tmp_1));
                cast.append_to(op, tmp_1)?;
                let tmp_2 = cast.new_node(CASTNode::VarRef(tmp_2));
                cast.append_to(op, tmp_2)?;
            }

            // {t} {tmp_1}, {tmp_2};
            // { {compile(lhs, tmp_1)} }
            // { {compile(rhs, tmp_2)} }
            // return_var = {tmp_1} + {tmp_2};
            // todo!()
        }
        IRNode::Assign(id) => {
            let name = ast[*id].name();
            if let Some(_return_var) = return_var.clone() {
                single_parent!(CASTNode::Assign(name.clone()))
            } else {
                return Err(anyhow::format_err!(
                    "An assignment occured with `return_var = None`"
                ));
            }
        }
        //LastValueReturn:
        //When parent is a method def OR there is no parent OR the parent is also a LastValueReturn, ignore
        //When parent is an assignment, set final child to assignment and have the parent be brackets
        //When parent is a VariableDef, replace `curr` with two nodes: a C::VariableDef and brackets (treated like an assignment would be)

        // `return_var` is passed to only the final child. This is handled by `single_parent` by default
        // When the children are method definitions, brackets would be syntatically invalid
        IRNode::LastValueReturn => match &ast[children[0]].data {
            IRNode::MethodDef { .. } | IRNode::LastValueReturn => {
                single_parent_no_return!(CASTNode::Ignore)
            }
            _ => single_parent!(CASTNode::Brackets),
        },
        IRNode::ValueConsume => match &ast[children[0]].data {
            IRNode::VarDef { .. } => single_parent!(CASTNode::Ignore),
            _ => single_parent!(CASTNode::Semicolon),
        },
    }

    Ok(cast)
}

// /// * `ast` - The input AST for EmScript code
// /// * `curr_ast` - The current node to be evaluated in `ast`
// /// * `expr_tmp_vars` - The name of the variable which should be assigned to in this
// fn ast_to_cast_recurse(
//     ast: &Tree<ASTNode>,
//     curr_ast: NodeId,
//     return_var: Option<String>,
// ) -> anyhow::Result<Tree<CASTNode>> {
//     let children = ast[curr_ast].children.clone();

//     let mut cast = Tree::new();

//     // //tmp
//     // println!("{} -> {:?}", ast[curr_ast].data, return_var);

//     /// Shorthand for adding a node to `cast` and appending all children of `curr_ast` to it
//     ///
//     /// `return_var` will be passed *only* to the last child. All others will receive `None`
//     macro_rules! single_parent {
//         ($child:expr) => {{
//             let n = cast.new_node($child);
//             if !children.is_empty() {
//                 for i in 0..children.len() - 1 {
//                     let c = children[i];
//                     let mut c_tree = ast_to_cast_recurse(ast, c, None)?;
//                     cast.append_tree(n, &mut c_tree)?;
//                 }
//                 let last_child = children.last().unwrap();
//                 let mut c_tree = ast_to_cast_recurse(ast, *last_child, return_var)?;
//                 cast.append_tree(n, &mut c_tree)?;
//             }
//         }};
//     }

//     macro_rules! single_parent_no_return {
//         ($child:expr) => {{
//             let n = cast.new_node($child);
//             for i in 0..children.len() {
//                 let c = children[i];
//                 let mut c_tree = ast_to_cast_recurse(ast, c, None)?;
//                 cast.append_tree(n, &mut c_tree)?;
//             }
//         }};
//     }

//     /// * `child_index`
//     /// * `return_var`
//     macro_rules! recurse_child {
//         ($child_index:expr, $return_var:expr) => {
//             ast_to_cast_recurse(ast, children[$child_index], $return_var)?
//         };
//     }

//     match &ast[curr_ast].data.t {
//         ASTNodeType::Literal { val } => {
//             //When a literal has a return var, set `{return_var} = {val};`
//             //Otherwise, just return the literal itself
//             if let Some(return_var) = return_var {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let assign = cast.new_node(CASTNode::Assign(return_var.clone()));
//                 let rhs = cast.new_node(CASTNode::Literal(val.clone()));
//                 cast.append_to(assign, rhs)?;
//                 cast.append_to(semicolon, assign)?;
//             } else {
//                 single_parent!(CASTNode::Literal(val.clone()))
//             }
//         }
//         ASTNodeType::VariableRef { name } => {
//             //When a var ref has a return var, set `{return_var} = {val};`
//             //Otherwise, just return the literal itself
//             if let Some(return_var) = return_var {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let assign = cast.new_node(CASTNode::Assign(return_var.clone()));
//                 let rhs = cast.new_node(CASTNode::VarRef(name.clone()));
//                 cast.append_to(assign, rhs)?;
//                 cast.append_to(semicolon, assign)?;
//             } else {
//                 single_parent!(CASTNode::VarRef(name.clone()))
//             }
//         }
//         ASTNodeType::VariableDef { name } => {
//             if let None = &return_var {
//                 let parent = cast.new_node(CASTNode::Ignore);

//                 //Create a variable declaration
//                 {
//                     let semicolon = cast.new_node(CASTNode::Semicolon);
//                     let var_dec = cast.new_node(CASTNode::VarDec {
//                         name: name.clone(),
//                         t: Type::Int32,
//                     });
//                     cast.append_to(semicolon, var_dec)?;
//                     cast.append_to(parent, semicolon)?;
//                 }

//                 //Compile the rhs with `name` as return_var
//                 {
//                     let brackets = cast.new_node(CASTNode::Brackets);
//                     let mut rhs = recurse_child!(0, Some(name.clone()));

//                     cast.append_tree(brackets, &mut rhs)?;
//                     cast.append_to(parent, brackets)?;
//                 }
//             } else {
//                 return Err(anyhow::format_err!(
//                     "An assignment [VarDef] occured with `return_var = {return_var:?}`, when assignment returns void"
//                 ));
//             }
//         }
//         ASTNodeType::MethodDef {
//             name,
//             inputs,
//             return_type,
//         } => {
//             let parent = cast.new_node(CASTNode::MethodDef {
//                 name: name.clone(),
//                 inputs: inputs.clone(),
//                 return_type: return_type.clone(),
//             });
//             //Create a remporary variable to hold the return value
//             let return_val_tmp = generate_tmp();
//             {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let return_val_dec = cast.new_node(CASTNode::VarDec {
//                     name: return_val_tmp.clone(),
//                     t: Type::Int32,
//                 });
//                 cast.append_to(semicolon, return_val_dec)?;
//                 cast.append_to(parent, semicolon)?;
//             }

//             //Recurse on the body using the return value `return_val_tmp`
//             {
//                 let child = &mut recurse_child!(0, Some(return_val_tmp.clone()));
//                 cast.append_tree(parent, child)?;
//             }

//             //Return the temporary return value
//             {
//                 let return_statement = cast.new_node(CASTNode::Return);
//                 let return_val_ref = cast.new_node(CASTNode::VarRef(return_val_tmp));
//                 cast.append_to(return_statement, return_val_ref)?;
//                 cast.append_to(parent, return_statement)?;
//             }
//         }
//         ASTNodeType::MethodCall { name } => {
//             // let params_tmp_vars = Vec::with_capacity(children.len());
//             // //For each parameter
//             // for param in children {
//             //     params_tmp_vars.push();
//             // }
//         }
//         ASTNodeType::IfCondition => {
//             let parent = cast.new_node(CASTNode::Ignore);
//             let t = Type::Int32;
//             let tmp_conditional = generate_tmp();
//             //TODO: deal with the case where the body returns `void`
//             // let tmp_body = generate_tmp();

//             //Create declarations for the temp variables
//             {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let var_dec_list = cast.new_node(CASTNode::VarDecList {
//                     names: vec![tmp_conditional.clone()],
//                     // names: vec![tmp_conditional.clone(), tmp_body.clone()],
//                     t,
//                 });
//                 cast.append_to(semicolon, var_dec_list)?;
//                 cast.append_to(parent, semicolon)?;
//             }

//             //Compile conditional value
//             {
//                 let brackets = cast.new_node(CASTNode::Brackets);
//                 cast.append_to(parent, brackets)?;

//                 let condition = &mut recurse_child!(0, Some(tmp_conditional.clone()));
//                 cast.append_tree(brackets, condition)?;
//             }
//             //Compile if-statement
//             {
//                 let if_statement = cast.new_node(CASTNode::If);
//                 cast.append_to(parent, if_statement)?;

//                 //Append condition
//                 let condition = cast.new_node(CASTNode::VarRef(tmp_conditional.clone()));
//                 cast.append_to(if_statement, condition)?;

//                 //Append body
//                 let body = &mut recurse_child!(1, return_var.clone());
//                 cast.append_tree(if_statement, body)?;
//             }

//             // if let Some(n) = return_var {}
//         }
//         //Binary operations
//         ASTNodeType::Add
//         | ASTNodeType::Sub
//         | ASTNodeType::Mul
//         | ASTNodeType::Div
//         | ASTNodeType::Eq
//         | ASTNodeType::Ne
//         | ASTNodeType::Lt
//         | ASTNodeType::Gt
//         | ASTNodeType::Le
//         | ASTNodeType::Ge => {
//             let parent = cast.new_node(CASTNode::Ignore);
//             let t = Type::Int32;
//             let tmp_1 = generate_tmp();
//             let tmp_2 = generate_tmp();

//             //Create declarations for the temp variables
//             {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let var_dec_list = cast.new_node(CASTNode::VarDecList {
//                     names: vec![tmp_1.clone(), tmp_2.clone()],
//                     t,
//                 });
//                 cast.append_to(semicolon, var_dec_list)?;
//                 cast.append_to(parent, semicolon)?;
//             }
//             //Compile the `lhs`
//             {
//                 let brackets = cast.new_node(CASTNode::Brackets);
//                 cast.append_to(parent, brackets)?;

//                 let lhs = &mut recurse_child!(0, Some(tmp_1.clone()));
//                 cast.append_tree(brackets, lhs)?;
//             }
//             //Compile the `rhs`
//             {
//                 let brackets = cast.new_node(CASTNode::Brackets);
//                 cast.append_to(parent, brackets)?;

//                 let rhs = &mut recurse_child!(1, Some(tmp_2.clone()));
//                 cast.append_tree(brackets, rhs)?;
//             }
//             //Final assignment statement
//             {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let assignment = cast.new_node(CASTNode::Assign(return_var.unwrap()));
//                 cast.append_to(semicolon, assignment)?;
//                 cast.append_to(parent, semicolon)?;

//                 let op = {
//                     let op = match &ast[curr_ast].data.t {
//                         ASTNodeType::Add => CASTNode::Add,
//                         ASTNodeType::Sub => CASTNode::Sub,
//                         ASTNodeType::Mul => CASTNode::Mul,
//                         ASTNodeType::Div => CASTNode::Div,
//                         ASTNodeType::Eq => CASTNode::Eq,
//                         ASTNodeType::Ne => CASTNode::Ne,
//                         ASTNodeType::Lt => CASTNode::Lt,
//                         ASTNodeType::Gt => CASTNode::Gt,
//                         ASTNodeType::Le => CASTNode::Le,
//                         ASTNodeType::Ge => CASTNode::Ge,
//                         _ => unreachable!(),
//                     };
//                     cast.new_node(op)
//                 };
//                 cast.append_to(assignment, op)?;

//                 let tmp_1 = cast.new_node(CASTNode::VarRef(tmp_1));
//                 cast.append_to(op, tmp_1)?;
//                 let tmp_2 = cast.new_node(CASTNode::VarRef(tmp_2));
//                 cast.append_to(op, tmp_2)?;
//             }

//             // {t} {tmp_1}, {tmp_2};
//             // { {compile(lhs, tmp_1)} }
//             // { {compile(rhs, tmp_2)} }
//             // return_var = {tmp_1} + {tmp_2};
//             // todo!()
//         }
//         ASTNodeType::Assign { name } => {
//             if let Some(_return_var) = return_var.clone() {
//                 single_parent!(CASTNode::Assign(name.clone()))
//             } else {
//                 return Err(anyhow::format_err!(
//                     "An assignment occured with `return_var = None`"
//                 ));
//             }
//         }
//         //LastValueReturn:
//         //When parent is a method def OR there is no parent OR the parent is also a LastValueReturn, ignore
//         //When parent is an assignment, set final child to assignment and have the parent be brackets
//         //When parent is a VariableDef, replace `curr` with two nodes: a C::VariableDef and brackets (treated like an assignment would be)

//         // `return_var` is passed to only the final child. This is handled by `single_parent` by default
//         // When the children are method definitions, brackets would be syntatically invalid
//         ASTNodeType::LastValueReturn => match &ast[children[0]].data.t {
//             ASTNodeType::MethodDef { .. } | ASTNodeType::LastValueReturn => {
//                 single_parent_no_return!(CASTNode::Ignore)
//             }
//             _ => single_parent!(CASTNode::Brackets),
//         },
//         ASTNodeType::ValueConsume => match &ast[children[0]].data.t {
//             ASTNodeType::VariableDef { .. } => single_parent!(CASTNode::Ignore),
//             _ => single_parent!(CASTNode::Semicolon),
//         },
//     }

//     Ok(cast)
// }
