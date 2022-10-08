use std::{sync::Mutex, vec};

use crate::{
    ast::{ASTNode, ASTNodeType, Value},
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
}

impl std::fmt::Display for CASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            CASTNode::Ignore => format!("Ignore"),
            CASTNode::Literal(val) => format!("{val}"),
            CASTNode::MethodDef {
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
            CASTNode::Assign(name) => format!("Assign `{name}`"),

            CASTNode::VarDec { name, t } => format!("Declare `{} {name}`", cast_type_to_string(*t)),
            CASTNode::VarDecList { names, t } => {
                let list = names.join(", ");
                format!("DeclareList `{} {list}`", cast_type_to_string(*t))
            }
            CASTNode::Brackets => format!("Brackets"),
            CASTNode::Semicolon => format!("Semicolon"),
            CASTNode::Add => format!("Add"),
            CASTNode::Sub => format!("Sub"),
            CASTNode::Return => format!("Return"),
            CASTNode::VarRef(name) => format!("VarRef `{name}`"),
            CASTNode::VarDecAssign { name, t } => {
                format!("DeclareAssign `{} {name}`", cast_type_to_string(*t))
            }
            CASTNode::Mul => format!("Mul"),
            CASTNode::Div => format!("Div"),
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
        Type::Int => format!("long"),
        Type::Int32 => format!("int"),
        Type::String => todo!(),
    }
}

/// Takes in a `C` AST and outputs a string ready to be written to a `C` program file
pub fn cast_to_string(cast: &Tree<CASTNode>) -> String {
    // println!("==C_AST==\n{cast}\n=========");

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
        CASTNode::Ignore => join_all_children!("\n"),
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
            let body_string = join_all_children!("\n");
            format!("{return_type_string} {name}({inputs_string}) {{\n{body_string}\n}}")
        }
        //Basic bin ops
        CASTNode::Add => format!("({} + {})", child!(0), child!(1)),
        CASTNode::Sub => format!("({} - {})", child!(0), child!(1)),
        CASTNode::Mul => format!("({} * {})", child!(0), child!(1)),
        CASTNode::Div => format!("({} / {})", child!(0), child!(1)),
        //
        CASTNode::Semicolon => format!("{};", child!(0)),
        CASTNode::Brackets => format!("{{{}}}", join_all_children!("\n")),
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
    ast_to_cast_recurse(ast, ast.find_head().expect("Couldn't find AST head"), None)
}

/// * `ast` - The input AST for EmScript code
/// * `curr_ast` - The current node to be evaluated in `ast`
/// * `expr_tmp_vars` - The name of the variable which should be assigned to in this
fn ast_to_cast_recurse(
    ast: &Tree<ASTNode>,
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
                    let mut c_tree = ast_to_cast_recurse(ast, c, None)?;
                    cast.append_tree(n, &mut c_tree)?;
                }
                let last_child = children.last().unwrap();
                let mut c_tree = ast_to_cast_recurse(ast, *last_child, return_var)?;
                cast.append_tree(n, &mut c_tree)?;
            }
        }};
    }

    macro_rules! single_parent_no_return {
        ($child:expr) => {{
            let n = cast.new_node($child);
            for i in 0..children.len() {
                let c = children[i];
                let mut c_tree = ast_to_cast_recurse(ast, c, None)?;
                cast.append_tree(n, &mut c_tree)?;
            }
        }};
    }

    /// * `child_index`
    /// * `return_var`
    macro_rules! recurse_child {
        ($child_index:expr, $return_var:expr) => {
            ast_to_cast_recurse(ast, children[$child_index], $return_var)?
        };
    }

    match &ast[curr_ast].data.t {
        ASTNodeType::Literal { val } => {
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
        ASTNodeType::VariableRef { name } => {
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
        ASTNodeType::VariableDef { name } => {
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
                    "An assignment [VarDef] occured with `return_var = Some(_)`, when assignment returns void"
                ));
            }
        }
        ASTNodeType::MethodDef {
            name,
            inputs,
            return_type,
        } => {
            let parent = cast.new_node(CASTNode::MethodDef {
                name: name.clone(),
                inputs: inputs.clone(),
                return_type: return_type.clone(),
            });
            //Create a remporary variable to hold the return value
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
        ASTNodeType::MethodCall { name } => todo!(),
        ASTNodeType::IfCondition => todo!(),
        ASTNodeType::Add | ASTNodeType::Sub | ASTNodeType::Mul | ASTNodeType::Div => {
            let parent = cast.new_node(CASTNode::Ignore);
            let t = Type::Int32;
            let tmp_1 = generate_tmp();
            let tmp_2 = generate_tmp();

            //Create the declarations for the tmp vars
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
            //The final assignment statement
            {
                let semicolon = cast.new_node(CASTNode::Semicolon);
                let assignment = cast.new_node(CASTNode::Assign(return_var.unwrap()));
                cast.append_to(semicolon, assignment)?;
                cast.append_to(parent, semicolon)?;

                let op = {
                    let op = match &ast[curr_ast].data.t {
                        ASTNodeType::Add => CASTNode::Add,
                        ASTNodeType::Sub => CASTNode::Sub,
                        ASTNodeType::Mul => CASTNode::Mul,
                        ASTNodeType::Div => CASTNode::Div,
                        ASTNodeType::Eq => todo!(),
                        ASTNodeType::Ne => todo!(),
                        ASTNodeType::Lt => todo!(),
                        ASTNodeType::Gt => todo!(),
                        ASTNodeType::Le => todo!(),
                        ASTNodeType::Ge => todo!(),
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
        ASTNodeType::Mul => todo!(),
        ASTNodeType::Div => todo!(),
        ASTNodeType::Eq => todo!(),
        ASTNodeType::Ne => todo!(),
        ASTNodeType::Lt => todo!(),
        ASTNodeType::Gt => todo!(),
        ASTNodeType::Le => todo!(),
        ASTNodeType::Ge => todo!(),
        ASTNodeType::Assign { name } => {
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
        ASTNodeType::LastValueReturn => match &ast[children[0]].data.t {
            ASTNodeType::MethodDef { .. } | ASTNodeType::LastValueReturn => {
                single_parent_no_return!(CASTNode::Ignore)
            }
            _ => single_parent!(CASTNode::Brackets),
        },
        ASTNodeType::ValueConsume => match &ast[children[0]].data.t {
            ASTNodeType::VariableDef { .. } => single_parent!(CASTNode::Ignore),
            _ => single_parent!(CASTNode::Semicolon),
        },
    }

    Ok(cast)
}
