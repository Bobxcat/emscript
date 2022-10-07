use crate::{
    ast::{ASTNode, ASTNodeType, Value},
    tree::{NodeId, Tree},
    verify::Type,
};

#[derive(Debug, Clone)]
pub enum CASTNode {
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
}

impl std::fmt::Display for CASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var = (CASTNode::Add, CASTNode::Brackets);

        match var {
            (CASTNode::Add, CASTNode::Brackets) => println!("hi"),
            (CASTNode::Add, CASTNode::Sub) => println!("Also"),
            _ => (),
        }

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
            CASTNode::Brackets => format!("Brackets"),
            CASTNode::Semicolon => format!("Semicolon"),
            CASTNode::Add => format!("Add"),
            CASTNode::Sub => format!("Sub"),
            CASTNode::Return => format!("Return"),
            CASTNode::VarRef(name) => format!("VarRef `{name}`"),
            CASTNode::VarDecAssign { name, t } => {
                format!("DeclareAssign `{} {name}`", cast_type_to_string(*t))
            }
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
    println!("==C_AST==\n{cast}\n=========");

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
        CASTNode::Add => format!("({} + {})", child!(0), child!(1)),
        CASTNode::Sub => format!("({} - {})", child!(0), child!(1)),
        CASTNode::Semicolon => format!("{};", child!(0)),
        CASTNode::Brackets => format!("{{{}}}", join_all_children!("\n")),
        CASTNode::Assign(name) => format!("{name} = {}", child!(0)),
        CASTNode::VarDec { name, t } => format!("{} {name}", cast_type_to_string(*t)),
        CASTNode::Return => format!("return {};", child!(0)),
        CASTNode::VarRef(name) => format!("{name}"),
        CASTNode::VarDecAssign { name, t } => {
            format!("{} {name} = {}", cast_type_to_string(*t), child!(0))
        }
    }
}

pub fn ast_to_cast(ast: &Tree<ASTNode>) -> anyhow::Result<Tree<CASTNode>> {
    ast_to_cast_recurse(ast, ast.find_head().expect("Couldn't find AST head"))
}

fn ast_to_cast_recurse(ast: &Tree<ASTNode>, curr_ast: NodeId) -> anyhow::Result<Tree<CASTNode>> {
    let children = ast[curr_ast].children.clone();

    let mut cast = Tree::new();

    /// Shorthand for adding a node to `cast` and appending all children of `curr_ast` to it
    macro_rules! single_parent {
        ($child:expr) => {{
            let n = cast.new_node($child);
            append_children_to!(n);
            // for c in children {
            //     let mut c_tree = ast_to_cast_recurse(ast, c);
            //     cast.append_tree(n, &mut c_tree).unwrap();
            // }
        }};
    }

    /// Gets each child's subtree and appends it to `cast` with the given node as the parent
    macro_rules! append_children_to {
        ($parent:expr) => {
            for c in children {
                let mut c_tree = ast_to_cast_recurse(ast, c)?;
                cast.append_tree($parent, &mut c_tree)?;
            }
        };
    }

    match &ast[curr_ast].data.t {
        ASTNodeType::Literal { val } => single_parent!(CASTNode::Literal(val.clone())),
        ASTNodeType::VariableRef { name } => single_parent!(CASTNode::VarRef(name.clone())),
        ASTNodeType::VariableDef { name } => {
            if let ASTNodeType::LastValueReturn = ast[children[0]].data.t {
                single_parent!(CASTNode::Ignore)
            } else {
                single_parent!(CASTNode::VarDecAssign {
                    name: name.clone(),
                    t: Type::Int32
                })
            }
        }
        ASTNodeType::MethodDef {
            name,
            inputs,
            return_type,
        } => single_parent!(CASTNode::MethodDef {
            name: name.clone(),
            inputs: inputs.clone(),
            return_type: return_type.clone(),
        }),
        ASTNodeType::MethodCall { name } => todo!(),
        ASTNodeType::IfCondition => todo!(),
        ASTNodeType::Add => single_parent!(CASTNode::Add),
        ASTNodeType::Sub => single_parent!(CASTNode::Sub),
        ASTNodeType::Mul => todo!(),
        ASTNodeType::Div => todo!(),
        ASTNodeType::Eq => todo!(),
        ASTNodeType::Ne => todo!(),
        ASTNodeType::Lt => todo!(),
        ASTNodeType::Gt => todo!(),
        ASTNodeType::Le => todo!(),
        ASTNodeType::Ge => todo!(),
        ASTNodeType::Assign { name } => {
            if let ASTNodeType::LastValueReturn = ast[children[0]].data.t {
                single_parent!(CASTNode::Ignore)
            } else {
                single_parent!(CASTNode::Assign(name.clone()))
            }
        }
        //LastValueReturn:
        //When parent is a method def OR there is no parent OR the parent is also a LastValueReturn, ignore
        //When parent is an assignment, set final child to assignment and have the parent be brackets
        //When parent is a VariableDef, replace `curr` with two nodes: a C::VariableDef and brackets (treated like an assignment would be)
        ASTNodeType::LastValueReturn => {
            macro_rules! assign {
                ($name:expr) => {{
                    let last_child = children.last().unwrap();
                    //The brackets themselves, to which all children are appended
                    let brackets = cast.new_node(CASTNode::Brackets);
                    //Every child except the last one is treated as normal
                    for c in 0..(children.len() - 1) {
                        let mut c = ast_to_cast_recurse(ast, children[c])?;
                        cast.append_tree(brackets, &mut c)?;
                    }
                    //The last child becomes an assignment
                    {
                        let mut c_rhs = ast_to_cast_recurse(ast, *last_child)?;
                        let c_assignment = cast.new_node(CASTNode::Assign($name.clone()));
                        let c_semicolon = cast.new_node(CASTNode::Semicolon);
                        cast.append_to(brackets, c_semicolon)?;
                        cast.append_to(c_semicolon, c_assignment)?;
                        cast.append_tree(c_assignment, &mut c_rhs)?;
                    }

                    brackets
                }};
            }
            if let Some(parent) = ast[curr_ast].parent {
                match &ast[parent].data.t {
                    //Cases where a temp variable must be used to transmit the information
                    ASTNodeType::MethodCall { .. }
                    | ASTNodeType::IfCondition
                    | ASTNodeType::Add
                    | ASTNodeType::Sub
                    | ASTNodeType::Mul
                    | ASTNodeType::Div
                    | ASTNodeType::Eq
                    | ASTNodeType::Ne
                    | ASTNodeType::Lt
                    | ASTNodeType::Gt
                    | ASTNodeType::Le
                    | ASTNodeType::ValueConsume
                    | ASTNodeType::Ge => todo!(),
                    //Assignments and such
                    //For Assignments, if a variable is declared with a name matching `name` for the parent assignment, its name must be scrambled in such a way to be unique
                    //For example: `let a = { let a = 5; a };` *cannot* become `int a; {int a = 5; a = a;}`
                    //Instead, it should become something like `int a; { int _a = 5; a = _a; }`
                    //Another example: `let a = { let a = { let a = 3; a}; a};` -> `int a; { int _a; { int __a = 3; _a = __a; } a = _a; }`
                    ASTNodeType::Assign { name } => {
                        let _brackets = assign!(name);
                    }
                    //When the parent is `VariableDef`, it's treated like an `Assign` except that there is an additional `VarDec` placed before the `Brackets`
                    ASTNodeType::VariableDef { name } => {
                        //Every tree needs exactly 1 parent, so `parent` will be that parent
                        let parent = cast.new_node(CASTNode::Ignore);

                        //The VarDec coming before the brackets
                        let var_dec = cast.new_node(CASTNode::VarDec {
                            name: name.clone(),
                            t: Type::Int32,
                        });

                        let var_dec_semicolon = cast.new_node(CASTNode::Semicolon);

                        let assign_brackets = assign!(name);

                        cast.append_to(var_dec_semicolon, var_dec)?;
                        cast.append_to(parent, var_dec_semicolon)?;
                        cast.append_to(parent, assign_brackets)?;
                    }
                    //Ignored or almost ignored
                    //For a MethodDef, the last child must be a `Return`. However, brackets are not needed
                    ASTNodeType::MethodDef { .. } => {
                        let last_child = children.last().unwrap();
                        //The parent to all children
                        let parent = cast.new_node(CASTNode::Ignore);
                        //Every child except the last one is treated as normal
                        for c in 0..(children.len() - 1) {
                            let mut c = ast_to_cast_recurse(ast, children[c])?;
                            cast.append_tree(parent, &mut c)?;
                        }
                        //The last child becomes a return statement
                        {
                            let mut c_rhs = ast_to_cast_recurse(ast, *last_child)?;
                            let c_return = cast.new_node(CASTNode::Return);
                            cast.append_to(parent, c_return)?;
                            cast.append_tree(c_return, &mut c_rhs)?;
                        }
                    }
                    ASTNodeType::LastValueReturn => {
                        single_parent!(CASTNode::Ignore);
                        // let parent = cast.new_node(CASTNode::Ignore);
                        // append_children_to!(parent)
                    }
                    _ => panic!(
                        "LastValueReturn has invalid parent type: {:#?}",
                        &ast[parent].data.t.clone()
                    ),
                }
            } else {
                single_parent!(CASTNode::Ignore);
            }
        }
        ASTNodeType::ValueConsume => single_parent!(CASTNode::Semicolon),
    }

    Ok(cast)
}
