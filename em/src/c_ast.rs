// use std::{
//     backtrace::Backtrace,
//     collections::{HashMap, HashSet},
//     mem::size_of,
//     sync::Mutex,
//     vec,
// };

// use once_cell::sync::Lazy;

// use crate::{
//     ast::{ASTNode, ASTNodeType},
//     interface::Interface,
//     ir::{IRNode, IRNodeType, IdentInfo, IRAST},
//     tree::{NodeId, Tree},
//     utils::{format_compact, PREFIX_TMP},
//     value::{
//         custom_types::{custom_types, str_to_type},
//         Type, Value,
//     },
// };

// static C_TMP_VAR_COUNT: Lazy<Mutex<u128>> = Lazy::new(|| Mutex::new(0));

// /// Generates a globally unique tmp variable name
// fn generate_tmp() -> String {
//     let mut count = C_TMP_VAR_COUNT.lock().unwrap();
//     //Prefix is the compile time constant `utils::PREFIX_TMP`
//     let s = format!("{PREFIX_TMP}{}", format_compact(*count));
//     *count += 1;
//     s
// }

// #[derive(Debug, Clone)]
// pub enum CASTNode {
//     /// This node is ignored in evaluation.
//     /// Can be used, for example, to have a sequence of statements without enclosing brackets or parens
//     Ignore,
//     Literal(Value),
//     MethodDef {
//         name: String,
//         inputs: Vec<(Type, String)>,
//         return_type: Type,
//     },
//     ExternMethodDef {
//         mod_name: String,
//         imp_name: String,
//         name: String,
//         inputs: Vec<(Type, String)>,
//         return_type: Type,
//     },
//     /// Represents a struct definition
//     StructDef {
//         name: String,
//         fields: Vec<(Type, String)>,
//     },
//     StructDec {
//         name: String,
//     },
//     /// `{name}({input1}, {input2}, ..)`
//     MethodCall {
//         name: String,
//         inputs: Vec<String>,
//     },
//     /// `& {child}`
//     Reference,
//     /// Represents an if statement
//     ///
//     /// `2` children, first is conditional, second is body
//     If,
//     /// Represents an assignment of a variable with the given name to the value returned by this node's child
//     ///
//     /// `1` child
//     Assign(String),
//     /// Represents a variable being declared, but *not* assigned, i.e.:
//     /// `{type} {name}`. For example, `int my_num`
//     ///
//     /// `0` children
//     VarDec {
//         name: String,
//         t: Type,
//     },
//     /// Represents a comma-seperated list of variable declarations, with no assignments. For example:
//     /// `{type} {name0}, {name1}, {name2};`
//     ///
//     /// `0` children
//     VarDecList {
//         names: Vec<String>,
//         t: Type,
//     },
//     /// Represents a variable being declared and assigned on one line, i.e.:
//     /// `{type} {name} = {child}`. For example, `int my_num = (3 + (2-1))`
//     ///
//     /// `1` child
//     VarDecAssign {
//         name: String,
//         t: Type,
//     },
//     /// Represents a variable name in an expression
//     ///
//     /// `0` children
//     VarRef(String),
//     /// Represents a reference to a field, with the child being the object the field belongs to
//     ///
//     /// `1` child
//     FieldRef(String),
//     /// `return {child};`
//     ///
//     /// `1` child
//     Return,
//     /// Represents putting a sequence of statements in curly braces, i.e.:
//     /// `{{child_0}\n{child_1}\n...}`
//     ///
//     /// `1+` children
//     Brackets,
//     /// `{child};`
//     ///
//     /// `1` child
//     Semicolon,
//     //Basic ops
//     Add,
//     Sub,
//     Mul,
//     Div,
//     Eq,
//     Ne,
//     Lt,
//     Gt,
//     Le,
//     Ge,
// }

// impl std::fmt::Display for CASTNode {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         use CASTNode::*;
//         let s = match self {
//             Ignore => format!("Ignore"),
//             Literal(val) => format!("{val}"),
//             MethodDef {
//                 name,
//                 inputs,
//                 return_type,
//             } => {
//                 let inputs_string = inputs
//                     .clone()
//                     .into_iter()
//                     .map(|(t, name)| format!("{} {name}", cast_type_to_string(t)))
//                     .collect::<Vec<_>>()
//                     .join(", ");
//                 format!(
//                     "{} {name}({inputs_string})",
//                     cast_type_to_string(return_type.clone())
//                 )
//             }
//             ExternMethodDef {
//                 mod_name,
//                 imp_name,
//                 name,
//                 inputs,
//                 return_type,
//             } => {
//                 let inputs_string = inputs
//                     .clone()
//                     .into_iter()
//                     .map(|(t, name)| format!("{} {name}", cast_type_to_string(t)))
//                     .collect::<Vec<_>>()
//                     .join(", ");
//                 format!(
//                     "[mod_name: {mod_name}, imp_name: {imp_name}]extern {} {name}({inputs_string})",
//                     cast_type_to_string(return_type.clone())
//                 )
//             }
//             StructDef { name, fields } => {
//                 todo!("TODO: implement `Display` for `CASTNode::StructDef`");
//             }
//             StructDec { name } => format!("StructDec: {name}"),

//             MethodCall { name, inputs } => format!("MethodCall: `{name}({})`", inputs.join(", ")),
//             Reference => format!("Ref"),

//             Assign(name) => format!("Assign `{name}`"),
//             If => format!("If"),

//             VarDec { name, t } => format!("Declare `{} {name}`", cast_type_to_string(t.clone())),
//             VarDecList { names, t } => {
//                 let list = names.join(", ");
//                 format!("DeclareList `{} {list}`", cast_type_to_string(t.clone()))
//             }
//             Brackets => format!("Brackets"),
//             Semicolon => format!("Semicolon"),
//             Add => format!("Add"),
//             Sub => format!("Sub"),
//             Return => format!("Return"),
//             VarRef(name) => format!("VarRef `{name}`"),
//             FieldRef(name) => format!("FieldRef `{name}`"),
//             VarDecAssign { name, t } => {
//                 format!("DeclareAssign `{} {name}`", cast_type_to_string(t.clone()))
//             }
//             Mul => format!("Mul"),
//             Div => format!("Div"),
//             Eq => format!("Eq"),
//             Ne => format!("Ne"),
//             Lt => format!("Lt"),
//             Gt => format!("Gt"),
//             Le => format!("Le"),
//             Ge => format!("Ge"),
//         };
//         write!(f, "{s}")
//     }
// }

// fn cast_literal_to_string(val: &Value) -> String {
//     match val {
//         Value::Void => format!("void"),
//         Value::Bool(v) => format!("{v}"),
//         Value::Int(v) => format!("{v}"),
//         Value::Int32(v) => format!("{v}"),
//         Value::String(_) => todo!(),
//     }
// }

// fn cast_type_to_string(t: Type) -> String {
//     match t {
//         Type::Void => format!("void"),
//         Type::Bool => format!("bool"),
//         Type::Int => {
//             println!(
//                 "Warning: `Type::Int` was passed to `cast_type_to_string`. This should be avoided but will default to `long`"
//             );
//             format!("long")
//         }
//         Type::Int32 => format!("int"),

//         //:(
//         Type::Custom(id) => custom_types()
//             .get_k(&id)
//             .unwrap()
//             .mangled_name
//             .as_ref()
//             .unwrap()
//             .clone(),
//         Type::Ref(t) => format!("{} *", cast_type_to_string(*t)),
//     }
// }

// /// Takes in a `C` AST and outputs a string ready to be written to a `C` program file
// pub fn cast_to_string(cast: &Tree<CASTNode>) -> String {
//     //The items to be included at the top of the c file
//     const INCLUDES: &'static [&str] = &["<stdbool.h>"];
//     let includes_string = INCLUDES
//         .clone()
//         .into_iter()
//         .map(|s| format!("#include {s}\n"))
//         .collect::<Vec<_>>()
//         .join("");
//     let body_string =
//         cast_to_string_recurse(cast, cast.find_head().expect("Couldn't find AST head"));

//     format!("{includes_string}\n{body_string}")
// }

// /// The recursive backend of `cast_to_string`. Some notes:
// /// - To reduce the size of the output string, spaces and newlines are used minimally
// fn cast_to_string_recurse(cast: &Tree<CASTNode>, curr: NodeId) -> String {
//     use CASTNode::*;
//     let children = cast[curr].children.clone();

//     /// Recursively calls `cast_to_string_recurse` on the provided node (given an index in `children`)
//     macro_rules! child {
//         ($idx:expr) => {
//             cast_to_string_recurse(cast, children[$idx])
//         };
//     }
//     /// Recursively calls `cast_to_string_recurse` on the provided node (given a `NodeId` in `cast`)
//     macro_rules! child_id {
//         ($idx:expr) => {
//             cast_to_string_recurse(cast, c)
//         };
//     }

//     /// Recursively calls `cast_to_string_recurse` on each child and joins their outputted strings with the given seperator
//     macro_rules! join_all_children {
//         ($sep:expr) => {
//             children
//                 .into_iter()
//                 .map(|c| cast_to_string_recurse(cast, c))
//                 .collect::<Vec<_>>()
//                 .join($sep)
//         };
//     }

//     match &cast[curr].data {
//         Ignore => join_all_children!(""), //\n
//         Literal(val) => cast_literal_to_string(val),
//         MethodDef {
//             name,
//             inputs,
//             return_type,
//         } => {
//             let inputs_string = inputs
//                 .into_iter()
//                 .map(|(t, name)| format!("{} {name}", cast_type_to_string(t.clone())))
//                 .collect::<Vec<_>>()
//                 .join(",");
//             let return_type_string = cast_type_to_string(return_type.clone());
//             let body_string = join_all_children!(""); //\n
//             format!("{return_type_string} {name}({inputs_string}) {{\n{body_string}\n}}")
//         }
//         ExternMethodDef {
//             mod_name,
//             imp_name,
//             name,
//             inputs,
//             return_type,
//         } => {
//             let inputs_string = inputs
//                 .into_iter()
//                 .map(|(t, name)| format!("{} {name}", cast_type_to_string(t.clone())))
//                 .collect::<Vec<_>>()
//                 .join(", ");
//             let return_type_string = cast_type_to_string(return_type.clone());
//             format!("__attribute__((import_module(\"{mod_name}\"), import_name(\"{imp_name}\"))) extern {return_type_string} {name}({inputs_string});")
//         }
//         StructDef { name, fields } => {
//             let fields_string = fields
//                 .into_iter()
//                 .map(|(t, name)| format!("{} {name};", cast_type_to_string(t.clone())))
//                 .collect::<Vec<_>>()
//                 .join("");
//             format!("typedef struct {{{fields_string}}} {name};")
//         }
//         StructDec { name } => format!("typedef struct {name};"),
//         MethodCall { name, inputs } => format!("{name}({})", inputs.join(",")),
//         Reference => format!("&({})", child!(0)),
//         If => format!("if ({}) {{{}}}", child!(0), child!(1)),

//         //Basic bin ops
//         Add => format!("({}+{})", child!(0), child!(1)),
//         Sub => format!("({}-{})", child!(0), child!(1)),
//         Mul => format!("({}*{})", child!(0), child!(1)),
//         Div => format!("({}/{})", child!(0), child!(1)),
//         Eq => format!("({}=={})", child!(0), child!(1)),
//         Ne => format!("({}!={})", child!(0), child!(1)),
//         Gt => format!("({}>{})", child!(0), child!(1)),
//         Lt => format!("({}<{})", child!(0), child!(1)),
//         Ge => format!("({}>={})", child!(0), child!(1)),
//         Le => format!("({}<={})", child!(0), child!(1)),

//         //Misc
//         Semicolon => format!("{};", child!(0)),
//         Brackets => format!("{{{}}}", join_all_children!("")), //\n
//         Assign(name) => format!("{name} = {}", child!(0)),
//         VarDec { name, t } => format!("{} {name}", cast_type_to_string(t.clone())),
//         VarDecList { names, t } => {
//             let list = names.join(", ");
//             format!("{} {list}", cast_type_to_string(t.clone()))
//         }
//         Return => format!("return {};", child!(0)),
//         VarRef(name) => format!("{name}"),
//         FieldRef(name) => format!("{}.{name}", child!(0)),
//         VarDecAssign { name, t } => {
//             format!("{} {name} = {}", cast_type_to_string(t.clone()), child!(0))
//         }
//     }
// }

// pub fn ast_to_cast(ast: &Tree<ASTNode>, interface: &Interface) -> anyhow::Result<Tree<CASTNode>> {
//     let mut ir_ast = IRAST::from_ast(ast, &interface)?;
//     ir_ast.mangle(vec!["hello", "fibonacci"].into_iter().collect());

//     let mut used_wasm_imports = HashSet::new();

//     let mut ast = ir_ast_to_cast_recurse(
//         &ir_ast,
//         ast.find_head().expect("Couldn't find AST head"),
//         None,
//         &mut used_wasm_imports,
//     )?;

//     let head = ast.find_head().unwrap();

//     //NOTE:
//     //Since the following operations are **PREPENDING** (not appending), earlier additions occur later in the file

//     //Add all the `extern` method declarations after type declarations and definitions
//     {
//         //Note: this same code exists in `IRAST::from_ast`
//         for (name, mangled) in used_wasm_imports {
//             let imp = &interface.wasm_imports[&name];

//             let mut param_idx = 0;
//             let method_dec = ast.new_node(CASTNode::ExternMethodDef {
//                 mod_name: imp.mod_name.clone(),
//                 imp_name: name,
//                 name: mangled,
//                 inputs: imp
//                     .params
//                     .iter()
//                     .map(|t| {
//                         param_idx += 1;
//                         (t.clone(), format!("_{param_idx}"))
//                     })
//                     .collect(),
//                 return_type: imp.ret.clone(),
//             });
//             ast.prepend_to(head, method_dec)?;
//         }
//     }

//     //Add all custom type definitions after their declarations
//     {
//         let custom_types = custom_types();
//         for (_id, t) in custom_types.iter_k() {
//             let struct_def = ast.new_node(CASTNode::StructDef {
//                 name: t
//                     .mangled_name
//                     .clone()
//                     .expect(&format!("a custom type's name was not mangled: {t:#?}")),
//                 fields: t
//                     .fields
//                     .iter()
//                     .map(|(name, t)| (t.clone(), name.clone()))
//                     .collect(),
//             });
//             ast.prepend_to(head, struct_def)?;
//         }
//     }

//     //Add all custom type declarations
//     {
//         let custom_types = custom_types();
//         for (_, t) in custom_types.iter_k() {
//             let struct_def = ast.new_node(CASTNode::StructDec {
//                 name: t
//                     .mangled_name
//                     .clone()
//                     .expect(&format!("a custom type's name was not mangled: {t:#?}")),
//             });
//             ast.prepend_to(head, struct_def)?;
//         }
//     }

//     Ok(ast)
// }

// // /// Represents a
// // pub struct UsedExport {
// //     pub name: String,
// //     pub mangled_name: String,
// //     pub params: Vec<Type>,
// //     pub ret: Type,
// // }

// /// * `ast` - The input AST for EmScript code
// /// * `curr_ast` - The current node to be evaluated in `ast`
// /// * `expr_tmp_vars` - The name of the variable which should be assigned to in this
// /// * `used_wasm_imports` - Denotes which `wasm_imports` have been used from the interface.
// ///     The first of each tuple is the original method name, the latter is the mangled name
// ///
// /// TODO: Compile to WASM directly
// fn ir_ast_to_cast_recurse(
//     ast: &IRAST,
//     curr_ast: NodeId,
//     return_var: Option<String>,
//     used_wasm_imports: &mut HashSet<(String, String)>,
// ) -> anyhow::Result<Tree<CASTNode>> {
//     let children = ast[curr_ast].children.clone();

//     let mut cast = Tree::new();
//     /// Shorthand for adding a node to `cast` and appending all children of `curr_ast` to it
//     ///
//     /// `return_var` will be passed *only* to the last child. All others will receive `None`
//     macro_rules! single_parent {
//         ($child:expr) => {{
//             let n = cast.new_node($child);
//             if !children.is_empty() {
//                 for i in 0..children.len() - 1 {
//                     let c = children[i];
//                     let mut c_tree = ir_ast_to_cast_recurse(ast, c, None, used_wasm_imports)?;
//                     cast.append_tree(n, &mut c_tree)?;
//                 }
//                 let last_child = children.last().unwrap();
//                 let mut c_tree =
//                     ir_ast_to_cast_recurse(ast, *last_child, return_var, used_wasm_imports)?;
//                 cast.append_tree(n, &mut c_tree)?;
//             }
//         }};
//     }

//     macro_rules! single_parent_no_return {
//         ($child:expr) => {{
//             let n = cast.new_node($child);
//             for i in 0..children.len() {
//                 let c = children[i];
//                 let mut c_tree = ir_ast_to_cast_recurse(ast, c, None, used_wasm_imports)?;
//                 cast.append_tree(n, &mut c_tree)?;
//             }
//         }};
//     }

//     /// * `child_index`
//     /// * `return_var`
//     macro_rules! recurse_child {
//         ($child_index:expr, $return_var:expr) => {{
//             ir_ast_to_cast_recurse(ast, children[$child_index], $return_var, used_wasm_imports)?
//         }};
//     }

//     /// Wraps the inputted code to a local method with proper parameters and calls said method
//     macro_rules! branch_function {
//         {$($toks:tt)*} => {{
//             let ____branch = move ||-> anyhow::Result<Tree<CASTNode>>  {
//                 {$($toks)*}
//                 Ok(cast)
//             };

//             (____branch)()
//         }};
//     }

//     match &ast[curr_ast].data.t {
//         IRNodeType::Literal(val) => branch_function! {
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
//         },

//         IRNodeType::VarRef(id) => branch_function! {
//             let name = ast[*id].name();
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
//         },
//         //Note that `FieldRef` is treated almost exactly like `VarRef`
//         IRNodeType::FieldRef(field_name) => branch_function! {
//             //The expansion for `FieldRef` is as follows:

//             // let tmp;
//             // { recurse_child!(0, tmp) }
//             //
//             // return_var = tmp.field_name;

//             if let Some(return_var) = return_var {
//                 let parent = cast.new_node(CASTNode::Ignore);

//                 //Declare tmp variable and use `recurse_child!` to assign it:
//                 let tmp = generate_tmp();
//                 let tmp_t = str_to_type("Foo").unwrap();
//                 {
//                     //tmp declaration
//                     let semicolon = cast.new_node(CASTNode::Semicolon);
//                     let tmp_dec = cast.new_node(CASTNode::VarDec {
//                         name: tmp.clone(),
//                         t: tmp_t.clone(),
//                     });
//                     cast.append_to(semicolon, tmp_dec)?;
//                     cast.append_to(parent, semicolon)?;

//                     //tmp assignment
//                     let tmp_subtree = &mut recurse_child!(0, Some(tmp.clone()));
//                     cast.append_tree(parent, tmp_subtree)?;
//                 }

//                 //Assign `return_var = tmp.field_name`
//                 {
//                     let semicolon = cast.new_node(CASTNode::Semicolon);
//                     let assign = cast.new_node(CASTNode::Assign(return_var.clone()));
//                     let rhs = cast.new_node(CASTNode::FieldRef(field_name.clone()));
//                     let rhs_var = cast.new_node(CASTNode::VarRef(tmp.clone()));

//                     cast.append_to(rhs, rhs_var)?;
//                     cast.append_to(assign, rhs)?;
//                     cast.append_to(semicolon, assign)?;
//                     cast.append_to(parent, semicolon)?;
//                 }

//                 // let semicolon = cast.new_node(CASTNode::Semicolon);
//                 // let assign = cast.new_node(CASTNode::Assign(return_var.clone()));
//                 // let rhs = cast.new_node(CASTNode::VarRef(name.clone()));
//                 // cast.append_to(assign, rhs)?;
//                 // cast.append_to(semicolon, assign)?;
//                 // todo!()
//             } else {
//                 single_parent!(CASTNode::FieldRef(field_name.clone()))
//             }
//         },
//         IRNodeType::Reference => branch_function! {
//             //If child is a VarRef:
//             //  return_var = &{child};

//             //If child is not a VarRef:
//             //  let tmp;
//             //  { recurse_child!(0, tmp) }
//             //  return_var = &tmp;

//             let parent = cast.new_node(CASTNode::Ignore);
//             let return_var_rhs_name; //Either `_tmp{..}` or `{child_name}`

//             if let IRNodeType::VarRef(child_name) = ast[children[0]].data.t {
//                 return_var_rhs_name = ast[child_name].name().clone();
//             } else {
//                 let tmp = generate_tmp();
//                 let tmp_t = str_to_type("Foo").unwrap();

//                 //`let tmp;`
//                 {
//                     let semicolon = cast.new_node(CASTNode::Semicolon);
//                     let var_dec = cast.new_node(CASTNode::VarDec {
//                         name: tmp.clone(),
//                         t: tmp_t.clone(),
//                     });
//                     cast.append_to(semicolon, var_dec)?;
//                     cast.append_to(parent, semicolon)?;
//                 }

//                 //`{ recurse_child!(..) }`
//                 {
//                     let brackets = cast.new_node(CASTNode::Brackets);
//                     let subtree = &mut recurse_child!(0, Some(tmp.clone()));
//                     cast.append_tree(brackets, subtree)?;
//                     cast.append_to(parent, brackets)?;
//                 }

//                 return_var_rhs_name = tmp;
//             }

//             //`return_var = &{return_var_rhs_name};`
//             if let Some(return_var) = return_var {
//                 let assign = cast.new_node(CASTNode::Assign(return_var));
//                 let semicolon = cast.new_node(CASTNode::Semicolon);

//                 let rhs = cast.new_node(CASTNode::Reference);
//                 let rhs_var = cast.new_node(CASTNode::VarRef(return_var_rhs_name));

//                 cast.append_to(rhs, rhs_var)?;
//                 cast.append_to(assign, rhs)?;
//                 cast.append_to(semicolon, assign)?;
//                 cast.append_to(parent, semicolon)?;
//             }
//             // let tmp = generate_tmp();
//             // let tmp_t = str_to_type("Foo").unwrap();

//             // //`let tmp;`
//             // {
//             //     let semicolon = cast.new_node(CASTNode::Semicolon);
//             //     let var_dec = cast.new_node(CASTNode::VarDec {
//             //         name: tmp.clone(),
//             //         t: tmp_t.clone(),
//             //     });
//             //     cast.append_to(semicolon, var_dec)?;
//             //     cast.append_to(parent, semicolon)?;
//             // }

//             // //`{ recurse_child!(..) }`
//             // {
//             //     let brackets = cast.new_node(CASTNode::Brackets);
//             //     let subtree = &mut recurse_child!(0, Some(tmp.clone()));
//             //     cast.append_tree(brackets, subtree)?;
//             //     cast.append_to(parent, brackets)?;
//             // }

//         },
//         IRNodeType::VarDef(id) => branch_function! {
//             let (t, name) = ast[*id].name_and_return_type();
//             if let None = &return_var {
//                 let parent = cast.new_node(CASTNode::Ignore);

//                 //Create a variable declaration
//                 {
//                     let semicolon = cast.new_node(CASTNode::Semicolon);
//                     let var_dec = cast.new_node(CASTNode::VarDec {
//                         name: name.clone(),
//                         t,
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
//         },
//         //Assignments are just variable definitions without the declaration
//         IRNodeType::Assign(id) => branch_function! {
//             let name = ast[*id].name();

//             if let None = &return_var {
//                 let parent = cast.new_node(CASTNode::Ignore);

//                 //Compile the rhs with `name` as return_var
//                 {
//                     let brackets = cast.new_node(CASTNode::Brackets);
//                     let mut rhs = recurse_child!(0, Some(name.clone()));

//                     cast.append_tree(brackets, &mut rhs)?;
//                     cast.append_to(parent, brackets)?;
//                 }
//             } else {
//                 return Err(anyhow::format_err!(
//                     "An assignment [Assign] occured with `return_var = {return_var:?}`, when assignment returns void"
//                 ));
//             }
//         },
//         IRNodeType::MethodDef(id) => branch_function! {
//             // Gather the method's information
//             let (name, inputs, return_type) = {
//                 let method_info = &ast[*id];

//                 if let IdentInfo::Method {
//                     name,
//                     params,
//                     return_type,
//                 } = method_info
//                 {
//                     (name, params, return_type.clone())
//                 } else {
//                     return Err(anyhow::format_err!(
//                     "Should never happen: attempted to parse `MethodDef` whose identifier referred to a `Var`, not `Method` ({})",
//                     method_info.name()
//                     ));
//                 }
//             };
//             //Create the parent, which will be a `MethodDef` node
//             let inputs: Vec<_> = inputs
//                 .iter()
//                 .map(|id| {
//                     let (t, s) = ast[*id].name_and_return_type();
//                     (t, s.clone())
//                 })
//                 .collect();
//             let parent = cast.new_node(CASTNode::MethodDef {
//                 name: name.clone(),
//                 inputs: inputs.clone(),
//                 return_type: return_type.clone(),
//             });
//             //Create a temporary variable to hold the return value
//             let return_val_tmp = generate_tmp();
//             {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let return_val_dec = cast.new_node(CASTNode::VarDec {
//                     name: return_val_tmp.clone(),
//                     t: return_type.clone(),
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
//         },
//         IRNodeType::MethodCall(id) => branch_function! {
//             // Gather the method's information
//             let (name, inputs, ret) = {
//                 let method_info = &ast[*id];

//                 if let IdentInfo::Method {
//                     name,
//                     params,
//                     return_type,
//                 } = method_info
//                 {
//                     (
//                         name,
//                         params
//                             .into_iter()
//                             .map(|id| (ast[*id].name().clone(), ast[*id].return_type()))
//                             .collect::<Vec<_>>(),
//                         return_type.clone(),
//                     )
//                 } else if let IdentInfo::ExternMethod {
//                     imp_name,
//                     name,
//                     params,
//                     return_type,
//                     ..
//                 } = method_info
//                 {
//                     used_wasm_imports.insert((imp_name.clone(), name.clone()));
//                     (name, params.clone(), return_type.clone())
//                 } else {
//                     return Err(anyhow::format_err!(
//                     "Should never happen: attempted to parse `MethodCall` whose identifier referred to a `Var`, not `Method` ({})",
//                     method_info.name()
//                     ));
//                 }
//             };
//             // println!("Method call: {:?}", ast[*id].name_and_return_type());

//             let parent = cast.new_node(CASTNode::Ignore);

//             //A list of the names and types of all temporary variables for holding parameter values
//             let mut params_tmp_vars = Vec::with_capacity(children.len());
//             //For each parameter, create a temporary variable
//             for i in 0..children.len() {
//                 //Generate the temporary variable
//                 let param_tmp_var = generate_tmp();
//                 // let t = ast[inputs[i]].return_type();
//                 let t = inputs[i].1.clone();

//                 //Create the representation for the tmp var in `cast`
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let param_tmp_dec = cast.new_node(CASTNode::VarDec {
//                     name: param_tmp_var.clone(),
//                     t: t.clone(),
//                 });
//                 cast.append_to(semicolon, param_tmp_dec)?;
//                 cast.append_to(parent, semicolon)?;

//                 //Recurse on the parameter using `param_tmp_var`
//                 {
//                     let child = &mut recurse_child!(i, Some(param_tmp_var.clone()));
//                     cast.append_tree(parent, child)?;
//                 }

//                 //Push the temporary variable to the list of params
//                 params_tmp_vars.push((t.clone(), param_tmp_var));
//             }

//             //Finish by adding the method call itself, which assigns to `return_val`
//             //Method calls where `return_val == None` will not assign at all
//             {
//                 //Regarless of whether or not there's a return type, create the semicolon/method call subtree
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let inputs = params_tmp_vars.into_iter().map(|(_, name)| name).collect();

//                 let method_call = cast.new_node(CASTNode::MethodCall {
//                     name: name.clone(),
//                     inputs,
//                 });

//                 if let Some(return_var) = return_var {
//                     let assign = cast.new_node(CASTNode::Assign(return_var.clone()));

//                     cast.append_to(assign, method_call)?;
//                     cast.append_to(semicolon, assign)?;
//                     cast.append_to(parent, semicolon)?;
//                 } else {
//                     cast.append_to(semicolon, method_call)?;
//                     cast.append_to(parent, semicolon)?;
//                 }
//             }
//         },
//         IRNodeType::IfCondition => branch_function! {
//             let parent = cast.new_node(CASTNode::Ignore);
//             let tmp_conditional_t = Type::Bool;
//             let tmp_conditional = generate_tmp();
//             //TODO: deal with the case where the body returns `void`
//             // let tmp_body = generate_tmp();

//             //Create declarations for the temp variables
//             {
//                 let semicolon = cast.new_node(CASTNode::Semicolon);
//                 let var_dec_list = cast.new_node(CASTNode::VarDecList {
//                     names: vec![tmp_conditional.clone()],
//                     t: tmp_conditional_t,
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
//         },
//         //Binary operations
//         IRNodeType::Add
//         | IRNodeType::Sub
//         | IRNodeType::Mul
//         | IRNodeType::Div
//         | IRNodeType::Eq
//         | IRNodeType::Ne
//         | IRNodeType::Lt
//         | IRNodeType::Gt
//         | IRNodeType::Le
//         | IRNodeType::Ge => branch_function! {
//                 //Compiles as follows:

//                 // let tmp_1, tmp_2; <<must(?) have same type
//                 //
//                 // { recurse_child!(0, tmp_1); }
//                 // { recurse_child!(0, tmp_2); }
//                 //
//                 // return_var = tmp_1 {op} tmp_2;

//                 // println!("{}", Backtrace::force_capture());
//                 let parent = cast.new_node(CASTNode::Ignore);
//                 let tmp_t = Type::Int32; //Ah, types
//                 let tmp_1 = generate_tmp();
//                 let tmp_2 = generate_tmp();

//                 //Create declarations for the temp variables
//                 {
//                     let semicolon = cast.new_node(CASTNode::Semicolon);
//                     let var_dec_list = cast.new_node(CASTNode::VarDecList {
//                         names: vec![tmp_1.clone(), tmp_2.clone()],
//                         t: tmp_t,
//                     });
//                     cast.append_to(semicolon, var_dec_list)?;
//                     cast.append_to(parent, semicolon)?;
//                 }

//                 //Compile the `lhs`
//                 {
//                     let lhs = &mut recurse_child!(0, Some(tmp_1.clone()));

//                     let brackets = cast.new_node(CASTNode::Brackets);
//                     cast.append_to(parent, brackets)?;
//                     cast.append_tree(brackets, lhs)?;
//                 }

//                 //Compile the `rhs`
//                 {
//                     let rhs = &mut recurse_child!(1, Some(tmp_2.clone()));

//                     let brackets = cast.new_node(CASTNode::Brackets);
//                     cast.append_to(parent, brackets)?;
//                     cast.append_tree(brackets, rhs)?;
//                 }
//                 //Final assignment statement
//                 if let Some(return_var) = return_var {
//                     let semicolon = cast.new_node(CASTNode::Semicolon);
//                     let assignment = cast.new_node(CASTNode::Assign(return_var));
//                     cast.append_to(semicolon, assignment)?;
//                     cast.append_to(parent, semicolon)?;

//                     //The only difference between all the bin ops is the actual operator in use
//                     let op = {
//                         use IRNodeType::*;
//                         let op = match &ast[curr_ast].data.t {
//                             Add => CASTNode::Add,
//                             Sub => CASTNode::Sub,
//                             Mul => CASTNode::Mul,
//                             Div => CASTNode::Div,
//                             Eq => CASTNode::Eq,
//                             Ne => CASTNode::Ne,
//                             Lt => CASTNode::Lt,
//                             Gt => CASTNode::Gt,
//                             Le => CASTNode::Le,
//                             Ge => CASTNode::Ge,
//                             _ => unreachable!(),
//                         };
//                         cast.new_node(op)
//                     };
//                     cast.append_to(assignment, op)?;

//                     let tmp_1 = cast.new_node(CASTNode::VarRef(tmp_1));
//                     cast.append_to(op, tmp_1)?;
//                     let tmp_2 = cast.new_node(CASTNode::VarRef(tmp_2));
//                     cast.append_to(op, tmp_2)?;
//                 }
//         },

//         //LastValueReturn:
//         //When parent is a method def OR there is no parent OR the parent is also a LastValueReturn, ignore
//         //When parent is an assignment, set final child to assignment and have the parent be brackets
//         //When parent is a VariableDef, replace `curr` with two nodes: a C::VariableDef and brackets (treated like an assignment would be)

//         // `return_var` is passed to only the final child. This is handled by `single_parent` by default
//         // When the children are method definitions, brackets would be syntatically invalid
//         IRNodeType::LastValueReturn => branch_function! {match &ast[children[0]].data.t {
//             IRNodeType::MethodDef { .. } | IRNodeType::LastValueReturn => {
//                 single_parent_no_return!(CASTNode::Ignore)
//             }
//             _ => single_parent!(CASTNode::Brackets),
//         }},
//         IRNodeType::ValueConsume => branch_function! {match &ast[children[0]].data.t {
//             IRNodeType::VarDef { .. } => single_parent!(CASTNode::Ignore),
//             _ => single_parent!(CASTNode::Semicolon),
//         }},
//     }
// }
