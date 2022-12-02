use std::{
    backtrace::Backtrace,
    collections::{HashMap, HashSet},
    mem::{size_of, size_of_val},
    ops::Index,
};

use crate::{
    ast::{ASTNode, ASTNodeType, StringContext},
    interface::Interface,
    tree::{Node, NodeId, Tree},
    utils::{format_compact, PREFIX_IDENT},
    value::{
        custom_types::{custom_types, custom_types_mut, str_to_type},
        CustomTypeId, Type, TypeOrName, Value,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentID(usize);
#[derive(Debug, Clone)]
pub enum IRNodeType {
    Literal(Value),
    /// A variable identifier
    ///
    /// `0` children
    VarRef(IdentID),
    /// A reference to a field of an object. Currently just holds the name of the field (mangling should be unnecessary)
    ///
    /// `1` child
    FieldRef(String),
    /// The initial declaration of a variable, where its initial value is the evalution of this node's child
    ///
    /// `1` child
    VarDef(IdentID),
    /// The definition for a method. The body of the code is stored as a child of this node
    ///
    /// `1` child
    MethodDef(IdentID),
    // MethodDef {
    //     id: IdentID,
    //     /// The inputs to a method are guaranteed to be a variable, not a method
    //     inputs: Vec<IdentID>,
    //     return_type: Type,
    // },
    /// Represents a method call being made
    ///
    /// `0+` children, 1 for each parameter
    MethodCall(IdentID),

    /// `& {child}`
    ///
    /// `1` child
    Reference,

    /// Represents an `if` statement
    ///
    /// `2` children, first is conditional (evaluates to bool), second is body
    IfCondition,

    //Assign
    /// Assignment of the given variable to the value given by this node's child
    ///
    /// `1` child
    Assign(IdentID),

    /// Represents a list of expressions from which the last expression's value is returned
    ///
    /// `1+` children, must have at least one child. Executes but ignores the return values of all but the last child
    LastValueReturn,

    /// Represents an expression which produces a value consumed by something like a semicolon (and returns void)
    ///
    /// `1` child
    ValueConsume,

    //Binary ops
    /// `2` children
    Add,
    /// `2` children
    Sub,
    /// `2` children
    Mul,
    /// `2` children
    Div,
    /// `2` children
    Eq,
    /// `2` children
    Ne,
    /// `2` children
    Lt,
    /// `2` children
    Gt,
    /// `2` children
    Le,
    /// `2` children
    Ge,
}

#[derive(Debug, Clone)]
pub struct IRNode {
    pub t: IRNodeType,
    pub ctx: StringContext,
    /// The type which the expression represented by this node returns.
    ///
    /// Before running `IRAST::set_types_recurse`, each node just has `Type::Void` (to avoid the use of `Option<Type>`)
    pub return_type: Type,
}

impl std::fmt::Display for IRNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone)]
pub enum IdentInfo {
    /// This identifier refers to a variable
    Var { name: String, t: Type },
    /// This identifier refers to a method
    Method {
        name: String,
        params: Vec<IdentID>,
        return_type: Type,
    },
    ExternMethod {
        mod_name: String,
        imp_name: String,
        name: String,
        params: Vec<(String, Type)>,
        return_type: Type,
    },
    /// This identifier is a custom type with the given ID
    CustomType {
        mangled_name: String,
        id: CustomTypeId,
    },
}

impl IdentInfo {
    pub fn name(&self) -> &String {
        match self {
            IdentInfo::Var { name, .. }
            | IdentInfo::Method { name, .. }
            | IdentInfo::ExternMethod { name, .. }
            | IdentInfo::CustomType {
                mangled_name: name, ..
            } => name,
        }
    }
    pub fn return_type(&self) -> Type {
        match self {
            IdentInfo::Var { t, .. }
            | IdentInfo::Method { return_type: t, .. }
            | IdentInfo::ExternMethod { return_type: t, .. } => t.clone(),
            IdentInfo::CustomType { id, .. } => Type::Custom(*id),
        }
    }
    pub fn name_and_return_type(&self) -> (Type, &String) {
        //Why be performant?
        (self.return_type(), self.name())
    }
}

/// An AST closely related to the EmScript AST, semantically.
///
/// Identifiers do not store the string themselves, instead a reference to which identifier they refer to
pub struct IRAST {
    pub tree: Tree<IRNode>,
    /// All identifiers in the AST
    pub idents: HashMap<IdentID, IdentInfo>,
}

/// For use in `IRAST::from_ast` or when otherwise converting from a raw AST
///
/// Stores a stack of lookup tables.
/// When walking recursively through an AST, when a scope increase is encountered then `increase_scope()` is called
/// and vice-versa
///
/// This stack represents a way of seeing what identifiers are visible at a given point as well as
/// the global collection of all identifiers
#[derive(Debug, Clone)]
pub(crate) struct IdentScopeStack {
    /// Represents all identifiers which have *ever* been encountered by the stack
    global_idents: HashMap<IdentID, IdentInfo>,
    /// Represents all identifiers which are currently visible (if this has been appropriately used walking the AST)
    ///
    /// More specifically, the name of some identifier put into this table will return the `IdentID` of the matching
    /// identifier in `global_idents`
    tables: Vec<HashMap<String, IdentID>>,
}

impl IdentScopeStack {
    pub fn new() -> Self {
        Self {
            global_idents: HashMap::new(),

            tables: Vec::new(),
        }
    }
    /// Increases the current scope of this stack. Any new identifiers will be inserted into this new scope
    pub fn increase_scope(&mut self) {
        self.tables.push(HashMap::new())
    }
    pub fn decrease_scope(&mut self) {
        self.tables.pop();
    }
    /// Generates a new identifier with the given information, updates `self` with the new ident, and returns its ID
    ///
    /// If `info.name()` is already an entry in `self.tables`, then it __will__ be overwritten.
    /// This should normally be the desired effect
    pub fn new_ident(&mut self, info: IdentInfo) -> IdentID {
        let mut n = self.global_idents.len();
        while self.global_idents.contains_key(&IdentID(n)) {
            n += 1;
        }
        let id = IdentID(n);

        //Insert into *both* the global identifier pool and the stack
        self.tables
            .last_mut()
            .unwrap()
            .insert(info.name().clone(), id);
        self.global_idents.insert(id, info);

        id
    }
    /// Gets the identifier which takes priority with the given by name
    ///
    /// In other words, for an identifier with this name in the current location,
    /// this method will return which `IdentID` this refers to
    pub fn get_ident_from_name(&self, name: &str) -> Option<IdentID> {
        //Iterate over all tables in backwards order -- later tables represent lower scopes, which are higher priority
        for table in self.tables.iter().rev() {
            for (s, id) in table {
                if name == s {
                    return Some(*id);
                }
            }
        }

        None
    }
}

impl IRAST {
    pub fn from_ast(ast: &Tree<ASTNode>, interface: &Interface) -> anyhow::Result<Self> {
        //First, create an empty identifier stack. Note that the scope must be increased before first use
        let mut ident_stack = IdentScopeStack::new();

        // Create initial globally scoped values, namely interface types and methods

        // Note that custom types are already stored in the global state `CUSTOM_TYPES` (accessible via `custom_types()`).
        // So, `CustomType` identifiers are just references to the actual CustomType along with a mangled name. Also, it won't break anything if
        // the load order of methods & types into `ident_stack` is arbitrary (since all CustomTypes already exist with the same IDs and don't rely on the methods)

        ident_stack.increase_scope();

        //Add all custom types
        {
            let custom_types = custom_types();
            for (id, t) in custom_types.iter_k() {
                let info = IdentInfo::CustomType {
                    mangled_name: t.name.clone(),
                    id: *id,
                };
                ident_stack.new_ident(info);
            }
        }

        //Add all methods
        //Note: this same code exists in `c_ast::ast_to_cast`
        for (name, imp) in &interface.wasm_imports {
            let mut param_idx = 0;
            let info = IdentInfo::ExternMethod {
                mod_name: imp.mod_name.clone(),
                imp_name: name.clone(),
                name: name.clone(),
                params: imp
                    .params
                    .iter()
                    .map(|t| {
                        param_idx += 1;
                        (format!("_{param_idx}"), t.clone())
                    })
                    .collect(),
                return_type: imp.ret.clone(),
            };

            ident_stack.new_ident(info);
        }

        // Generate an untyped AST
        let tree =
            Self::from_ast_recurse(ast, ast.find_head().unwrap(), &mut ident_stack, interface)?;
        let mut tree = Self {
            tree,
            idents: ident_stack.global_idents,
        };

        // Verify and set expression types
        tree.set_types_recurse(tree.tree.find_head().unwrap(), Type::Void)?;

        Ok(tree)
    }
    /// * `ast` - the AST of the input code
    /// * `curr` - the current node in `ast` which is being transformed
    /// * `ident_stack` - the identifier stack
    /// * `interface` - defines external methods and such
    fn from_ast_recurse(
        ast: &Tree<ASTNode>,
        curr: NodeId,
        ident_stack: &mut IdentScopeStack,
        interface: &Interface,
    ) -> anyhow::Result<Tree<IRNode>> {
        // println!(
        //     "Current node: {:?}\nBacktrace:\n{}",
        //     curr,
        //     Backtrace::force_capture()
        // );
        let children = ast[curr].children.clone();
        let mut ir_tree = Tree::new();

        /// Recursively calls this method on all children, appending them with the supplied `IRNode` as their parent
        ///
        /// Returns the ID of the parent which was added from the supplied value
        ///
        /// * `parent_ir_node`
        macro_rules! single_parent {
            ($parent:expr) => {{
                let parent = ast_node_from_type!($parent);

                let parent = ir_tree.new_node(parent);

                single_parent_id!(parent);
            }};
        }

        /// Equivalent to `single_parent` except that it takes the ID of an already-generated parent node instead of value
        macro_rules! single_parent_id {
            ($parent:expr) => {{let mut prev = curr;

                for c in children {
                    //When the scope increases, adjust (if c > prev: increase_scope)
                    //When the scope decreases, adjust (if prev < c: decrease_scope) <- do an additional check at the end

                    if ast[c].data.scope_depth > ast[prev].data.scope_depth {
                        ident_stack.increase_scope();
                    }
                    let mut subtree = Self::from_ast_recurse(ast, c, ident_stack, interface)?;
                    ir_tree.append_tree($parent, &mut subtree)?;

                    if ast[c].data.scope_depth < ast[prev].data.scope_depth {
                        ident_stack.decrease_scope();
                    }
                    prev = c;
                }
                //The additional scope change check needed at the end of the looping, to deal with
                //when the final child has a deeper scope than the parent (which is often)
                if ast[curr].data.scope_depth < ast[prev].data.scope_depth {
                    ident_stack.decrease_scope();
                }}};
        }

        /// Extracts a type from an inputted `TypeOrName` value or returns an `Err` out of the containing method if failed
        macro_rules! type_or_name_to_type {
            ($t:expr) => {
                match $t {
                    TypeOrName::T(t) => t.clone(),
                    //When recieving a custom type, get the underlying type via the `IdentStack`
                    TypeOrName::Name(type_name) => {
                        if let Some(id) = ident_stack.get_ident_from_name(type_name) {
                            match &ident_stack.global_idents[&id] {
                                IdentInfo::CustomType {id, ..} => Type::Custom(*id),
                                _ => return Err(anyhow::format_err!("Identifier encountered which was expected to be a custom type but was instead `{:?}`\n{}\n",
                                ident_stack.global_idents[&id], &ast[curr].data.context
                            ))
                            }
                        } else {
                            return Err(anyhow::format_err!("`TypeOrName` ecountered but not found in `IdentStack`:\n{}\n",
                            &ast[curr].data.context))
                        }
                    }
                }
            };
        }

        /// Formats an error using the input to `ret_err!` as input for `format!` and returns an error of that string,
        /// including the `StringContext` of the current node after a newline
        macro_rules! ret_err {
            ($($toks:tt)*) => {{
                let supplied = format!($($toks)*);
                return Err(anyhow::format_err!("{supplied}\n{}\n", &ast[curr].data.context));
            }};
        }

        macro_rules! ast_node_from_type {
            ($t:expr) => {
                IRNode {
                    t: $t,
                    ctx: ast[curr].data.context.clone(),
                    //Types have not been populated yet
                    return_type: Type::Void,
                }
            };
        }

        match &ast[curr].data.t {
            ASTNodeType::VariableDef { name, t } => {
                //Variables, when declared/defined, do not affect the scope of the rhs of their assignment
                //So, create a temporary parent variable when running the children and replace it later
                let tmp = ir_tree.new_node(ast_node_from_type!(IRNodeType::Add));
                single_parent_id!(tmp);

                //Find out what the type of this variable is
                let t = {
                    if let Some(t) = t {
                        type_or_name_to_type!(t)
                    } else {
                        todo!(
                            "\n\nType inferencing is not yet supported. Consider adding an explicit type, such as `i32` \n{}\n",
                            &ast[curr].data.context
                        )
                    }
                };

                //Generate the identifier and replace the temporary data
                let ident = ident_stack.new_ident(IdentInfo::Var {
                    name: name.clone(),
                    t,
                });
                ir_tree[tmp].data.t = IRNodeType::VarDef(ident);
            }
            ASTNodeType::MethodDef {
                name,
                inputs,
                return_type,
            } => {
                // Generate all the parameters' identifiers
                let mut params = Vec::with_capacity(inputs.len());
                for (t, s) in inputs {
                    let ident = ident_stack.new_ident(IdentInfo::Var {
                        name: s.clone(),
                        t: type_or_name_to_type!(t),
                    });
                    params.push(ident);
                }
                // Generate the method identifier itself
                let method_id = ident_stack.new_ident(IdentInfo::Method {
                    name: name.clone(),
                    params: params.clone(),
                    return_type: type_or_name_to_type!(return_type),
                });
                // Finish by recursing with just this node as the parent
                single_parent!(IRNodeType::MethodDef(method_id));

                // Once finished with recursing on the body, a very important step:
                // Without this, method parameters would permeate the scope of the method definition
                // Essentially, remove all of the parameters from the current scope of the stack manually
                {
                    let table = ident_stack.tables.last_mut().unwrap();
                    for id in params {
                        let param_name = ident_stack.global_idents[&id].name();
                        table.remove(param_name);
                    }
                }
            }
            //Identifier references
            ASTNodeType::VariableRef { name } => {
                // Try and find this variable, if unable to then return an error
                // Eventually error handling may become more sophisticated
                if let Some(var_id) = ident_stack.get_ident_from_name(name) {
                    single_parent!(IRNodeType::VarRef(var_id));
                } else {
                    ret_err!("Variable referenced which was not in scope: `{name}`");
                }
            }
            ASTNodeType::FieldRef { field } => {
                single_parent!(IRNodeType::FieldRef(field.clone()));
            }
            //TODO: A lot
            ASTNodeType::Reference => {
                single_parent!(IRNodeType::Reference)
            }
            ASTNodeType::MethodCall { name } => {
                // Try and find this method, if unable to then return an error
                // Eventually error handling may become more sophisticated
                if let Some(method_id) = ident_stack.get_ident_from_name(name) {
                    single_parent!(IRNodeType::MethodCall(method_id));
                } else {
                    ret_err!("Method called which was not in scope: `{name}`");
                }
            }
            ASTNodeType::Assign { name } => {
                // let ident = most_valid_ident!(name);
                if let Some(ident) = ident_stack.get_ident_from_name(name) {
                    single_parent!(IRNodeType::Assign(ident));
                } else {
                    ret_err!("Assignment on variable which was not in scope: {name}");
                }
            }
            // Trivial transformations
            ASTNodeType::Literal { val } => single_parent!(IRNodeType::Literal(val.clone())),
            ASTNodeType::IfCondition => single_parent!(IRNodeType::IfCondition),
            ASTNodeType::Add => single_parent!(IRNodeType::Add),
            ASTNodeType::Sub => single_parent!(IRNodeType::Sub),
            ASTNodeType::Mul => single_parent!(IRNodeType::Mul),
            ASTNodeType::Div => single_parent!(IRNodeType::Div),
            ASTNodeType::Eq => single_parent!(IRNodeType::Eq),
            ASTNodeType::Ne => single_parent!(IRNodeType::Ne),
            ASTNodeType::Lt => single_parent!(IRNodeType::Lt),
            ASTNodeType::Gt => single_parent!(IRNodeType::Gt),
            ASTNodeType::Le => single_parent!(IRNodeType::Le),
            ASTNodeType::Ge => single_parent!(IRNodeType::Ge),
            ASTNodeType::LastValueReturn => single_parent!(IRNodeType::LastValueReturn),
            ASTNodeType::ValueConsume => single_parent!(IRNodeType::ValueConsume),
        }

        // println!("Generated tree size: {}", size_of_val(&ir_tree));

        Ok(ir_tree)
    }
    /// Recursively sets the type of each `IRNode` and returns errors corresponding to type mismatches
    fn set_types_recurse(&mut self, curr: NodeId, return_type: Type) -> anyhow::Result<()> {
        use IRNodeType::*;

        let children = self.tree[curr].children.clone();

        match &self.tree[curr].data.t {
            Literal(v) => {
                let t = v.t();
                //Check for coercability if a type mismatch is encountered
                if return_type != t {
                    match t {
                        Type::Void => todo!(),
                        Type::Bool => todo!(),
                        Type::Int => todo!(),
                        Type::Int32 => todo!(),
                        Type::Custom(id) => todo!(),
                        //For references, automatically dereference by one level
                        //(i.e. see if the referenced type is coercable)
                        Type::Ref(t) => todo!(),
                    }
                }
            }
            VarRef(_) => todo!(),
            FieldRef(_) => todo!(),
            VarDef(_) => todo!(),
            MethodDef(_) => todo!(),
            MethodCall(_) => todo!(),
            Reference => todo!(),
            IfCondition => todo!(),
            Assign(_) => todo!(),
            LastValueReturn => todo!(),
            ValueConsume => todo!(),
            Add | Sub | Mul | Div | Eq | Ne | Lt | Gt | Le | Ge => {
                //Check to make sure both children can both be coerced into the same type (passed from above)
                self.set_types_recurse(children[0], return_type.clone())?;
                self.set_types_recurse(children[1], return_type.clone())?;
            }
        }

        Ok(())
    }
    /// Mangles the AST to guarantee that each identifier is globally unique
    ///
    /// This also guarantees to keep the original name of the identifier somewhere within its new name
    /// * `no_mangle_methods` - The set of methods which will never be mangled.
    /// Each identifier will only count once, and the earliest encountered matching method name will not be mangled
    pub fn mangle(&mut self, mut no_mangle_methods: HashSet<&str>) {
        let mut ident_count = 0;
        let mut custom_types = custom_types_mut();
        for ident in self.idents.values_mut() {
            let mut mangle = true;
            let var_name = match ident {
                //`no_mangle_methods` does not include any ExternMethod so they can just be trated normally
                IdentInfo::Var { name, .. } | IdentInfo::ExternMethod { name, .. } => name,
                IdentInfo::Method { name, .. }
                | IdentInfo::CustomType {
                    mangled_name: name, ..
                } => {
                    //If this method was in the list of no_mangle_methods, remove it and don't mangle it
                    if no_mangle_methods.remove(name.as_str()) {
                        mangle = false;
                    }

                    name
                }
            };
            if mangle {
                *var_name = format!("{PREFIX_IDENT}{}_{}", format_compact(ident_count), var_name);
                ident_count += 1;
            }

            //If it was a custom type, update `custom_types`
            if let IdentInfo::CustomType { mangled_name, id } = ident {
                custom_types.get_k_mut(id).as_mut().unwrap().mangled_name =
                    Some(mangled_name.clone());
            }
        }
    }
}

impl Index<NodeId> for IRAST {
    type Output = Node<IRNode>;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.tree[index]
    }
}

impl Index<IdentID> for IRAST {
    type Output = IdentInfo;

    fn index(&self, index: IdentID) -> &Self::Output {
        &self.idents[&index]
    }
}
