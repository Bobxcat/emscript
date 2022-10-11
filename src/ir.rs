use std::{
    collections::{HashMap, HashSet},
    ops::Index,
};

use crate::{
    ast::{ASTNode, ASTNodeType, Value},
    tree::{Node, NodeId, Tree},
    verify::Type,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentID(usize);
pub enum IRNode {
    Literal(Value),
    /// A variable identifier
    ///
    /// `0` children
    VarRef(IdentID),
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
pub enum IdentInfo {
    /// This identifier refers to a variable
    Var { name: String, t: Type },
    /// This identifier refers to a method
    Method {
        name: String,
        params: Vec<IdentID>,
        return_type: Type,
    },
}

impl IdentInfo {
    pub fn name(&self) -> &String {
        match self {
            IdentInfo::Var { name, .. } | IdentInfo::Method { name, .. } => name,
        }
    }
    pub fn return_type(&self) -> Type {
        match self {
            IdentInfo::Var { t, .. } | IdentInfo::Method { return_type: t, .. } => *t,
        }
    }
    pub fn name_and_return_type(&self) -> (Type, &String) {
        match self {
            IdentInfo::Var { t, name, .. }
            | IdentInfo::Method {
                return_type: t,
                name,
                ..
            } => (*t, name),
        }
    }
}

/// An AST closely related to the EmScript AST, semantically.
///
/// Identifiers do not store the string themselves, instead a reference to which identifier they refer to
pub struct IRAST {
    pub tree: Tree<IRNode>,
    /// All identifiers in the AST
    ///
    /// TODO: how to deal with the difference between methods with parameter types & return type and variables,
    /// which just have a type?
    /// Possibly: store identifier info as an enum (method or variable). Or, have a seperate hashmap for methods
    pub idents: HashMap<IdentID, IdentInfo>,
}

/// For use in `IRAST::from_ast`.
///
/// Stores a stack of lookup tables.
/// When walking recursively through an AST, when a scope increase is encountered then `increase_scope()` is called
/// and vice-versa
///
/// This stack represents a way of seeing what identifiers are visible at a given point and which
#[derive(Debug, Clone)]
struct IdentScopeStack {
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
    pub fn from_ast(ast: &Tree<ASTNode>) -> anyhow::Result<Self> {
        //First, create an empty identifier stack. Note that the
        let mut ident_stack = IdentScopeStack::new();
        let tree = Self::from_ast_recurse(ast, ast.find_head().unwrap(), &mut ident_stack)?;
        // Self::from_ast_recurse(ast, &mut None, ast.find_head().unwrap(), &mut ident_stack)?;
        Ok(Self {
            tree,
            idents: ident_stack.global_idents,
        })
    }
    /// * `curr` - the current node in `ast` which is being transformed
    /// * `idents` - the table of all identifiers that have existed in the program
    /// * `ident_stack` - the table of identifiers
    fn from_ast_recurse(
        ast: &Tree<ASTNode>,
        curr: NodeId,
        ident_stack: &mut IdentScopeStack,
        // increase_scope: bool,
    ) -> anyhow::Result<Tree<IRNode>> {
        // println!(
        //     "Current node: {}\nRaw Context: {:?}\nCurrent ident stack: {:#?}\n\n",
        //     ast[curr].data, ast[curr].data.context, ident_stack
        // );
        let children = ast[curr].children.clone();
        let mut ir_tree = Tree::new();

        /// Recursively calls this method on all children, appending them with the supplied `IRNode` as their parent
        ///
        /// * `parent_ir_node`
        macro_rules! single_parent {
            ($parent:expr) => {{
                let parent = ir_tree.new_node($parent);

                let mut prev = curr;

                for c in children {
                    //When the scope increases, adjust (if c > prev: increase_scope)
                    //When the scope decreases, adjust (if prev < c: decrease_scope) <- do an additional check at the end

                    if ast[c].data.scope_depth > ast[prev].data.scope_depth {
                        ident_stack.increase_scope();
                    }
                    let mut subtree = Self::from_ast_recurse(ast, c, ident_stack)?;
                    // let mut subtree = recurse_on_id!(c);
                    ir_tree.append_tree(parent, &mut subtree)?;

                    if ast[c].data.scope_depth < ast[prev].data.scope_depth {
                        ident_stack.decrease_scope();
                    }
                    prev = c;
                }
                //The additional scope change check needed at the end of the looping, to deal with
                //when the final child has a deeper scope than the parent (which is often)
                if ast[curr].data.scope_depth < ast[prev].data.scope_depth {
                    ident_stack.decrease_scope();
                }
            }};
        }

        match &ast[curr].data.t {
            ASTNodeType::VariableDef { name, t } => {
                let ident = ident_stack.new_ident(IdentInfo::Var {
                    name: name.clone(),
                    t: t.ok_or(anyhow::format_err!(
                        "VariableDef `{name}` has a type which is currently unknown"
                    ))?,
                });

                single_parent!(IRNode::VarDef(ident))
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
                        name: name.clone(),
                        t: *t,
                    });
                }
                // Generate the method identifier itself
                let method_id = ident_stack.new_ident(IdentInfo::Method {
                    name: name.clone(),
                    params,
                    return_type: *return_type,
                });
                // Finish by recursing with just this node as the parent
                single_parent!(IRNode::MethodDef(method_id))
                // single_parent!(IRNode::MethodDef {
                //     id: method_id,
                //     inputs: params,
                //     return_type: *return_type
                // })
            }
            //Identifier references
            ASTNodeType::VariableRef { name } => {
                // Try and find this variable, if unable to then return an error
                // Eventually error handling may become more sophisticated
                if let Some(var_id) = ident_stack.get_ident_from_name(name) {
                    single_parent!(IRNode::VarRef(var_id))
                } else {
                    return Err(anyhow::format_err!(
                        "Variable referenced which was not in scope: `{name}`"
                    ));
                }
            }
            ASTNodeType::MethodCall { name } => {
                // Try and find this method, if unable to then return an error
                // Eventually error handling may become more sophisticated
                if let Some(method_id) = ident_stack.get_ident_from_name(name) {
                    single_parent!(IRNode::MethodCall(method_id))
                } else {
                    return Err(anyhow::format_err!(
                        "Method called which was not in scope: `{name}`"
                    ));
                }
            }
            ASTNodeType::Assign { name } => {
                // let ident = most_valid_ident!(name);
                if let Some(ident) = ident_stack.get_ident_from_name(name) {
                    single_parent!(IRNode::Assign(ident))
                } else {
                    return Err(anyhow::format_err!(
                        "Assignment on variable which was not in scope: {name}"
                    ));
                }
            }
            // Trivial transformations
            ASTNodeType::Literal { val } => single_parent!(IRNode::Literal(val.clone())),
            ASTNodeType::IfCondition => single_parent!(IRNode::IfCondition),
            ASTNodeType::Add => single_parent!(IRNode::Add),
            ASTNodeType::Sub => single_parent!(IRNode::Sub),
            ASTNodeType::Mul => single_parent!(IRNode::Mul),
            ASTNodeType::Div => single_parent!(IRNode::Div),
            ASTNodeType::Eq => single_parent!(IRNode::Eq),
            ASTNodeType::Ne => single_parent!(IRNode::Ne),
            ASTNodeType::Lt => single_parent!(IRNode::Lt),
            ASTNodeType::Gt => single_parent!(IRNode::Gt),
            ASTNodeType::Le => single_parent!(IRNode::Le),
            ASTNodeType::Ge => single_parent!(IRNode::Ge),
            ASTNodeType::LastValueReturn => single_parent!(IRNode::LastValueReturn),
            ASTNodeType::ValueConsume => single_parent!(IRNode::ValueConsume),
        }

        Ok(ir_tree)
    }
    /// Mangles the AST to guarantee that each identifier is globally unique
    ///
    /// This also guarantees to keep the original name of the identifier somewhere within its new name
    /// * `no_mangle_methods` - The set of methods which will never be mangled.
    /// Each identifier will only count once, and the earliest encountered matching method name will not be mangled
    pub fn mangle(&mut self, mut no_mangle_methods: HashSet<&str>) {
        let mut ident_count = 0;
        for ident in self.idents.values_mut() {
            let mut mangle = true;
            let var_name = match ident {
                IdentInfo::Var { name, .. } => name,
                IdentInfo::Method { name, .. } => {
                    //If this method was in the list of no_mangle_methods, remove it and don't mangle it
                    if no_mangle_methods.remove(name.as_str()) {
                        mangle = false;
                    }

                    name
                }
            };
            if mangle {
                *var_name = format!("_{}_{}", ident_count, var_name);
                ident_count += 1;
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
