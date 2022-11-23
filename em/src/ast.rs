use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Range, Sub},
};

use crate::value::{Type, TypeOrName, Value};

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub t: ASTNodeType,
    pub context: StringContext,
    /// The depth of this node in terms of its scope
    ///
    /// Increased by things such as:
    /// * Method declarations
    /// * LastValueReturn
    ///
    /// Not increased by things like:
    /// * ValueConsume
    pub scope_depth: usize,
}

impl ASTNode {
    pub fn new(t: ASTNodeType, context: StringContext) -> Self {
        Self {
            t,
            context,
            scope_depth: 0,
        }
    }
}

impl Display for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ASTNodeType::*;
        let s = &*match &self.t {
            Literal { val } => format!("{val}"),
            Add => format!("Add"),
            Sub => format!("Sub"),
            Mul => format!("Mul"),
            Div => format!("Div"),
            Eq => format!("Eq"),
            Ne => format!("Ne"),
            Lt => format!("Lt"),
            Gt => format!("Gt"),
            Le => format!("Le"),
            Ge => format!("Ge"),
            Assign { name } => format!("Assign: {name}"),
            VariableRef { name } => format!("Var: {name}"),
            FieldRef { field } => format!("Field: {field}"),
            VariableDef { name, .. } => format!("Var Def: {name}"),
            Reference => format!("Ref"),
            ValueConsume { .. } => format!("Value consume"),
            LastValueReturn { .. } => format!("Last value return"),
            MethodDef {
                name,
                return_type,
                inputs,
                ..
            } => {
                let inputs_string = {
                    let mut all_inputs = Vec::new();
                    for (t, name) in inputs {
                        all_inputs.push(format!("{t} {name}"));
                    }
                    all_inputs.join(", ")
                };
                format!("fn {name}({inputs_string}) -> {return_type}")
            }
            MethodCall { name } => format!("Method call: {name}"),
            IfCondition => format!("If statement"),
        };
        write!(f, "{}", s)
        // write!(f, "[scope:{}]{}", self.scope_depth, s)
    }
}

#[derive(Debug, Clone)]
pub enum ASTNodeType {
    /// A primitive literal
    ///
    /// `0` children
    Literal { val: Value },
    /// A variable identifier.
    ///
    /// `0` children
    VariableRef { name: String },
    /// An identifier for a field of some object. The child of this node is the object whose field is being referenced
    ///
    /// `1` child
    FieldRef { field: String },
    /// The initial declaration of a variable, where its initial value is the evalution of this node's child
    /// Contains a type which is `Some(_)` if the type of the variable has been determined, `None` otherwise
    ///
    /// `1` child
    VariableDef { name: String, t: Option<TypeOrName> },
    /// `& {child}`
    ///
    /// `1` child
    Reference,

    /// The definition for a method. The body of the code is stored as a child of this node
    ///
    /// `1` child
    MethodDef {
        name: String,
        inputs: Vec<(TypeOrName, String)>,
        return_type: TypeOrName,
    },
    /// Represents a method call being made
    ///
    /// `0+` children, 1 for each parameter
    MethodCall { name: String },
    /// Represents an `if` statement
    ///
    /// `2` children, first is conditional (evaluates to bool), second is body
    IfCondition,

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

    //Assign
    /// Assignment of the variable `name` to the value given by this node's child
    ///
    /// `1` child
    Assign { name: String },

    /// Represents a list of expressions from which the last expression's value is returned
    ///
    /// `1+` children, must have at least one child. Executes but ignores the return values of all but the last child
    LastValueReturn,

    /// Represents an expression which produces a value consumed by something like a semicolon (and returns void)
    ///
    /// `1` child
    ValueConsume,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringContext {
    /// Span describing the token index in the raw file it was parsed from
    pub raw_span: Range<usize>,
    /// The index of the start of the token on the line it started on
    pub index_in_line: usize,
    /// The index of the line on which the span starts
    pub line_index: usize,
    /// The line containing the start of the token
    pub line: String,
}

impl StringContext {
    pub fn new(raw_span: Range<usize>, raw: &str) -> Self {
        //The raw text until the start of the span (does not include the span start)
        let raw_until_span_start = &raw[..raw_span.start];

        //Find which line the span is on and what its index is in that line
        let (line_index, index_in_line) = {
            let mut newlines_encountered = 0;
            let mut last_newline_index = 0;
            for (i, c) in raw_until_span_start.char_indices() {
                if c == '\n' {
                    newlines_encountered += 1;
                    last_newline_index = i;
                }
            }

            let line_index = newlines_encountered;
            let index_in_line = (raw_span.start - last_newline_index).saturating_sub(1);

            (line_index, index_in_line)
        };

        //The line that the StringContext starts on
        let line = raw.lines().collect::<Vec<_>>()[line_index].to_string();

        Self {
            raw_span,
            index_in_line,
            line_index,
            line,
        }
    }
    pub fn empty() -> Self {
        Self {
            raw_span: Range { start: 0, end: 0 },
            index_in_line: 0,
            line_index: 0,
            line: String::new(),
        }
    }
    pub fn to_string(&self) -> String {
        /// The amount of extra whitespace added to the line when displaying it
        const EXTRA_WHITESPACE: usize = 2;

        let pointer = "-".repeat(EXTRA_WHITESPACE + self.index_in_line) + "^";

        format!(
            "line {}\n{}{}\n{}",
            self.line_index + 1,
            " ".repeat(EXTRA_WHITESPACE),
            self.line,
            pointer
        )
    }
}

impl Display for StringContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
