use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Range, Sub},
};

use crate::verify::Type;

#[derive(Debug, Clone, PartialEq)]
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
            Assign { .. } => format!("Assign"),
            VariableRef { name } => format!("Var: {name}"),
            VariableDef { name, .. } => format!("Var Def: {name}"),
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
            //ASTNodeType::MethodCall { name } => format!("Method call: {name}"),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A value without a value
    Void,
    Bool(bool),
    /// An integer of unknown size (defaults to i32, stored as i128 until implicit typing is figured out)
    Int(i128),
    Int32(i32),
    String(String),
}

macro_rules! bin_op_match {
    ($lhs:ident, $rhs:ident, $sym:tt) => {
        //An operation between an unsized generic type (ie. `Int`, `Float`) and a sized counterpart (`Int32`, `Int64`, etc.) coerces the unsized value to the sized type
        //match (self, rhs) {
        match ($lhs, $rhs) {
            //Integers
            (Int(lhs), Int(rhs)) => Ok(Int(lhs $sym rhs)),

            (Int32(lhs), Int(rhs)) => Ok(Int32(lhs $sym rhs as i32)),
            (Int(lhs), Int32(rhs)) => Ok(Int32(lhs as i32 $sym rhs)),
            (Int32(lhs), Int32(rhs)) => Ok(Int32(lhs $sym rhs )),

            //Other

            _ => Err(()),
        }
    }
}

macro_rules! arithmetic_impl_for_value {
    ($trait:ident, $trait_fn:ident, $sym:tt) => {
        impl $trait for Value {
            type Output = Result<Value, ()>;

            fn $trait_fn(self, rhs: Self) -> Self::Output {
                use Value::*;
                bin_op_match!(self, rhs, $sym)
            }
        }
    };
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, other) {
            //Integers
            (Int(lhs), Int(rhs)) => Some(lhs.cmp(rhs)),

            (Int32(lhs), Int(rhs)) => Some(lhs.cmp(&((*rhs) as i32))),
            (Int(lhs), Int32(rhs)) => Some(((*lhs) as i32).cmp(rhs)),
            (Int32(lhs), Int32(rhs)) => Some(lhs.cmp(rhs)),
            //Bool
            (Bool(lhs), Bool(rhs)) => Some(lhs.cmp(rhs)),
            //...
            _ => None,
        }
    }
}

arithmetic_impl_for_value!(Add, add, +);
arithmetic_impl_for_value!(Sub, sub, -);
arithmetic_impl_for_value!(Mul, mul, *);
arithmetic_impl_for_value!(Div, div, /);

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        let s = match self {
            Void => format!("Void()"),
            Bool(b) => format!("Bool({b})"),
            Int(n) => format!("Int[?]({n})"),
            Int32(n) => format!("Int32({n})"),
            String(s) => format!("String({s})"),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNodeType {
    /// A primitive literal
    ///
    /// `0` children
    Literal { val: Value },
    /// A variable identifier
    ///
    /// `0` children
    VariableRef { name: String },
    /// The initial declaration of a variable, where its initial value is the evalution of this node's child
    ///
    /// `1` child
    VariableDef { name: String },
    /// The definition for a method. The body of the code is stored as a child of this node
    ///
    /// `1` child
    MethodDef {
        name: String,
        inputs: Vec<(Type, String)>,
        return_type: Type,
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
