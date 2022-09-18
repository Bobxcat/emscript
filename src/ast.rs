use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Range, Sub},
    rc::Rc,
};

use crate::verify::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTNode {
    pub t: ASTNodeType,
    pub context: StringContext,
}

impl ASTNode {
    pub fn new(t: ASTNodeType, context: StringContext) -> Self {
        Self { t, context }
    }
    /*fn get_childen(&self) -> Vec<&ASTNode> {
        use ASTNodeType::*;
        match &self.t {
            //Binary ops
            Add { lhs, rhs }
            | Sub { lhs, rhs }
            | Mul { lhs, rhs }
            | Div { lhs, rhs }
            | Assign { lhs, rhs } => {
                vec![lhs, rhs]
            }
            //Unary ops
            ValueConsume { expr } => vec![expr],
            VariableDef { val, .. } => vec![val],
            //Childless endpoints
            Literal { .. } | VariableRef { .. } => vec![],
            LastValueReturn { expr_list } => expr_list.iter().collect(),
            MethodDef { body, .. } => vec![body],
            MethodCall { name } => todo!(),
        }
    }
    fn to_tree_string(&self, indent_level: usize) -> String {
        use ASTNodeType::*;
        const INDENTS_PER_LEVEL: usize = 2;

        let mut s = str::repeat(" ", INDENTS_PER_LEVEL * indent_level);

        s += &*match &self.t {
            Literal { val } => format!("{val}"),
            Add { .. } => format!("Add"),
            Sub { .. } => format!("Sub"),
            Mul { .. } => format!("Mul"),
            Div { .. } => format!("Div"),
            Assign { .. } => format!("Assign"),
            VariableRef { name } => format!("Variable Reference: {name}"),
            VariableDef { name, .. } => format!("Variable Definition: {name}"),
            ValueConsume { .. } => format!("Value consume"),
            LastValueReturn { expr_list } => format!("Last value return"),
            MethodDef {
                name, return_type, ..
            } => format!("Method definition: {name}() -> {return_type}"),
            MethodCall { name } => format!("Method call: {name}"),
            //ASTNodeType::MethodCall { name } => format!("Method call: {name}"),
        };

        s += "\n";

        for c in self.get_childen() {
            s += &c.to_tree_string(indent_level + 1);
        }

        s
    }*/
}

impl Display for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ASTNodeType::*;
        let s = &*match &self.t {
            Literal { val } => format!("{val}"),
            Add { .. } => format!("Add"),
            Sub { .. } => format!("Sub"),
            Mul { .. } => format!("Mul"),
            Div { .. } => format!("Div"),
            Assign { .. } => format!("Assign"),
            VariableRef { name } => format!("Variable Reference: {name}"),
            VariableDef { name, .. } => format!("Variable Definition: {name}"),
            ValueConsume { .. } => format!("Value consume"),
            LastValueReturn { .. } => format!("Last value return"),
            MethodDef {
                name, return_type, ..
            } => format!("Method definition: {name}() -> {return_type}"),
            MethodCall { name } => format!("Method call: {name}"),
            //ASTNodeType::MethodCall { name } => format!("Method call: {name}"),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A value without a value
    Void,
    /// An integer of unknown size (defaults to i32, stored as i128 until implicit typing is figured out)
    Int {
        n: i128,
    },
    Int32 {
        n: i32,
    },
    String {
        s: String,
    },
}

macro_rules! arithmetic_impl_for_value {
    ($trait:ident, $trait_fn:ident, $sym:tt) => {
        impl $trait for Value {
            type Output = Result<Value, ()>;

            fn $trait_fn(self, rhs: Self) -> Self::Output {
                use Value::*;
                //An operation between an unsized generic type (ie. `Int`, `Float`) and a sized counterpart (`Int32`, `Int64`, etc.) coerces the unsized value to the sized type
                match (self, rhs) {
                    //Integers
                    (Int { n: n_0}, Int { n }) => Ok(Int { n: n_0 $sym n }),

                    (Int32 { n: n_0}, Int { n }) => Ok(Int32 { n: n_0 $sym n as i32 }),
                    (Int { n: n_0}, Int32 { n }) => Ok(Int32 { n: n_0 as i32 $sym n }),
                    (Int32 { n: n_0}, Int32 { n }) => Ok(Int32 { n: n_0 $sym n }),

                    //Other

                    _ => Err(()),
                }
            }
        }
    };
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
            Int { n } => format!("Int[?]({n})"),
            Int32 { n } => format!("Int32({n})"),
            String { s } => format!("String({s})"),
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
    MethodDef { name: String, return_type: Type },
    /// Represents a method call being made
    ///
    /// `0+` children, 1 for each parameter
    MethodCall { name: String },
    //Binary ops
    /// `2` children
    Add,
    /// `2` children
    Sub,
    /// `2` children
    Mul,
    /// `2` children
    Div,

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
            line,
        }
    }
    pub fn empty() -> Self {
        Self {
            raw_span: Range { start: 0, end: 0 },
            index_in_line: 0,
            line: String::new(),
        }
    }
    pub fn to_string(&self) -> String {
        /// The amount of extra whitespace added to the line when displaying it
        const EXTRA_WHITESPACE: usize = 2;

        let pointer = "-".repeat(EXTRA_WHITESPACE + self.index_in_line) + "^";

        format!("{}{}\n{}", " ".repeat(EXTRA_WHITESPACE), self.line, pointer)
    }
}

impl Display for StringContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
