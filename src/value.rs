use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::{ast::ASTNodeType, c_ast::CASTNode, tree::Tree};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Bool,
    Int,
    Int32,
    /// A custom type. This is anything represented by a struct or enum.
    /// This includes things like `String`, which are not user-defined
    Custom(CustomTypeId),
}

/// Represents a custom type
pub struct CustomType {
    pub name: String,
    pub implementation: CustomTypeImpl,
}

pub struct CallbackMethodData {
    /// The method called when
    callback: Box<dyn FnMut(&mut CustomType, &[Value]) -> Value>,
    /// The ordered list of parameter typings for this method
    pub params: Vec<Type>,
    /// The return type of this method
    pub return_type: Type,
}

/// The imlementation of a custom type.
pub enum CustomTypeImpl {
    /// A custom type which uses callbacks to Rust closures to implement functionality on objects of the type
    ///
    ///
    Callback {
        /// The fields of this custom type. These are all accessable to
        pub_fields: HashMap<String, Type>,
        methods: HashMap<String, CallbackMethodData>,
    }, // /// Represents an implementation of a custom type done directly in `C`, storing its AST.
       // /// Useful for builtin custom types, such as `String`
       // ///
       // /// All `CustomType`s with this impl *must* be provided to the compiler pre-compilation,
       // /// and will be prepended to the rest of the generated code (with proper mangling)
       // RawC {
       //     c_ast: Tree<CASTNode>
       // },
       // /// Represents a custom type implementation written directly in EmScript,
       // /// which will be compiled and prepended to the generated `C` file
       // EmScriptRaw(String),
       // /// Reprents a custom type which is written in the original `.em` file, provided in the program
       // EmScript,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CustomTypeId {
    Name(String),
    Id(usize),
}

impl Display for CustomTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            CustomTypeId::Name(s) => s.clone(),
            CustomTypeId::Id(id) => format!("{id}"),
        };
        write!(f, "{}", s)
    }
}

impl Type {
    pub fn try_from_str(s: &str) -> Result<Self, ()> {
        use Type::*;
        Ok(match s {
            "void" => Void,
            "bool" => Bool,
            "i32" => Int32,
            _ => Custom(CustomTypeId::Name(s.into())),
        })
    }
    pub fn try_from_str_custom_types_with_id(
        s: &str,
        custom_types: &HashMap<usize, CustomType>,
    ) -> Result<Self, ()> {
        use Type::*;
        Ok(match s {
            "void" => Void,
            "bool" => Bool,
            "i32" => Int32,
            _ => {
                let id = {
                    let mut res = Err(());
                    for (id, t) in custom_types {
                        if s == &t.name {
                            res = Ok(*id);
                            break;
                        }
                    }
                    res?
                };
                Custom(CustomTypeId::Id(id))
            }
        })
    }
    /// Returns `true` if `self` is coercable to `other`.
    /// To be more precise, this is if implicit conversion from `self` to `other` is permissible
    pub fn coercable_to(&self, other: &Type) -> bool {
        use Type::*;
        //All the types that `self` can be coerced into. This will always contain `self` by default
        let mut coercable_types = match self {
            Int => vec![Int32],
            _ => Vec::new(),
        };
        coercable_types.push(self.clone());

        //Return whether or not `other` is a type coercable from `self`
        coercable_types.contains(&other)
    }
    /// Returns `true` if *either* `self` is coercable to `other` or `other` is coercable to `self`
    pub fn coercable_to_unordered(&self, other: &Type) -> bool {
        self.coercable_to(other) || other.coercable_to(self)
    }
    pub fn can_add(&self, other: &Type) -> bool {
        self.coercable_to_unordered(other)
    }
    pub fn can_sub(&self, other: &Type) -> bool {
        self.coercable_to_unordered(other)
    }
    pub fn can_mul(&self, other: &Type) -> bool {
        self.coercable_to_unordered(other)
    }
    pub fn can_div(&self, other: &Type) -> bool {
        self.coercable_to_unordered(other)
    }
    pub fn can_eq(&self, other: &Type) -> bool {
        self.coercable_to_unordered(other)
    }
    pub fn can_ne(&self, other: &Type) -> bool {
        self.coercable_to_unordered(other)
    }
    /// Returns `true` if the provided operation can be applied to `self` and `other`, in that order
    pub fn can_operate(&self, other: &Type, bin_op: &ASTNodeType) -> bool {
        // If the value is coercable, than the operation is necessarily valid
        if self.coercable_to_unordered(other) {
            return true;
        }
        match bin_op {
            ASTNodeType::Add => self.can_add(other),
            ASTNodeType::Sub => self.can_sub(other),
            ASTNodeType::Mul => self.can_mul(other),
            ASTNodeType::Div => self.can_div(other),
            ASTNodeType::Eq => self.can_eq(other),
            ASTNodeType::Ne => self.can_ne(other),
            ASTNodeType::Lt => todo!(),
            ASTNodeType::Gt => todo!(),
            ASTNodeType::Le => todo!(),
            ASTNodeType::Ge => todo!(),
            _ => panic!("Called `can_operate` with non-operator: {:#?}", bin_op),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Void => "void".into(),
                Type::Bool => "bool".into(),
                Type::Int => "{integer}".into(),
                Type::Int32 => "i32".into(),
                // Type::String => "string",
                Type::Custom(id) => format!("CustomType[{id}]"),
            }
        )
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

impl Value {
    /// Gets the type of `self`
    pub fn t(&self) -> Type {
        match self {
            Value::Void => Type::Void,
            Value::Bool(_) => Type::Bool,
            Value::Int(_) => Type::Int,
            Value::Int32(_) => Type::Int32,
            Value::String(_) => todo!(),
        }
    }
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
