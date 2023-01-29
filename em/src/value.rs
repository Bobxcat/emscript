use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};



use crate::ast::ASTNodeType;

pub mod custom_types {
    use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

    use once_cell::sync::Lazy;

    use crate::{utils::MultiMap, value::CustomTypeId};

    use super::{CustomType, Type, TypeOrName};

    static CUSTOM_TYPES: Lazy<RwLock<MultiMap<CustomTypeId, String, CustomType>>> =
        Lazy::new(|| RwLock::new(MultiMap::default()));

    pub fn custom_types() -> RwLockReadGuard<'static, MultiMap<CustomTypeId, String, CustomType>> {
        CUSTOM_TYPES.read().expect("Failed to lock `CUSTOM_TYPES`")
    }

    pub fn custom_types_mut(
    ) -> RwLockWriteGuard<'static, MultiMap<CustomTypeId, String, CustomType>> {
        CUSTOM_TYPES
            .write()
            .expect("Failed to mutably lock `CUSTOM_TYPES`")
    }

    pub fn str_to_type(s: &str) -> Option<Type> {
        let t = TypeOrName::from_str(s);
        let custom_types = custom_types();

        match t {
            TypeOrName::T(t) => Some(t),
            TypeOrName::Name(s) => custom_types.get_k_from_v(&s).map(|id| Type::Custom(*id)),
        }
    }

    pub fn insert_custom_type(t: CustomType) -> Option<CustomType> {
        let name = t.name.clone();
        let id = gen_unique_custom_type_id();

        let mut custom_types = custom_types_mut();
        custom_types.insert(id, name, t)
    }

    fn gen_unique_custom_type_id() -> CustomTypeId {
        let custom_types = custom_types();
        let mut n = CustomTypeId(4 * custom_types.len()); //Mul by number so that, when a type is removed, it's easy to find a new one
        while custom_types.contains_k(&n) {
            n.0 += 1;
        }

        n
    }
}

/// Represents a restriction on a type, which can match multiple types or none at all
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeRestriction {
    /// Whether or not a type can be addable on the right hand side with `Type`
    ///
    /// Or, whether or not `{Type} + {t}` is possible
    AddRhs(Type),
    /// Whether or not a type can be addable on the right hand side with `Type`
    ///
    /// Or, whether or not `{Type} - {t}` is possible
    SubRhs(Type),
    /// Whether or not a type can be addable on the right hand side with `Type`
    ///
    /// Or, whether or not `{Type} * {t}` is possible
    MulRhs(Type),
    /// Whether or not a type can be addable on the right hand side with `Type`
    ///
    /// Or, whether or not `{Type} / {t}` is possible
    DivRhs(Type),
    /// Requires `let {Type} foo = {t}` to be valid
    CoercableTo(Type),
}

impl TypeRestriction {
    /// Returns `true` iff the restrictions on `self` are valid for the given type
    pub fn matches(&self, t: Type) -> bool {
        use Type::*;
        match self {
            TypeRestriction::AddRhs(rhs)
            | TypeRestriction::SubRhs(rhs)
            | TypeRestriction::MulRhs(rhs)
            | TypeRestriction::DivRhs(rhs) => match (t, rhs) {
                (Int, Int) => true,
                (Int, Int32) => true,
                (Int32, Int) => true,
                (Int32, Int32) => todo!(),
                //
                (Custom(_lhs), _rhs) => todo!(),
                (_lhs, Custom(_rhs)) => todo!(),
                //
                (Ref(_), Int) => todo!(),
                (Int, Ref(_)) => todo!(),
                (Ref(_), Int32) => todo!(),
                (Int32, Ref(_)) => todo!(),
                (Ref(_lhs), Ref(_rhs)) => todo!(),
                _ => false,
            },
            TypeRestriction::CoercableTo(new_t) => {
                if *new_t == t {
                    return true;
                }

                //All types except abstract types (i.e `Int`, `Float` which are unsized) are
                // coercable into themselves (handled above),
                // and generally number types can coerce upwards in size only
                let prim_coercability: HashMap<Type, &[Type]> =
                    [(Void, &[][..]), (Bool, &[]), (Int, &[Int32]), (Int32, &[])]
                        .into_iter()
                        .collect();
                if let Some(coercable_types) = prim_coercability.get(new_t) {
                    coercable_types.contains(new_t)
                } else {
                    match t {
                        Custom(_) => *new_t == t,
                        //Auto-deref by one level
                        Ref(t) => *new_t == *t,
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}

/// Represents a type, either custom or builtin
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Bool,
    Int,
    Int32,
    Int64,
    /// A custom type. This is anything represented by a struct or enum.
    /// This includes things like `String`, which are not user-defined
    Custom(CustomTypeId),
    /// `& {T}`
    Ref(Box<Type>),
}

impl Type {
    /// Returns the type representing the width of wasm pointers into memory
    pub fn ptr_type() -> Self {
        #[cfg(not(feature = "mem_64bit"))]
        {
            Type::Int64
        }
        #[cfg(feature = "mem_64bit")]
        {
            Type::Int32
        }
    }
}

/// Represents a custom type
#[derive(Debug, Clone)]
pub struct CustomType {
    pub name: String,
    pub mangled_name: Option<String>,
    pub fields: HashMap<String, Type>,
    pub implementation: CustomTypeImpl,
}

#[derive(Debug, Clone, Copy)]
pub enum CustomTypeImpl {
    /// A custom type implemented by the EmScript code and contained within it
    Em,
    /// A type shared between the EmScript and environment, functionality exported by environment
    Shared,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CustomTypeId(pub usize);

impl Display for CustomTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Represents either a type or the name of a type which has yet to be given a name
///
/// For use in AST, before all custom types (especially EmScript defined ones) have been inserted into `CUSTOM_TYPES`
#[derive(Debug, Clone)]
pub enum TypeOrName {
    T(Type),
    Name(String),
}

impl Display for TypeOrName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TypeOrName::T(t) => t.to_string(),
                TypeOrName::Name(s) => s.to_string(),
            }
        )
    }
}

impl TypeOrName {
    pub fn from_str(s: &str) -> Self {
        use Type::*;
        Self::T(match s {
            "bool" => Bool,
            "i32" => Int32,
            "()" => Void,
            _ => return Self::Name(s.to_string()),
        })
    }
}

impl Type {
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
                Type::Void => "()".into(),
                Type::Bool => "bool".into(),
                Type::Int => "{integer}".into(),
                Type::Int32 => "i32".into(),
                Type::Int64 => "i64".into(),
                Type::Custom(id) => format!("CustomType[{id}]"),
                Type::Ref(t) => format!("&{t}"),
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
