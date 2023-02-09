use std::fmt::Display;

use crate::value::Type;

///.
///
/// examples of identifiers:
///
/// `core::iter::RangeIter::<i64>`
///
/// `var.field0.field1`
///
/// `var.field.f()`
///
/// (but not this) `RangeIter::<i64>::new().field.`
///
///.
///

/// The name of an identifier, including its path
pub struct IdentPath {
    name: String,
    /// The first part of the path, describing the type:
    type_path: Vec<String>,
    fields: Vec<String>,
}

impl IdentPath {
    pub fn new(v: Vec<String>) -> Self {
        todo!()
    }
    //
}

impl Display for IdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.sections.join("::"))
    }
}

#[derive(Debug, Clone)]
pub struct TypePath {
    segments: Vec<TypePathSegment>,
}

#[derive(Debug, Clone)]
pub struct TypePathSegment {
    name: String,
    info: TypePathSegmentInfo,
}

/// Extra information regarding the segment of a type path
#[derive(Debug, Clone)]
pub enum TypePathSegmentInfo {
    None,
    Generic(Type),
}
