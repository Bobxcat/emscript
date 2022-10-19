use std::collections::HashMap;

use crate::value::{Type, Value};

pub struct MethodId(usize);

pub struct MethodExport {
    name: String,
    params: Type,
    return_type: Type,
    callback: Box<dyn Fn(&[Value]) -> Value>,
}

pub struct Interface {
    methods_export: HashMap<MethodId, MethodExport>,
    methods_import: HashMap<MethodId>,
}

pub fn compile_api(raw: &str) -> Interface {
    todo!()
}
