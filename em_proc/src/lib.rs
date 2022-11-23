#[macro_use]
extern crate syn;

mod c_ast_assist;
mod generate_translation_with_sizes;

use proc_macro::TokenStream;

/// Parses a method declaration re-statement of the form
/// `fn method_name(Type1, Type2, ..., Typen) -> TypeRet; (size_of::<Type1>(), size_of::<Type2>(), ..., size_of::<Typen>()) -> size_of::<TypeRet>();`
/// and generates a translation layer, a closure which takes wasm-friendly inputs and returns a wasm-friendly output,
/// calling `method_name(..)` internally
///
/// Does not currently support methods using "self". Does not currently support any parameters of a size that is not a multiple of 4 bytes.
/// For any references (which should __never__ be passed to WASM and should only ever be recieved *by* WASM), denote their size as `1` since WASM is 32-bit.
/// All sizes given must be integer literals
///
/// Intended usage is for `[wasmer::sys::externals::Function::new_native_with_env]`, may also work with `[Function::new_native]`
///
/// Example of usage:
/**```
use wasmer::*;

#[derive(WasmerEnv, Clone, Default)]
pub struct WasmEnv {
    #[wasmer(export)]
    pub memory: LazyInit<Memory>,
}
let w_env = WasmEnv::default();

let store = Store::default();
let mut interface = Interface::default();

fn print_c(env: &WasmEnv, s: &str, slice: &[u8]) {
    let mut slice_as_str = String::new();
    slice.clone().read_to_string(&mut slice_as_str).unwrap();
    println!("{s} -- `{slice:?}` -- `{slice_as_str}`",)
}
interface.wasm_imports.push(MethodImport {
    mod_name: "env".to_string(),
    method_name: "print".to_string(),
    f: Function::new_native_with_env(
        &store,
        w_env.clone(),
        generate_translation_with_sizes!(
            fn print_c(&WasmEnv, &str, &[u8]) (1, 1, 1)
        ),
    ),
});

```
**/
#[proc_macro]
pub fn generate_translation_with_sizes(input: TokenStream) -> TokenStream {
    generate_translation_with_sizes::generate_translation_with_sizes(input)
}
