pub mod memory;

/// The path that the `wasm_libraries` project will compile to
pub const WASM_LIBRARIES_PATH: &'static str = {
    #[cfg(debug_assertions)]
    {
        "target/wasm32-unknown-unknown/debug/wasm_libraries.wasm"
    }
    #[cfg(not(debug_assertions))]
    {
        "target/wasm32-unknown-unknown/release/wasm_libraries.wasm"
    }
};
