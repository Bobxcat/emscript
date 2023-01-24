/*!
 *
 * A library dedicated to WASM code representing different core EmScript types and functionality
 *
 *
*/

#![no_std]
#![crate_type = "staticlib"]

use emscript::memory::MemoryIndex;

extern "C" {
    fn malloc(size: MemoryIndex, align: MemoryIndex) -> MemoryIndex;
}

pub extern "C" fn lib_foo(a: i32, b: i32) {
    // WAllocator
}

#[repr(C)]
struct Iterator {
    //
}
