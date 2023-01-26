/*!
 *
 * A library dedicated to WASM code representing different core EmScript types and functionality
 *
 *
*/

#![no_std]
// #![crate_type = "staticlib"]
#![feature(step_trait)]

pub mod iter;

use core::iter::Step;

use em_core::{memory::MemoryIndex, *};

extern "C" {
    pub fn malloc(size: MemoryIndex, align: MemoryIndex) -> MemoryIndex;
    pub fn mrealloc(loc: MemoryIndex, new_size: MemoryIndex) -> MemoryIndex;
    pub fn mfree(loc: MemoryIndex, size: MemoryIndex);
}

#[no_mangle]
pub extern "C" fn lib_foo(a: u32, b: u32) {
    unsafe {
        mfree(a + b, b - a);
    }
}
