// use derive_more::{Add, AddAssign, From, Neg, Not, Sub, SubAssign};

/// The type representing an index in WASM memory
#[cfg(not(feature = "mem_64bit"))]
pub type MemoryIndex = u32;

/// The type representing an index in WASM memory
#[cfg(feature = "mem_64bit")]
pub type MemoryIndex = u64;
