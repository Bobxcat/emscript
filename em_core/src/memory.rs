// use derive_more::{Add, AddAssign, From, Neg, Not, Sub, SubAssign};

/// The type representing an index in WASM memory
#[cfg(not(feature = "mem_64bit"))]
pub type MemoryIndex = u32;

/// The type representing an index in WASM memory
#[cfg(feature = "mem_64bit")]
pub type MemoryIndex = u64;

// As of now, the stack allocations are handled by the host, so these var names are not needed
// /// The name of the global variable holding the pointer to the stack in memory
// pub const STACK_PTR_VAR_NAME: &str = "_stack_ptr";
// /// The name of the global variable denoting the maximum size of the stack (in bytes)
// pub const STACK_MAX_SIZE_VAR_NAME: &str = "_stack_max_len";
// /// The name of the global variable denoting the current size of the stack's allocation (in bytes)
// pub const STACK_SIZE_VAR_NAME: &str = "_stack_len";

// ryan is so awesome!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
