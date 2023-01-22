use std::{collections::HashMap, ops::Add, sync::Mutex};

use derive_more::{Add, Not};
use once_cell::sync::Lazy;

use crate::interface::WasmEnv;

/// The type representing an index in WASM memory
#[cfg(not(feature = "mem_64bit"))]
pub type MemoryIndex = u32;

/// The type representing an index in WASM memory
#[cfg(feature = "mem_64bit")]
pub type MemoryIndex = u64;

pub const MEM_ALLOC_NAME: &str = "malloc";
pub const MEM_FREE_NAME: &str = "mfree";
pub const MEM_REALLOC_NAME: &str = "mrealloc";

pub trait WAllocator {
    fn malloc(&mut self, env: &WasmEnv, size: MemoryIndex, align: MemoryIndex) -> MemoryIndex;
    fn mrealloc(&mut self, env: &WasmEnv, loc: MemoryIndex, new_size: MemoryIndex) -> MemoryIndex;
    fn mfree(&mut self, env: &WasmEnv, loc: MemoryIndex, size: MemoryIndex);
}

#[derive(Default, Debug, Clone, Copy, Hash, Add, Not)]
struct ChunkIdx(MemoryIndex);

/// The default allocator
///
/// Works by allocating chunks of a certain size when needed
pub struct WAllocatorDefault<const CHUNK_SIZE: MemoryIndex> {
    /// Number of memory chunks currently allocated
    len: ChunkIdx,
    /// For each pair of (len, list):
    ///
    /// `list` is the listing of all chunks which have exactly `len`
    /// free chunks ahead of and including itself
    freed: HashMap<ChunkIdx, Vec<ChunkIdx>>,
}

impl<const CHUNK_SIZE: MemoryIndex> WAllocatorDefault<CHUNK_SIZE> {
    fn chunk_to_mem(&self, chunk: ChunkIdx) -> MemoryIndex {
        chunk.0 * CHUNK_SIZE
    }
}

impl<const CHUNK_SIZE: MemoryIndex> Default for WAllocatorDefault<CHUNK_SIZE> {
    fn default() -> Self {
        Self {
            len: Default::default(),
            freed: Default::default(),
        }
    }
}

impl<const CHUNK_SIZE: MemoryIndex> WAllocator for WAllocatorDefault<CHUNK_SIZE> {
    fn malloc(&mut self, env: &WasmEnv, size: MemoryIndex, align: MemoryIndex) -> MemoryIndex {
        // let curr_len = ;
        todo!()
    }

    fn mrealloc(&mut self, env: &WasmEnv, loc: MemoryIndex, new_size: MemoryIndex) -> MemoryIndex {
        todo!()
    }

    fn mfree(&mut self, env: &WasmEnv, loc: MemoryIndex, size: MemoryIndex) {
        todo!()
    }
}
