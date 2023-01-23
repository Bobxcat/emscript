use std::{
    collections::HashMap,
    ops::{Add, Range},
    sync::Mutex,
};

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

#[derive(Default, Debug, Clone, Copy, Hash, Add, Not, PartialEq, Eq, PartialOrd, Ord)]
struct ChunkIdx(MemoryIndex);

/// The default allocator
///
/// Works by allocating chunks of a certain size when needed
#[derive(Debug, Clone)]
pub struct WAllocatorDefault<const CHUNK_SIZE: MemoryIndex> {
    /// Number of memory chunks currently allocated
    len: ChunkIdx,
    /// The chunk ranges which are currently allocated.
    ///
    /// Sorted in-order, with lower indices corresponding to earlier chunks
    allocated: Vec<Range<ChunkIdx>>,
    // /// For each pair of (len, list):
    // ///
    // /// `list` is the listing of all chunks which have exactly `len`
    // /// free chunks ahead of and including itself
    // freed: HashMap<ChunkIdx, Vec<ChunkIdx>>,
}

impl<const CHUNK_SIZE: MemoryIndex> WAllocatorDefault<CHUNK_SIZE> {
    /// Turns a chunk index to a memory index
    fn chunk_to_mem(&self, chunk: ChunkIdx) -> MemoryIndex {
        chunk.0 * CHUNK_SIZE
    }
    fn grow(&self, env: &WasmEnv, new_chunks: ChunkIdx) {
        //
    }
    /// Searches `allocated` for the given start index. Follows binary search rules, so
    /// returns `Ok(idx)` if the matching start index was found. Otherwise, returns `Err(idx)`, which
    /// is where a range with the given start index could be inserted while maintaining sort order
    fn search_allocated(&self, start_idx: ChunkIdx) -> Result<usize, usize> {
        self.allocated.binary_search_by(|r| r.start.cmp(&start_idx))
    }
    /// Allocates the given range of memory
    fn allocate(&mut self, range: Range<ChunkIdx>) {
        let idx = self.search_allocated(range.start).expect_err(&format!(
            "Tried to allocate existing range of memory. Current memory state: {:#?}",
            self
        ));

        self.allocated.insert(idx, range)
    }
}

impl<const CHUNK_SIZE: MemoryIndex> Default for WAllocatorDefault<CHUNK_SIZE> {
    fn default() -> Self {
        Self {
            len: Default::default(),
            allocated: Default::default(),
            // freed: Default::default(),
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
