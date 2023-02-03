use std::ops::{Range, RangeInclusive};

use derive_more::{Add, AddAssign, From, Not, Sub, SubAssign};

use em_core::memory::MemoryIndex;

use wasmer::FunctionEnvMut;
use wasmer_vm::LinearMemory;

use crate::WasmEnv;

/// The size of the stack, which does not grow
///
/// Currently set to `1 MB`
pub const STACK_SIZE: MemoryIndex = 1024 * 1024;

const PAGE_SIZE: u64 = u32::MAX as u64 + 1;

pub fn wasm_value_to_mem_idx(val: wasmer::Value) -> MemoryIndex {
    #[cfg(not(feature = "mem_64bit"))]
    {
        unsafe { std::mem::transmute(val.unwrap_i32()) }
    }
    #[cfg(feature = "mem_64bit")]
    {
        unsafe { std::mem::transmute(val.unwrap_i64()) }
    }
}

pub fn mem_idx_to_wasm_value(idx: MemoryIndex) -> wasmer::Value {
    #[cfg(not(feature = "mem_64bit"))]
    {
        wasmer::Value::I32(unsafe { std::mem::transmute(idx) })
    }
    #[cfg(feature = "mem_64bit")]
    {
        wasmer::Value::I64(unsafe { std::mem::transmute(idx) })
    }
}

/// A range of chunks representing `first..=last`

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct ChunkRange {
    first: ChunkIdx,
    last: ChunkIdx,
}

impl ChunkRange {
    pub fn new(first: ChunkIdx, last: ChunkIdx) -> Self {
        Self { first, last }
    }
    /// Gets the number of chunks within this range
    pub fn len(&self) -> ChunkIdx {
        self.last - self.first + 1.into()
    }
}

impl From<Range<ChunkIdx>> for ChunkRange {
    fn from(value: Range<ChunkIdx>) -> Self {
        Self {
            first: value.start,
            last: value.end - 1.into(),
        }
    }
}

impl From<RangeInclusive<ChunkIdx>> for ChunkRange {
    fn from(value: RangeInclusive<ChunkIdx>) -> Self {
        Self {
            first: *value.start(),
            last: *value.end(),
        }
    }
}

pub trait WAllocator<const BASE: MemoryIndex> {
    /// Allocates some memory of `size` contiguous bits with the given align.
    ///
    /// Returns the index of the first byte of allocated memory
    fn malloc(
        &mut self,
        env: FunctionEnvMut<WasmEnv>,
        size: MemoryIndex,
        align: MemoryIndex,
    ) -> MemoryIndex;
    /// Given an allocated piece of memory with the given starting position, increase its size to `new_size`, moving the allocation if needed
    ///
    /// Returns the new index of the first bytes of the allocation. This may or may not be different than `loc`
    fn mrealloc(
        &mut self,
        env: FunctionEnvMut<WasmEnv>,
        loc: MemoryIndex,
        new_size: MemoryIndex,
    ) -> MemoryIndex;
    /// Frees the given allocated memory, so that it may be used by in a future allocation
    fn mfree(&mut self, env: FunctionEnvMut<WasmEnv>, loc: MemoryIndex, size: MemoryIndex);
}

#[derive(
    Default,
    Debug,
    Clone,
    Copy,
    Hash,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Not,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    From,
)]
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
    allocated: Vec<ChunkRange>,
    /// Each element of this list represents a range of memory which has been allocated and then freed.
    ///
    /// It is possible for two chunks to be directly next to each other on this list (i.e. freed[i].last == freed[i + 1].start - 1 can happen)
    freed: Vec<ChunkRange>,
}

impl<const CHUNK_SIZE: MemoryIndex> Default for WAllocatorDefault<CHUNK_SIZE> {
    fn default() -> Self {
        Self {
            len: Default::default(),
            allocated: Default::default(),
            freed: Default::default(),
        }
    }
}

impl<const CHUNK_SIZE: MemoryIndex> WAllocatorDefault<CHUNK_SIZE> {
    /// Turns a chunk index to a memory index
    fn chunk_to_mem(&self, chunk: ChunkIdx) -> MemoryIndex {
        chunk.0 * CHUNK_SIZE
    }
    /// Grows the length of available memory by `new_chunks`, ensuring that the wasm memory
    /// has at least that many bytes
    fn grow(&mut self, env: FunctionEnvMut<WasmEnv>, new_chunks: ChunkIdx) {
        self.len += new_chunks;
        let len_bytes = (self.len.0 * CHUNK_SIZE) as u64;

        let mut store = WasmEnv::store();
        let store = store.as_mut().unwrap();

        let mut mem = env.data().memory.lock();

        // let view = mem.view(store);
        while len_bytes < mem.size().bytes().0 as u64 {
            mem.grow(1.into()).expect("Memory failed to grow");
        }
    }
    /// Searches `allocated` for the given start index. Follows binary search rules, so
    /// returns `Ok(idx)` if the matching start index was found. Otherwise, returns `Err(idx)`, which
    /// is where a range with the given start index could be inserted while maintaining sort order
    fn search_allocated(&self, start_idx: ChunkIdx) -> Result<usize, usize> {
        self.allocated.binary_search_by(|r| r.first.cmp(&start_idx))
    }
    /// Searches `freed` for the given start index. Follows binary search rules, so
    /// returns `Ok(idx)` if the matching start index was found. Otherwise, returns `Err(idx)`, which
    /// is where a range with the given start index could be inserted while maintaining sort order
    fn search_freed(&self, start_idx: ChunkIdx) -> Result<usize, usize> {
        self.freed.binary_search_by(|r| r.first.cmp(&start_idx))
    }
    /// Allocates the given range of memory
    fn allocate(&mut self, range: ChunkRange) {
        let idx = self.search_allocated(range.first).expect_err(&format!(
            "Tried to allocate existing range of memory. Current memory state: {:#?}",
            self
        ));

        self.allocated.insert(idx, range)
    }
    fn free(&mut self, range: ChunkRange) {
        let mem = self
            .search_allocated(range.first)
            .expect("memory to be freed was not allocated");
        if self.allocated[mem].last != range.last {
            panic!("Attempted to free memory with unexpected length");
        }

        self.allocated.remove(mem);

        // Insert the freed range of memory
        let insertion_index = self
            .freed
            .binary_search_by(|r| r.first.cmp(&range.first))
            .expect_err("Freed range of memory which was already freed");

        self.freed.insert(insertion_index, range);
    }
}

impl<const CHUNK_SIZE: MemoryIndex, const BASE: MemoryIndex> WAllocator<BASE>
    for WAllocatorDefault<CHUNK_SIZE>
{
    fn malloc(
        &mut self,
        env: FunctionEnvMut<WasmEnv>,
        size: MemoryIndex,
        align: MemoryIndex,
    ) -> MemoryIndex {
        // Number of chunks needed by the allocation, not accounting for alignment concerns
        let num_chunks = size.div_ceil(CHUNK_SIZE);

        // Look for freed chunks to populate
        for (i, freed) in self.freed.clone().iter().enumerate() {
            // If the freed range is long enough according to `num_chunks`, check if it's long enough including alignment
            let freed_len = freed.len();
            if freed_len.0 > num_chunks {
                continue;
            }

            let freed_start = self.chunk_to_mem(freed_len);
            // The offset which will need to be added to `freed` in order to fulfill the alignment
            let offset = freed_start % align;

            // The number of bytes, starting from the first allocated chunk, which will be needed to allocate enough memory
            let size_plus_offset = size + offset;
            // The final calculation for the number of chunks required to allocate this memory
            let num_chunks = size_plus_offset.div_ceil(CHUNK_SIZE);

            // If the number of free contiguous chunks is sufficient, then update `freed` and allocate. Then, return including the offset
            if num_chunks >= freed_len.0 {
                self.freed[i].first.0 += num_chunks;
                if self.freed[i].first >= self.freed[i].last - ChunkIdx(1) {
                    self.freed.remove(i);
                }
                self.allocate(freed.clone());
                return freed_start + offset;
            }
        }

        // If none of the freed chunks were large enough, allocate by growing the memory instance by `num_chunks`, including offset
        let loc = self.chunk_to_mem(self.len);
        let offset = loc % align;

        // Get the number of chunks to be allocated
        let num_chunks = (size + offset).div_ceil(CHUNK_SIZE);

        // Grow the memory by the proper number of chunks
        self.grow(env, num_chunks.into());

        // Finally, return the index in memory that was allocated
        loc + offset
    }

    fn mrealloc(
        &mut self,
        _env: FunctionEnvMut<WasmEnv>,
        loc: MemoryIndex,
        new_size: MemoryIndex,
    ) -> MemoryIndex {
        let loc_chunk: ChunkIdx = ((loc) / CHUNK_SIZE).into();
        // Get the correct allocation
        let alloc_idx = self
            .search_allocated(loc_chunk)
            .expect("Tried to call `mrealloc` on memory which was not already allocated");

        // Search through the chunks after `alloc_idx` to see if there is enough room already.
        // Otherwise, just allocate the whole chunk again and memcpy the values

        let new_size_chunks: ChunkIdx = ((loc % CHUNK_SIZE + new_size).div_ceil(CHUNK_SIZE)).into();

        let curr_end_idx = self.allocated[alloc_idx].last;

        // Use `self.search_freed(..)` until enough freed chunks have been found.
        // Do NOT allocate these freed chunks yet, since it's possible that not enough
        // freed chunks are available

        let mut freeable_chunks = Vec::new();

        // The number of chunks which are still needed for growing this memory portion to be possible
        let mut remaining_chunks = new_size_chunks - (self.allocated[alloc_idx].len());

        while remaining_chunks > 0.into() {
            match self.search_freed(curr_end_idx + 1.into()) {
                Ok(freed_idx) => {
                    let freed_range = self.freed[freed_idx];
                    remaining_chunks =
                        (remaining_chunks.0.saturating_sub(freed_range.len().0)).into();
                    freeable_chunks.push(freed_idx);
                    // If enough chunks have been found in a row, break out of the loop
                    if remaining_chunks == 0.into() {
                        break;
                    }
                }
                // If there was no freed chunk at this index, break out of the loop
                Err(_) => {
                    break;
                }
            }
        }

        todo!()
    }

    fn mfree(&mut self, _env: FunctionEnvMut<WasmEnv>, loc: MemoryIndex, size: MemoryIndex) {
        // The chunk containing `loc`
        let chunk_start: ChunkIdx = ((loc) / CHUNK_SIZE).into();
        // The chunk containing `loc + size`
        let chunk_end: ChunkIdx = ((loc + size) / CHUNK_SIZE).into();

        let _idx = self
            .search_allocated(chunk_start)
            .expect("Tried to free non-allocated memory chunk");

        // Generate the range of chunks which need to be freed
        let chunks = chunk_start..=chunk_end;
        let _chunks: ChunkRange = chunks.into();

        // In the case of one call freeing multiple allocations, this is not quite right
        // assert!(chunks.len() >= self.allocated[idx].len());

        // Free chunks starting at
        // self.free(chunks)

        //
        todo!()
    }
}

/// Represents the allocator used to control stack allocations.
/// This struct is given some number of contiguous bytes pre-allocated at the start of the memory
pub(crate) struct StackAllocator {
    /// The stack size in bytes
    stack_size: MemoryIndex,
    /// The active allocations on the stack
    allocations: Vec<RangeInclusive<MemoryIndex>>,
    /// The current end of the stack pointer
    stack_ptr: MemoryIndex,
}

impl StackAllocator {
    pub fn new(stack_size: MemoryIndex) -> Self {
        Self {
            stack_size,
            allocations: Vec::new(),
            stack_ptr: 0,
        }
    }
    pub const fn stack_ptr(&self) -> MemoryIndex {
        self.stack_ptr
    }
    /// Allocates `size` contiguous bytes with the given alignment and returns
    /// a pointer to the start of the allocation
    pub fn alloc(&mut self, size: MemoryIndex, align: MemoryIndex) -> MemoryIndex {
        let offset = self.stack_ptr % align;

        let alloc_start = self.stack_ptr + offset;
        let allocation = alloc_start..=(alloc_start + size);

        self.allocations.push(allocation);

        alloc_start
    }
    /// Removes the most recent allocation on the stack
    pub fn pop(&mut self) {
        let allocation = self
            .allocations
            .pop()
            .expect("Popped value off of empty stack");

        let allocation_start = *allocation.start();
        self.stack_ptr = allocation_start;
    }
    /// Removes the `n` most recent allocations on the stack
    pub fn pop_n(&mut self, n: MemoryIndex) {
        for _ in 0..n {
            self.pop();
        }
    }
    /// Invalidates all active allocations and resets the stack_ptr to 0.
    /// This is a fairly cheap operation
    pub fn pop_all(&mut self) {
        self.allocations.clear();
        self.stack_ptr = 0;
    }
}
