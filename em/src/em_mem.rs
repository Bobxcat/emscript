use std::{
    ops::{Index, IndexMut},
    ptr::NonNull,
    sync::{Arc, Mutex, MutexGuard},
};

use em_core::memory::MemoryIndex;
use wasmer::{MemoryType, Pages, WASM_PAGE_SIZE};
use wasmer_vm::{LinearMemory, MemoryStyle, VMMemoryDefinition};

#[derive(Debug, Clone)]
pub struct EmMemHandle(Arc<Mutex<EmMem>>);

impl EmMemHandle {
    pub fn new(inner: EmMem) -> Self {
        Self(Arc::new(Mutex::new(inner)))
    }
    pub fn lock<'a>(&'a self) -> MutexGuard<'a, EmMem> {
        self.0.lock().unwrap()
    }
}

impl LinearMemory for EmMemHandle {
    fn ty(&self) -> MemoryType {
        self.lock().ty()
    }

    fn size(&self) -> Pages {
        self.lock().size()
    }

    fn style(&self) -> MemoryStyle {
        self.lock().style()
    }

    fn grow(&mut self, delta: Pages) -> Result<Pages, wasmer::MemoryError> {
        self.lock().grow(delta)
    }

    fn vmmemory(&self) -> NonNull<VMMemoryDefinition> {
        self.lock().vmmemory()
    }

    fn try_clone(&self) -> Option<Box<dyn LinearMemory + 'static>> {
        self.lock().try_clone()
    }
}

#[derive(Debug)]
pub struct EmMem {
    /// The memory allocation
    mem: Vec<u8>,
    /// The pointer to the start of the memory allocation.
    /// Note that this is set every time the vector may reallocate (when it is modified)
    base: *mut u8,
}

impl EmMem {
    pub fn new() -> Self {
        let mut mem = Vec::with_capacity(WASM_PAGE_SIZE);
        let base = mem.as_mut_ptr();

        Self { mem, base }
    }

    fn len(&self) -> Pages {
        Pages((self.mem.len().div_ceil(WASM_PAGE_SIZE)) as u32)
    }

    /// Turns an index of this memory into a reference to some type T.
    ///
    /// This method does not do any sort of checking for validity and it is possible to get
    /// multiple mutable references to the same data using this method.
    ///
    /// Be strongly cautious when using this method
    #[inline(always)]
    pub unsafe fn offset_to_ref<T>(&self, offset: MemoryIndex) -> &mut T {
        let p = self.base.add(offset as usize);
        let p = p as *mut T;
        p.as_mut().unwrap()
    }
}

unsafe impl Send for EmMem {}
unsafe impl Sync for EmMem {}

impl LinearMemory for EmMem {
    fn ty(&self) -> MemoryType {
        MemoryType::new(Pages(0), None, false)
    }

    fn size(&self) -> Pages {
        self.len()
    }

    fn style(&self) -> MemoryStyle {
        MemoryStyle::Dynamic {
            offset_guard_size: u32::MAX as u64,
        }
    }
    fn grow(&mut self, delta: Pages) -> Result<Pages, wasmer::MemoryError> {
        let delta = delta.bytes().0;
        self.mem.append(&mut vec![0; delta]);
        self.base = self.mem.as_mut_ptr();
        Ok(self.size())
    }

    fn vmmemory(&self) -> NonNull<VMMemoryDefinition> {
        let mut def = VMMemoryDefinition {
            base: self.base,
            current_length: self.mem.len(),
        };
        NonNull::new(&mut def).unwrap()
    }

    fn try_clone(&self) -> Option<Box<dyn LinearMemory + 'static>> {
        let mut other_mem = self.mem.clone();
        let other_base = other_mem.as_mut_ptr();

        let other = Self {
            mem: other_mem,
            base: other_base,
        };

        Some(Box::new(other))
    }
}

impl<T> Index<T> for EmMem
where
    Vec<u8>: Index<T>,
{
    type Output = <Vec<u8> as Index<T>>::Output;

    fn index(&self, index: T) -> &Self::Output {
        &self.mem[index]
    }
}

impl<T> IndexMut<T> for EmMem
where
    Vec<u8>: IndexMut<T>,
{
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        &mut self.mem[index]
    }
}
