// use std::{
//     ops::{Index, IndexMut},
//     ptr::NonNull,
//     sync::{Arc, Mutex, MutexGuard},
// };

// use em_core::memory::MemoryIndex;
// use wasmer::{MemoryType, Pages, WASM_PAGE_SIZE};

// #[derive(Debug, Clone)]
// pub struct EmMemHandle(Arc<Mutex<EmMem>>);

// impl EmMemHandle {
//     pub fn new(inner: EmMem) -> Self {
//         Self(Arc::new(Mutex::new(inner)))
//     }
//     pub fn lock<'a>(&'a self) -> MutexGuard<'a, EmMem> {
//         // let tr = std::backtrace::Backtrace::capture();
//         // println!("{}", tr);
//         let l = self.0.lock().unwrap();
//         l
//     }
// }

// impl LinearMemory for EmMemHandle {
//     fn ty(&self) -> MemoryType {
//         self.lock().ty()
//     }

//     fn size(&self) -> Pages {
//         self.lock().size()
//     }

//     fn style(&self) -> MemoryStyle {
//         self.lock().style()
//     }

//     fn grow(&mut self, delta: Pages) -> Result<Pages, wasmer::MemoryError> {
//         self.lock().grow(delta)
//     }

//     fn vmmemory(&self) -> NonNull<VMMemoryDefinition> {
//         self.lock().vmmemory()
//     }

//     fn try_clone(&self) -> Option<Box<dyn LinearMemory + 'static>> {
//         self.lock().try_clone()
//     }
// }

// const EM_MEM_LEN: usize = (u32::MAX as usize);

// #[derive(Debug)]
// pub struct EmMem {
//     map: Mmap,
// }

// impl EmMem {
//     pub fn new() -> Self {
//         let map = Mmap::with_at_least(EM_MEM_LEN).unwrap();
//         // let mut map = Mmap::new();
//         // map.as_mut_ptr();
//         // map.make_accessible(0, Pages(1).bytes().0).unwrap();
//         Self { map }
//     }

//     fn base(&self) -> *mut u8 {
//         self.map.as_ptr() as *mut u8
//     }

//     fn len(&self) -> Pages {
//         Pages((self.map.len().div_ceil(WASM_PAGE_SIZE)) as u32)
//         // Pages((self.mem.len().div_ceil(WASM_PAGE_SIZE)) as u32)
//     }

//     /// Turns an index of this memory into a reference to some type T.
//     ///
//     /// This method does not do any sort of checking for validity and it is possible to get
//     /// multiple mutable references to the same data using this method.
//     ///
//     /// Be strongly cautious when using this method
//     #[inline(always)]
//     pub unsafe fn offset_to_ref<T>(&self, offset: MemoryIndex) -> &mut T {
//         let p = self.base().add(offset as usize);
//         let p = p as *mut T;
//         p.as_mut().unwrap()
//     }
// }

// unsafe impl Send for EmMem {}
// unsafe impl Sync for EmMem {}

// impl LinearMemory for EmMem {
//     fn ty(&self) -> MemoryType {
//         MemoryType::new(Pages(0), None, true)
//     }

//     fn size(&self) -> Pages {
//         self.len()
//     }

//     fn style(&self) -> MemoryStyle {
//         MemoryStyle::Dynamic {
//             offset_guard_size: u32::MAX as u64 / 2 + 1,
//         }
//     }

//     fn grow(&mut self, delta: Pages) -> Result<Pages, wasmer::MemoryError> {
//         Ok(self.len())

//         // let start = unsafe { self.base().add(self.len().bytes().0) as usize };

//         // println!("base: {}\nstart: {}", self.base() as usize, start);

//         // self.map.make_accessible(start, delta.bytes().0).unwrap();

//         // Ok(self.len())

//         // Ok(self.len())
//         // // println!("Growing by: {} pages ({} bytes)", delta.0, delta.bytes().0);
//         // let delta = delta.bytes().0;
//         // self.mem.append(&mut vec![0; delta]);
//         // self.base = self.mem.as_mut_ptr();
//         // Ok(self.size())
//     }

//     fn vmmemory(&self) -> NonNull<VMMemoryDefinition> {
//         // let mut def = VMMemoryDefinition {
//         //     base: self.base,
//         //     current_length: self.mem.len(),
//         // };

//         let mut def = VMMemoryDefinition {
//             base: self.base(),
//             current_length: self.map.len(),
//         };
//         NonNull::new(&mut def).unwrap()
//     }

//     fn try_clone(&self) -> Option<Box<dyn LinearMemory + 'static>> {
//         None

//         // let mut other_mem = self.mem.clone();
//         // let other_base = other_mem.as_mut_ptr();

//         // let other = Self { map: self.map};

//         // let other = Self {
//         //     mem: other_mem,
//         //     base: other_base,
//         // };

//         // Some(Box::new(other))
//     }
// }

// impl<T> Index<T> for EmMem
// where
//     Mmap: Index<T>,
//     // Vec<u8>: Index<T>,
// {
//     type Output = <Mmap as Index<T>>::Output;
//     // type Output = <Vec<u8> as Index<T>>::Output;

//     fn index(&self, index: T) -> &Self::Output {
//         &self.map[index]
//     }
// }

// impl<T> IndexMut<T> for EmMem
// where
//     Mmap: IndexMut<T>,
//     // Vec<u8>: IndexMut<T>,
// {
//     fn index_mut(&mut self, index: T) -> &mut Self::Output {
//         &mut self.map[index]
//     }
// }
