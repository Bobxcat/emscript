use std::{
    ffi::CStr,
    ptr::{self, slice_from_raw_parts_mut, Pointee},
};

use wasmer::{Memory, WasmPtr};

#[inline]
pub unsafe fn wasm_ptr_as_ref<'a, T>(offset: usize, mem: &'a Memory) -> &'a T
where
    T: Sized,
{
    wasm_ptr_as_ref_with_size(offset, mem, ())
}

#[inline]
pub unsafe fn wasm_ptr_as_ref_mut<'a, T>(offset: usize, mem: &'a Memory) -> &'a mut T
where
    T: Sized,
{
    wasm_ptr_as_ref_with_size_mut(offset, mem, ())
}

/// See [Pointee] for more details on `meta`
pub unsafe fn wasm_ptr_as_ref_with_size<'a, T>(
    offset: usize,
    mem: &'a Memory,
    meta: <T as Pointee>::Metadata,
) -> &'a T
where
    T: ?Sized,
{
    let view = mem.view::<u8>();

    unsafe {
        let val_start = view[offset].as_ptr() as *const ();
        let as_t: *const T = ptr::from_raw_parts(val_start, meta);

        as_t.as_ref().unwrap()
    }
}

/// See [Pointee] for more details on `meta`
pub unsafe fn wasm_ptr_as_ref_with_size_mut<'a, T>(
    offset: usize,
    mem: &'a Memory,
    meta: <T as Pointee>::Metadata,
) -> &'a mut T
where
    T: ?Sized,
{
    let view = mem.view::<u8>();

    unsafe {
        let val_start = view[offset].as_ptr() as *const ();
        let as_t: *mut T = ptr::from_raw_parts::<T>(val_start, meta) as *mut T;

        as_t.as_mut().unwrap()
    }
}

impl<T> GetRefFromMem for [T] {
    fn as_mem_ref_mut<'a>(offset: usize, mem: &'a Memory) -> Option<&'a mut Self> {
        //The initial pointer is to a region of memory structured like so: [ptr_to_start: u32, length: u32]
        let view = mem.view::<u8>();

        #[repr(C)]
        struct CustomSlice {
            ptr: WasmPtr<u8>,
            len: u32,
        }

        unsafe {
            let custom_slice: &CustomSlice = wasm_ptr_as_ref(offset, mem);
            let start = view[custom_slice.ptr.offset() as usize].as_ptr() as *mut T;

            Some(&mut *slice_from_raw_parts_mut(
                start,
                custom_slice.len as usize,
            ))
        }
    }
}

impl GetRefFromMem for str {
    fn as_mem_ref_mut<'a>(offset: usize, mem: &'a Memory) -> Option<&'a mut Self> {
        let view = mem.view::<u8>();

        //Right now, not especially efficient
        unsafe {
            let val_start = view[offset].as_ptr() as *const i8;
            let s = CStr::from_ptr(val_start).to_bytes();
            let s_len = s.len();

            let s = wasm_ptr_as_ref_with_size_mut(offset, mem, s_len);
            Some(s)
        }
    }
}

/// A trait which defines that an object of type `&T` can be constructed from a `WasmPtr` to corresponding C type.
/// Say that `T` has a corresponding C representation of `struct { .. } c_repr`. Implementing this trait for `T` means
/// mapping a `c_repr *` to a `&T`.
///
/// For example:
/// * The definition for slices in C is `struct { void *c; u32 len; }`, so the implementation for `&[T]` expects a pointer to this
/// memory layout and constructs a slice using these two components
/// * `&str` corresponds to `char *` in C, so the implementation for `&str` starts at the pointer and goes forward until finding a null character
/// which signifies the termination of the `&str`
///
/// Be VERY CAREFUL when implementing this trait, as a *lot* can go wrong (such as circular references -- avoid those).
/// One tip for implementation is to create intermediate structs for different levels of dereferencing. For example,
/// this is an implementation for `&[T]`:
/// ```
/// fn as_mem_ref_mut<'a>(offset: usize, mem: &'a Memory) -> Option<&'a mut [T]> {
///     //The initial pointer is to a region of memory structured like so: [ptr_to_start: u32, length: u32]
///     let view = mem.view::<u8>();
///
///     #[repr(C)]
///     struct CustomSlice {
///         ptr: WasmPtr<u8>,
///         len: u32,
///     }
///     unsafe {
///         let custom_slice: &CustomSlice = wasm_ptr_as_ref(offset, mem);
///         let start = view[custom_slice.ptr.offset() as usize].as_ptr() as *mut T;
///
///         Some(&mut *slice_from_raw_parts_mut(
///             start,
///             custom_slice.len as usize,
///         ))
///     }
/// }
/// ```
pub trait GetRefFromMem {
    #[inline]
    fn as_mem_ref<'a>(offset: usize, mem: &'a Memory) -> Option<&'a Self> {
        Some(&*Self::as_mem_ref_mut(offset, mem)?)
    }
    fn as_mem_ref_mut<'a>(offset: usize, mem: &'a Memory) -> Option<&'a mut Self>;

    #[inline]
    fn ref_from_wasm_ptr<'a, _G>(p: WasmPtr<_G>, mem: &'a Memory) -> Option<&'a Self>
    where
        _G: Copy,
    {
        Some(&*Self::as_mem_ref_mut(p.offset() as usize, mem)?)
    }
    #[inline]
    fn ref_from_wasm_ptr_mut<'a, _G>(p: WasmPtr<_G>, mem: &'a Memory) -> Option<&'a mut Self>
    where
        _G: Copy,
    {
        Self::as_mem_ref_mut(p.offset() as usize, mem)
    }
}
