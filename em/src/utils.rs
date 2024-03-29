use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    ops::{Index, IndexMut},
    time::SystemTime,
};

use bimap::BiHashMap;
use once_cell::sync::Lazy;

//Note about prefixes:
//- Should have a max length of `10`, so that the formatted variable ID (which has a max length of `22`)
//      still fits within the first `32` characters of the var name, which is when C stops differentiating the var names
//- Shouldn't  start with `_`: https://stackoverflow.com/questions/25090635/use-and-in-c-programs

pub const PREFIX_TMP: &str = "tmp";
pub const PREFIX_BREAKPOINT: &str = "br";
//Prefix identifiers should have the most underscores, since user-provided idents can start with underscores
//(For example, naming your variable `tmp20` shouldn't interfere with anything)
pub const PREFIX_IDENT: &str = "i_________"; //length 10

/// Prefix for host instrinsics, such as `malloc` and `stack_alloc`
pub const PREFIX_HOST_INTRINSIC: &str = "host_";

pub static MEM_ALLOC_NAME: Lazy<String> = Lazy::new(|| format!("{PREFIX_HOST_INTRINSIC}malloc"));
pub const MEM_FREE_NAME: Lazy<String> = Lazy::new(|| format!("{PREFIX_HOST_INTRINSIC}mfree"));
pub const MEM_REALLOC_NAME: Lazy<String> = Lazy::new(|| format!("{PREFIX_HOST_INTRINSIC}mrealloc"));

pub const STACK_ALLOC_NAME: Lazy<String> =
    Lazy::new(|| format!("{PREFIX_HOST_INTRINSIC}stackalloc"));

pub const STACK_POP_NAME: Lazy<String> = Lazy::new(|| format!("{PREFIX_HOST_INTRINSIC}stackpop"));
pub const STACK_POP_MULTIPLE_NAME: Lazy<String> =
    Lazy::new(|| format!("{PREFIX_HOST_INTRINSIC}stackpop_n"));

/// Formats `n` as a base 62 string using the following values:
/// `0-9`, `A-Z`, `a-z`
///
/// When formatting a `u128`, this means there are more than `u128::MAX + 1` total strings of length 22.
/// Any integer under 161 bits will fit in `27` characters, enough for a prefix of length 5
pub fn format_compact(mut n: u128) -> String {
    if n == 0 {
        return "0".into();
    }

    const LETTERS_LEN: u128 = 62;
    const LETTERS: [u8; LETTERS_LEN as usize] =
        *b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    let mut v = Vec::new();

    while n > 0 {
        let c = {
            let idx = n % LETTERS_LEN;
            n /= LETTERS_LEN;
            LETTERS[idx as usize]
        };
        v.push(c);
    }

    // Since the least significant digits are added first, the order must be reversed
    // For large strings, this is less efficient than computing the length with a logarithm
    // and pre-allocating all the memory, setting the values in reverse order
    v.reverse();
    String::from_utf8(v).unwrap()
}

#[allow(unused)]
pub fn time_dbg<T>(f: impl FnOnce() -> T) -> T {
    let start = SystemTime::now();
    let ret = (f)();
    let t = SystemTime::now().duration_since(start).unwrap();
    dbg!(t);

    ret
}

/// A HashMap which stores multiple keys of possibly different types for each value
///
/// Generally, operations using `K` are faster than ones using `V`
#[derive(Clone)]
pub struct MultiMap<K, V, T>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq,
{
    kmap: HashMap<K, T>,
    vmap: BiHashMap<V, K>,
}

impl<K, V, T> Default for MultiMap<K, V, T>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq,
{
    fn default() -> Self {
        Self {
            kmap: Default::default(),
            vmap: Default::default(),
        }
    }
}

impl<K, V, T> MultiMap<K, V, T>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq,
{
    pub fn insert(&mut self, k1: K, k2: V, val: T) -> Option<T> {
        if let Some(t) = self.kmap.insert(k1.clone(), val) {
            return Some(t);
        }

        //It doesn't matter wether or not this insert is `Some(_)` or `None`,
        //since the same will be true for `self.kmap`
        self.vmap.insert(k2, k1);

        None
    }
    pub fn iter_k(&self) -> std::collections::hash_map::Iter<K, T> {
        self.kmap.iter()
    }
    pub fn iter_keys(&self) -> bimap::hash::Iter<V, K> {
        self.vmap.iter()
    }
    #[inline]
    pub fn get_k(&self, k: &K) -> Option<&T> {
        self.kmap.get(k)
    }
    #[inline]
    pub fn get_v(&self, k: &V) -> Option<&T> {
        self.get_k(self.vmap.get_by_left(k)?)
    }
    #[inline]
    pub fn get_k_from_v(&self, k: &V) -> Option<&K> {
        self.vmap.get_by_left(k)
    }
    #[inline]
    pub fn get_v_from_k(&self, k: &K) -> Option<&V> {
        self.vmap.get_by_right(k)
    }
    #[inline]
    pub fn get_k_mut(&mut self, k: &K) -> Option<&mut T> {
        self.kmap.get_mut(k)
    }
    /// Not very efficient
    #[inline]
    pub fn get_v_mut(&mut self, k: &V) -> Option<&mut T> {
        let k = self.vmap.get_by_left(k)?.clone();
        self.get_k_mut(&k)
    }
    pub fn contains_k(&self, k: &K) -> bool {
        self.kmap.contains_key(k)
    }
    pub fn contains_v(&self, k: &V) -> bool {
        self.vmap.contains_left(k)
    }
    pub fn len(&self) -> usize {
        self.kmap.len()
    }
}

impl<K, V, T> Index<&K> for MultiMap<K, V, T>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq,
{
    type Output = T;

    fn index(&self, index: &K) -> &Self::Output {
        &self.kmap[index]
    }
}

impl<K, V, T> IndexMut<&K> for MultiMap<K, V, T>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq,
{
    fn index_mut(&mut self, index: &K) -> &mut Self::Output {
        self.kmap.get_mut(index).unwrap()
    }
}

impl<K, V, T> Debug for MultiMap<K, V, T>
where
    K: Hash + Eq + Clone + Debug,
    V: Hash + Eq + Debug,
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self
            .iter_keys()
            .map(|(v, k)| {
                let t = &self[k];
                format!("[{k:?}, {v:?}, {t:?}]")
            })
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{s}")
    }
}
