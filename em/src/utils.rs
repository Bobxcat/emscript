use std::{
    cell::RefCell,
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
    rc::Rc,
};

use bimap::BiMap;

//Note about prefixes:
//- Should have a max length of `10`, so that the formatted variable ID (which has a max length of `22`)
//      still fits within the first `32` characters of the var name, which is when C stops differentiating the var name

pub const PREFIX_TMP: &str = "_tmp";
//Prefix identifiers should have the most underscores, since user-provided idents can start with underscores
//(For example, naming your variable `_tmp20` shouldn't interfere with anything)
pub const PREFIX_IDENT: &str = "__________"; //10 underscores

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

/// A HashMap which stores multiple keys of possibly different types for each value
///
/// Generally, operations using `K` are faster than ones using `V`
#[derive(Clone, Debug)]
pub struct MultiMap<K, V, T>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq,
{
    kmap: HashMap<K, T>,
    vmap: BiMap<V, K>,
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
        self.kmap.insert(k1.clone(), val)?;

        //It doesn't matter wether or not this insert is `Some(_)` or `None`,
        //since the same will be true for `self.kmap`
        self.vmap.insert(k2, k1);

        None
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
