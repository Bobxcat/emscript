use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use bimap::BiMap;

pub const PREFIX_TMP: &str = "_tmp";
//Prefix identifiers should have the most underscores, since user-provided idents can have
pub const PREFIX_IDENT: &str = "_____";

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

// /// A HashMap which stores multiple keys of possibly different types for each value
// #[derive(Clone, Debug)]
// pub struct MultiMap<K, V, T>
// where
//     K: Hash + Eq,
//     V: Hash + Eq,
// {
//     kmap: HashMap<V, Rc<K>>,
//     vmap: HashMap<Rc<K>, T>,
// }

// impl<K, V, T> MultiMap<K, V, T>
// where
//     K: Hash + Eq,
//     V: Hash + Eq,
// {
//     pub fn insert(&mut self, k1: K, k2: V, val: T) -> Option<T> {
//         let k1 = Rc::new(k1);
//         //It doesn't matter wether or not this insert is `Some(_)` or `None`,
//         //since the same will be true for `self.vmap`
//         self.kmap.insert(k2, k1);
//         self.vmap.insert(k1, val)
//     }
//     pub fn get_k1(&self, k: K) -> Option<&T> {

//     }
// }