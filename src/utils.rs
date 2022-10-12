pub const PREFIX_TMP: &str = "_____";
pub const PREFIX_IDENT: &str = "_";

/// Formats `n` as a string using the following values:
/// `0-9`, `A-Z`, `a-z`
///
/// When formatting a `u128`, this means there are more than `u128::MAX + 1` total strings of length 22.
/// Any integer under 161 bits will fit in `27` characters, enough for a temporary var
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

    String::from_utf8(v).unwrap()
}
