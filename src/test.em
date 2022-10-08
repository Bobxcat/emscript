fn hello() -> bool {
    let a = 2 + 3 - {
        let b = 2 * {let c = 3; c} + 1;
        b
        };
    a
}