fn hello_again(i32 a) -> i32 {
    let i32 b = 2 + a;
    b
}

fn hello() -> i32 {
    //a = -4
    let i32 a = 1 + 2 - {
        let i32 b = 7;
        b
    };
    print({
        let i32 b = 2;
        b = b + a;
        b - a
    });
    //a = -6
    a = a + hello_again(a);
    //return -3
    a / 2
}