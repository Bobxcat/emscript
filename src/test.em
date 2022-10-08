fn hello_again() -> i32 {
    let a = 2 + 3;
    a
}

fn hello() -> i32 {
    //let a = -2;
    let a = 2 + 3 - {
        //let b = 7;
        let b = 2 * {
            let c = 3;
            c
        } + 1;
        b
    };
    let b = if a != 5 { 5 };
    b
}