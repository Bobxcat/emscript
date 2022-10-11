fn hello_again() -> i32 {
    let i32 a = 2 + 3;
    a
}

fn hello() -> i32 {
    //let a = -2;
    let i32 a = 2 + 3 - {
        //let b = 7;
        let i32 b = 2 * {
            let i32 c = 3;
            c
        } + 1;
        b
    } + hello_again();
    
    let i32 b = if a != 5 { 5 };
    b
}