fn hello() -> i32 {
    //let a = 3;
    //a = {
    //    let b = 3;
    //    b
    //};
    let a = {
        let b = 3;
        b
    };
    1 + 2 + 3 + 4 - (a + 6)
}

//fn mul(i32 a, i32 b) -> i32 {
//    a * b
//}

//fn sum(i32 start, i32 end) -> i32 {
//    let n = start;
//    if end > start {
//        n = n + sum(start + 1, end)
//    }
//
//    n
//}

//let a = {{let b = 3; b}};

//mul(5i32, 3)
//sum(1, 8)

//This is a comment, to be ignored \n. <<That newline included, and "this" string