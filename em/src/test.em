//fn equals(bool a, bool b) -> bool {
//    a == b
//}

//fn bar(i32 n) -> i32 {
//    n * (n + 1i32 + 2i32) * (n + 2i32)
//}

//fn foo(i32 n) -> i32 {
//    let i32 sum = 1i32;
//    if n >= 2i32 {
//        sum = foo(n - 1i32) + foo(n - 2i32);
//    }
//
//    //print_num(sum);
//
//    sum
//}

fn foo(i32 n) -> i32 {
    let i32 a = 0i32;
    let i32 b = 1i32;

    let i32 i = 1i32;

    loop {
        if i == n {
            break;
        }
        let i32 t = b;
        //b = add(a, b);
        b = a + b;
        a = t;
        
        i = i + 1i32;
    }

    b
}
