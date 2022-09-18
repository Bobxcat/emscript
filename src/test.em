fn my_method() {
    //let a = 10i32;
    //a * 10 / -2;
    let a = 5;

    {
        let b = 10 + a;
        let a = b * 2;
        a
    }
}

my_method()

//let b = my_method();
//b
//This is a comment, to be ignored \n. <<That newline included, and "this" string