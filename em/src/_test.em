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

fn ex() -> i32 {
    let a = {
        let c = 3;
        let d = 4;
        let b = {{3 + {4 - 2}} + {
            let e = c + d;
            e
        }};
    };
}

let b = {{{1} + {2}}}

//Generated (knows to assign value to `b`)
First:
    int b;
    {Compile({{1} + {2}}, b)}
Second:
    int b;
    { {Compile({1} + {2}, b)} }
Third:
    int b;
    {{
        `int tmp_1, tmp_2;
        { Compile({1}, tmp_1) }
        { Compile({2}, tmp_2) }
        b = tmp_1 + tmp_2;`
    }}
Fourth:
    int b;
    {{
        `int tmp_1, tmp_2;
        { Compile(1, tmp_1) }
        { Compile(2, tmp_2) }
        b = tmp_1 + tmp_2;`
    }}

// output from "+" ast
value tmp_1, tmp_2;
{
    ....
    tmp_1 = 4-2;
}
{
    ...
    tmp_2 = 3;
}

tmp_x = tmp_2 + tmp_1;

type AddNode struct {
    left, right AST
}

func (a *AddNode) Compile(into string) *CAST {
     tmp1 := gensym()
     tmp2 := gensym()
     l := a.left.Compile(tmp1)
     r := b.right.Compile(tmp2)
     return &CAST{
        `value {tmp1}, {tmp2}; // declare C vars for temp results
        { {l} }
        { {r} }
        {into} = {tmp1} + {tmp2}
        `
    }
}

compile(a *ast) *cast {
   dispatch on a's type
   and do the a thing
     recursively
}

// output from assignment
value lhs;
{
    value tmp_1, tmp_2;
    {
        ....
        tmp_1 = 4-2;
    }
    {
        ...
        tmp_2 = 3;
    }
    lhs = tmp_2 + tmp_1;
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