-- ==
-- error: "a".*consumed

let f(t: (i32, *[]i64)): i32 =
    let (x, a) = t in
    x

let main(): i64 =
    let n = 10
    let a = iota(n)
    let t = (5, a)
    let c = f(t) in
    a[0]
