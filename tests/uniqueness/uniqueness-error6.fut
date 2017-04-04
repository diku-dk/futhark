-- ==
-- error:
let f(t: (i32, *[]i32)): i32 =
    let (x, a) = t in
    x

let main(): i32 =
    let n = 10
    let a = iota(n)
    let t = (5, a)
    let c = f(t) in
    a[0]
