-- ==
-- error:
let f(a: *[]i32, i: i32, v: i32): i32 = let a[i]=v in a[i]

let main(): i32 =
    let n = 10
    let a = iota(n)
    let b = a -- a and b are aliases.
    let (i,j) = (2,5) in
    f(a,i,42) -- Consumes a (and b through the alias)
    + b[j] -- Error!
