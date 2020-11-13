-- ==
-- error: .*consumed.*
let f(a: *[][]i64): i64 = a[0,0]

let main(): i32 =
    let n = 10
    let a = replicate n (iota n)
    let c = transpose a in -- Rearrange creates an alias.
    f(a) + c[0,0] -- f(a) consumes both a and c, so error.
