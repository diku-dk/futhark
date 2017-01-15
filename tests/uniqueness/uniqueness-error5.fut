-- ==
-- error: .*consumed.*
fun f(a: *[][]i32): i32 = a[0,0]

fun main(): i32 =
    let n = 10
    let a = copy(replicate n (iota n))
    let c = transpose(a) in -- Transpose creates an alias.
    f(a) + c[0,0] -- f(a) consumes both a and c, so error.
