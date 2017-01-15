-- Make sure occurences are checked inside function parameters as well.
-- ==
-- error:

fun f(x: i32): i32 = x

fun main(): i32 =
    let n = 10
    let a = iota(n)
    let b = iota(n)
    let (i,j) = (2,5) in
    f((let a[i]=b[j] in 1) + (let b[j]=a[i] in 2))
