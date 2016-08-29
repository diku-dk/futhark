-- Test whether multiple references within the same sequence are
-- detected.
-- ==
-- error:

fun main(): int =
    let n = 10
    let a = iota(n)
    let b = iota(n)
    let (i,j) = (2,5) in
    (let a[i]=b[j] in 1) + (let b[j]=a[i] in 2) -- Error!
