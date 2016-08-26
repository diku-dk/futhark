-- Test whether multiple references within the same sequence are
-- detected.
-- ==
-- error:

fun main(): int =
    let n = 10 in
    let a = iota(n) in
    let b = iota(n) in
    let (i,j) = (2,5) in
    (let a[i]=b[j] in 1) + (let b[j]=a[i] in 2) -- Error!
