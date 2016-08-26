-- Make sure occurences are checked inside function parameters as well.
-- ==
-- error:

fun f(x: int): int = x

fun main(): int =
    let n = 10 in
    let a = iota(n) in
    let b = iota(n) in
    let (i,j) = (2,5) in
    f((let a[i]=b[j] in 1) + (let b[j]=a[i] in 2))
