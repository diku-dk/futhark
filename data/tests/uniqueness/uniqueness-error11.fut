-- Make sure occurences are checked inside function parameters as well.
-- ==
-- error:

fun f(x: int): int = x

fun main(): int =
    let n = 10
    let a = iota(n)
    let b = iota(n)
    let (i,j) = (2,5) in
    f((let a[i]=b[j] in 1) + (let b[j]=a[i] in 2))
