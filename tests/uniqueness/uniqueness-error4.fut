-- ==
-- error:
fun f(a: *[]int, i: int, v: int): int = let a[i]=v in a[i]

fun main(): int =
    let n = 10
    let a = iota(n)
    let b = a -- a and b are aliases.
    let (i,j) = (2,5) in
    f(a,i,42) -- Consumes a (and b through the alias)
    + b[j] -- Error!
