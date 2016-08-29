-- ==
-- error:
fun f(a: *[][]int): int = a[0,0]

fun main(): int =
    let n = 10
    let a = copy(replicate(n, iota(n)))
    let c = transpose(a) in -- Transpose creates an alias.
    f(a) + c[0,0] -- f(a) consumes both a and c, so error.
