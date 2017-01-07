-- ==
-- error:
fun f(t: (int, *[]int)): int =
    let (x, a) = t in
    x

fun main(): int =
    let n = 10
    let a = iota(n)
    let t = (5, a)
    let c = f(t) in
    a[0]
