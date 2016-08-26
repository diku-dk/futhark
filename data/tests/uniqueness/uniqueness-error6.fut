-- ==
-- error:
fun f(t: (int, *[]int)): int =
    let (x, a) = t in
    x

fun main(): int =
    let n = 10 in
    let a = iota(n) in
    let t = (5, a) in
    let c = f(t) in
    a[0]
