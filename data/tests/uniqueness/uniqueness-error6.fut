-- ==
-- error:
fun int f((int, *[int]) t) =
    let (x, a) = t in
    x

fun int main() =
    let n = 10 in
    let a = iota(n) in
    let t = (5, a) in
    let c = f(t) in
    a[0]
