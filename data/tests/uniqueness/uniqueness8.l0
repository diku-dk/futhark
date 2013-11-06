fun int f(*[int] a) = a[0]
fun int g([int] a) = a[0]

fun int main() =
    let n = 10 in
    let a = iota(n) in
    let b = a in
    if 1 = 2 then let c = g(b) in f(a) + c
             else let c = g(a) in f(b) // OK Because only one branch is taken.
