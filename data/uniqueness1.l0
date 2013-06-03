// Test that simple function argument consumption works.

fun int f(*[int] a) = a[0]

fun int main() =
    let n = 10 in
    let b = iota(n) in
    let a = b in // Alias a to b.
    let x = f(b) in // Consumes both b and a because a is aliased to b.
    0 // OK!
