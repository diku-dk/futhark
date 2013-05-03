fun int f(*[[int]] a) = a[0,0]

fun int main() =
    let n = 10 in
    let a = replicate(n, iota(n)) in
    let c = transpose(a) in // Transpose creates an alias.
    f(a) + c[0,0] // f(a) consumes both a and c, so error.
