fun real f(real   a        ) = a + 3.0
fun real g([real] a, real b) = a[0] * b
fun real h( real  a, real b) = a * b

fun real main([real] arr) =
    let x = map(f, arr)  in
    let y = map(g(x), x) in
    let z = map(h(y[0]), y) in
    z[0]
