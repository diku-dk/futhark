-- ==
fun f64 f(f64   a        ) = a + 3.0
fun f64 g([]f64 a, f64 b) = a[0] * b
fun f64 h( f64  a, f64 b) = a * b

fun f64 main([]f64 arr) =
    let x = map(f, arr)  in
    let y = map(g(x), x) in
    let z = map(h(y[0]), y) in
    z[0]
