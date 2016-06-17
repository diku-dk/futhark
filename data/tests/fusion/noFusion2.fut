-- ==
fun f64 f(f64 a        ) = a + 3.0
fun f64 g(f64 a        ) = a * 3.0

fun f64 main([]f64 arr) =
    let x = map(f, arr) in
    let y = map(f, x)   in
    let z = map(g, x)   in
    y[0] + z[0]
