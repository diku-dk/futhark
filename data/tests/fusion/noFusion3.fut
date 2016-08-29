-- ==
fun f(a: f64): f64 = a + 3.0
fun g(a: []f64) (b: f64): f64 = a[0] * b
fun h(a:  f64) (b: f64): f64 = a * b

fun main(arr: []f64): f64 =
    let x = map f arr 
    let y = map (g(x)) x
    let z = map (h(y[0])) y in
    z[0]
