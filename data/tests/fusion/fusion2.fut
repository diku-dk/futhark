-- ==
-- input {
--   [1.0,2.0,3.0,4.0]
-- }
-- output {
--   73.000000
-- }
fun f(a: f64): f64 = a + 3.0
fun g(a: f64): f64 = a * 3.0
fun h(x: f64) (y: (f64,f64)): f64 = let (a,b) = y in a * b - (a + b) + x

fun main(arr: []f64): f64 =
    let b = map(f, arr) in
    let x = map(f, b)   in
    let y = map(g, b)   in
    let z = map(h(x[1]), zip(x,y)) in
    z[0] --+ y[0]
