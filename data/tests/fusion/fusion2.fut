-- ==
-- input {
--   [1.0,2.0,3.0,4.0]
-- }
-- output {
--   73.000000
-- }
fun f64 f(f64 a        ) = a + 3.0
fun f64 g(f64 a        ) = a * 3.0
fun f64 h(f64 x, (f64,f64) y) = let (a,b) = y in a * b - (a + b) + x

fun f64 main([f64] arr) =
    let b = map(f, arr) in
    let x = map(f, b)   in
    let y = map(g, b)   in
    let z = map(h(x[1]), zip(x,y)) in
    z[0] --+ y[0]
