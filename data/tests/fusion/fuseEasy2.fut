-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   16.000000
-- }
fun f64 f(f64 a        ) = a + 3.0
fun f64 g(f64 a        ) = a * 3.0
fun f64 h(f64 a1, f64 a2, f64 a3) = a1 * a2 + a3

fun f64 main([]f64 arr) =
    let x = map(f, arr) in
    let y = map(g, arr) in
    let z = map(h, zip(x,y,x))   in
    z[0]
