-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   36.000000
-- }
fun f64 f(f64 a        ) = a + 3.0
fun f64 g(f64 a        ) = a * 3.0

fun f64 main([f64] arr) =
    let x = map(f, arr) in
    let y = map(g, x)   in
    let z = map(g, y)   in
    z[0]
