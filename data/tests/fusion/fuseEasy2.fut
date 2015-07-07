-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   16.000000
-- }
fun real f(real a        ) = a + 3.0
fun real g(real a        ) = a * 3.0
fun real h(real a1, real a2, real a3) = a1 * a2 + a3

fun real main([real] arr) =
    let x = map(f, arr) in
    let y = map(g, arr) in
    let z = map(h, zip(x,y,x))   in
    z[0]
