-- ==
-- input {
--   [1.0,2.0,3.0,4.0]
-- }
-- output {
--   73.000000
-- }
fun real f(real a        ) = a + 3.0
fun real g(real a        ) = a * 3.0
fun real h(real x, {real,real} y) = let {a,b} = y in a * b - (a + b) + x

fun real main([real] arr) =
    let b = map(f, arr) in
    let x = map(f, b)   in
    let y = map(g, b)   in
    let z = map(h(x[1]), zip(x,y)) in
    z[0] --+ y[0]
