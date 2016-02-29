-- ==
-- input {
--   [1.0,2.0,3.0,4.0]
-- }
-- output {
--   65.000000
-- }
fun f64 f(f64 a        ) = a + 3.0
fun f64 g(f64 a        ) = a * 3.0
fun f64 h(f64 a, f64 b) = a * b - (a + b)

fun f64 main([f64] arr) =
    let b = map(f, arr) in
    --let arr[1] = 3.33   in
    let x = map(f, b)   in
    let y = map(g, b)   in
    let z = map(h, zip(x,y)) in
    z[0]
