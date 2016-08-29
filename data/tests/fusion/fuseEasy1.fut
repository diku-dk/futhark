-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   36.000000
-- }
fun f(a: f64        ): f64 = a + 3.0
fun g(a: f64        ): f64 = a * 3.0

fun main(arr: []f64): f64 =
    let x = map f arr
    let y = map g x  
    let z = map g y   in
    z[0]
