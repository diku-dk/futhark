-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   16.000000
-- }
fun f(a: f64        ): f64 = a + 3.0
fun g(a: f64        ): f64 = a * 3.0
fun h(a1: f64, a2: f64, a3: f64): f64 = a1 * a2 + a3

fun main(arr: []f64): f64 =
    let x = map f arr in
    let y = map g arr in
    let z = map h (zip x y x)   in
    z[0]
