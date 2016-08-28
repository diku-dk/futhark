-- ==
-- input {
--   [1.0,2.0,3.0,4.0]
-- }
-- output {
--   65.000000
-- }
fun f(a: f64        ): f64 = a + 3.0
fun g(a: f64        ): f64 = a * 3.0
fun h(a: f64, b: f64): f64 = a * b - (a + b)

fun main(arr: []f64): f64 =
    let b = map f arr in
    --let arr[1] = 3.33   in
    let x = map f b   in
    let y = map g b   in
    let z = map h (zip(x,y)) in
    z[0]
