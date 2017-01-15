-- ==
-- input { [1.0, 2.0, -4.0, 1.5] }
-- output { 8.0 }

fun f(a: f64, b: f64): f64 = a + 3.0
fun g(a: f64, b: f64): f64 = a * 3.0

fun main(arr: []f64): f64 =
    let x = replicate (i32 arr[0]) 2.0
    let y = map f (zip x arr)
    let z = map g (zip arr x) in
    y[0] + z[0]
