-- ==
-- input { [1.0, 2.0, -4.0, 1.5] }
-- output { 8.0 }

fun f64 f(f64 a, f64 b) = a + 3.0
fun f64 g(f64 a, f64 b) = a * 3.0

fun f64 main([f64] arr) =
    let x = replicate(int(arr[0]), 2.0)  in
    let y = map(f, zip(x,arr)) in
    let z = map(g, zip(arr,x)) in
    y[0] + z[0]
