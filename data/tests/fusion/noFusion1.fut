-- ==
fun f(a: f64): f64 = a + 3.0
fun g(a: f64) (b: f64): f64 = a * b

fun main(arr: []f64): f64 =
    let n = int(arr[0]) in
    let x = map f arr   in
    loop(arr) = for i < n do
        let y = map (g(arr[i])) x
        in y
    in arr[0]
