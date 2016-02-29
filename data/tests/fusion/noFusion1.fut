-- ==
fun f64 f(f64 a        ) = a + 3.0
fun f64 g(f64 a, f64 b) = a * b

fun f64 main([f64] arr) =
    let N = int(arr[0]) in
    let x = map(f, arr)   in
    loop(arr) = for i < N do
        let y = map(g(arr[i]), x)
        in y
    in arr[0]
