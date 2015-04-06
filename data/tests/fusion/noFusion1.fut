// --
fun real f(real a        ) = a + 3.0
fun real g(real a, real b) = a * b

fun real main([real] arr) =
    let N = trunc(arr[0]) in
    let x = map(f, arr)   in
    loop(arr) = for i < N do
        let y = map(g(arr[i]), x)
        in y
    in arr[0]
