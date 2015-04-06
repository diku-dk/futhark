// --
fun real f(real a, real b) = a + 3.0
fun real g(real a, real b) = a * 3.0

fun real main([real] arr) =
    let x = replicate(trunc(arr[0]), 2.0)  in
    let y = map(f, zip(x,arr)) in
    let z = map(g, zip(arr,x)) in
    y[0] + z[0]
