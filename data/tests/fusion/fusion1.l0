fun real f(real a        ) = a + 3.0
fun real g(real a        ) = a * 3.0
fun real h(real a, real b) = a * b - (a + b)

fun real main([real] arr) =
    let b = map(f, arr) in
    //let arr[1] = 3.33   in
    let x = map(f, b)   in
    let y = map(g, b)   in
    let z = map(h, zip(x,y)) in
    z[0]
