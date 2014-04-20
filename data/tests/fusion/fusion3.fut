fun real f(real a        ) = a + 3.0
fun real g(real a        ) = a * 3.0
fun real h(real x, {real,real} y) = let {a,b} = y in a * b - (a + b) + x
fun real opp(real x, real a, real b) = x*(a+b)

fun real main([real] arr) =
    let arr2 = replicate(5, arr) in
    let y = map( fn real ([real] x)  =>
                    let a = map(f, x) in
                    let b = reduce(opp(2.0), 0.0, a) in
                    b//[1]
                , arr2)
    in y[0]
