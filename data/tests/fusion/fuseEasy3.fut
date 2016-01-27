-- ==
-- input { [1.0, 2.0, -4.0, 1.5] }
-- output { [13.0, 22.0, -2.0, 17.25] }

fun real f(real a        ) = a + 3.0
fun real g(real a        ) = a * 3.0
fun real h1(real a1, real a2, real a3) = a1 * a2 + a3
--fun real h2({real,real} a23) = let {a2,a3} = a23 in a2 * a3
fun real h2(real a1, {real,real} a23) = let {a2,a3} = a23 in a2 * a3 - a1

fun [real] main([real] arr) =
    let x = map(f, arr) in
    let y = map(g, arr) in
    if arr[0] < 0.0
    then map(h1, zip(x,y,x))
    --else map(h2(1.0), zip(y,x))
    else map(h2(y[0]), zip(x,x))
