-- ==
-- input { [1.0, 2.0, -4.0, 1.5] }
-- output { [13.0, 22.0, -2.0, 17.25] }

fun f(a: f64        ): f64 = a + 3.0
fun g(a: f64        ): f64 = a * 3.0
fun h1(a1: f64, a2: f64, a3: f64): f64 = a1 * a2 + a3
--fun f64 h2((f64,f64) a23) = let (a2,a3) = a23 in a2 * a3
fun h2(a1: f64) (a23: (f64,f64)): f64 = let (a2,a3) = a23 in a2 * a3 - a1

fun main(arr: []f64): []f64 =
    let x = map f arr in
    let y = map g arr in
    if arr[0] < 0.0
    then map h1 (zip x y x)
    --else map(h2(1.0), zip(y,x))
    else map (h2(y[0])) (zip x x)
