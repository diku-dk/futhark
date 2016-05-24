-- ==
-- input {
--   [-2.0,3.0,9.0]
-- }
-- output {
--   19.0
-- }
fun f64 f(f64 a        ) = a + 3.0
fun f64 g(f64 a        ) = a * 3.0
fun f64 h(f64 x, (f64,f64) y) = let (a,b) = y in a * b - (a + b) + x
fun f64 opp(f64 x, f64 a, f64 b) = x*(a+b)

fun f64 main([f64] arr) =
    let arr2 = replicate(5, arr) in
    let y = map( fn f64 ([f64] x)  =>
                    let a = map(f, x) in
                    let b = reduce(opp(1.0), 0.0, a) in
                    b
                , arr2)
    in y[0]
