-- ==
-- input {
--   [-2.0,3.0,9.0]
-- }
-- output {
--   19.0
-- }
fun f(a: f64        ): f64 = a + 3.0
fun g(a: f64        ): f64 = a * 3.0
fun h(x: f64, y: (f64,f64)): f64 = let (a,b) = y in a * b - (a + b) + x
fun opp(x: f64) (a: f64) (b: f64): f64 = x*(a+b)

fun main(arr: []f64): f64 =
    let arr2 = replicate 5 arr
    let y = map (\(x: []f64): f64   ->
                    let a = map f x
                    let b = reduce (opp(1.0)) (0.0) a in
                    b
                ) arr2
    in y[0]
