-- ==
-- input { [4.0, 2.0, -4.0, 1.5] }
-- output { 17.0 }

let f(a: f64, b: f64): f64 = a + 3.0
let g(a: f64, b: f64): f64 = a * 3.0

let main (arr: []f64): f64 =
    let n = t64 arr[0]
    let x = replicate n 2.0
    let y = map f (zip x (arr :> [n]f64))
    let z = map g (zip (arr :> [n]f64) x)
    in y[0] + z[0]
