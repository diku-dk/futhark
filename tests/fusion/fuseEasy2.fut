-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   16.000000
-- }
let f(a: f64        ): f64 = a + 3.0
let g(a: f64        ): f64 = a * 3.0
let h(a1: f64, a2: f64, a3: f64): f64 = a1 * a2 + a3

let main(arr: []f64): f64 =
    let x = map f arr
    let y = map g arr
    let z = map h (zip x y x)   in
    z[0]
