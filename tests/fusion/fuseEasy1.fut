-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   36.000000
-- }
let f(a: f64        ): f64 = a + 3.0
let g(a: f64        ): f64 = a * 3.0

let main(arr: []f64): f64 =
    let x = map f arr
    let y = map g x
    let z = map g y   in
    z[0]
