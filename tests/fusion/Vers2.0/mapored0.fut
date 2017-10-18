-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   -5.4
--   [2.0,-3.0,-1.4]
-- }
-- structure {
--      Redomap 1
-- }
--
let main(arr: []f64): (f64,[]f64) =
    let r = reduce (+) (0.0) arr
    let x = map    (+1.0) arr in
    (r,x)
