-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   [6.0, 1.0, 2.6]
--   [2.0, -3.0, -1.4]
--   [3.0, -7.0, -3.8]
-- }
-- structure { 
--      Map 1 
-- }
--
fun main(arr: []f64): ([]f64,[]f64,[]f64) =
    let x = map    ((+ 1.0), arr) in
    let y = zipWith((+),  x, arr) in
    let r = map    ((+ 5.0), arr) in
    (r,x,y)

