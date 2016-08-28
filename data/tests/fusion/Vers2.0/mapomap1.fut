-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   [4.0, -16.0, -9.6]
--   [2.0, -3.0, -1.4 ]
--   [3.0, -2.0, -0.3999999999999999]
--   [4.0, -6.0, -2.8 ]
--   [9.0, -6.0, -1.1999999999999997]
-- }
-- structure { 
--      Map 1 
-- }
--
fun main(arr: []f64): ([]f64,[]f64,[]f64,[]f64,[]f64) =
    let xy = map (fn (a: f64): (f64,f64)  => (a+1.0,a+2.0)) arr in
    let (x,y) = unzip(xy) in
    let z  = map (*2.0) x in
    let w  = map (*3.0) y in
    let r  = map (*4.0) arr in
    (r,x,y,z,w)
