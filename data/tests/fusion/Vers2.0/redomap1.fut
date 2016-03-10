-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   -5.4
--   [3.0, -2.0, -0.4]
--   [2.0, -3.0, -1.4]
--   [4.0, -6.0, -2.8]
--   [9.0, -6.0, -1.2]
-- }
-- structure {
--      Redomap 1
-- }
--
fun {f64,[f64],[f64],[f64],[f64]} main([f64] arr) =
    let xy = map(fn {f64,f64} (f64 a) => {a+1.0,a+2.0}, arr) in
    let {x,y} = unzip(xy) in
    let z  = map(*2.0, x) in
    let w  = map(*3.0, y) in
    let r  = reduce (+,0.0, arr) in
    {r,x,y,z,w}
