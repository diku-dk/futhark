-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   (
--     3.6
--   , [12.0, -3.0, 1.8000000000000003]
--   , [40.0, 15.0, 23.0]
--   , [0.7, -2.8, -1.68]
--   )
-- }
-- structure { 
--      Redomap 1 
-- }
--
fun (f64,[f64],[f64],[f64]) main([f64] arr) =
    let A = map(+3.0, arr)   in
    let B = map(+7.0, arr)   in
    let s = reduce(+,0.0, A) in

    let X1 = map(*3.0, A)    in
    let X2 = map(*5.0, B)    in
    let X3 = map(*0.7, arr)  in

    (s,X1,X2,X3)
