-- ==
-- input {
--   [ [2,3,4] , [3,2,1] ]
--   [5,7]
-- }
-- output {
-- [ [5, 15, 30]
-- , [21, 7, 0 ]
-- ]
-- }
fun [[int]] main ([[int,n],m] xss, [int,m] ys) =
  map( fn [int,n] ([int,n] xs, int y) =>
         map (fn int (int x) =>
                let tmp1 = iota(x) in
                let tmp2 = map(*y,tmp1) in
                reduce(+,0,tmp2)
             , xs)
     , zip(xss, ys) )
