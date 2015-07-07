-- ==
-- input {
--   [2,3,4]
--   [8,3,2]
-- }
-- output {
--   [8,9,12]
-- }
fun [int] main ([int,n] xs, [int,n] ys) =
  map( fn int (int x, int y) =>
         let tmp1 = iota(x) in
         let tmp2 = map(*y,tmp1) in
         reduce(+,0,tmp2)
     , zip(xs, ys) )
