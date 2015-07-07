-- ==
-- input {
--   [1,2,3,4]
-- }
-- output {
--   [0, 1, 3, 6]
-- }
fun [int] main ([int] xs) =
  map( fn int (int x) =>
         let tmp = iota(x) in
         reduce(+,0,tmp)
     , xs)
