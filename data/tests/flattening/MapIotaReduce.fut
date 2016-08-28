-- ==
-- input {
--   [1,2,3,4]
-- }
-- output {
--   [0, 1, 3, 6]
-- }
fun main (xs: []int): []int =
  map( fn (x: int): int  =>
         let tmp = iota(x) in
         reduce((+),0,tmp)
     , xs)
