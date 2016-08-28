-- ==
-- input {
--   5
--   [3,9,4,7,19]
-- }
-- output {
--   0
--   [9, 7, 19]
-- }
fun main(m: int, a: *[n]int): (int,[]int) =
  streamSeq( fn (chunk: int) (acc: int) (c: *[]int): (int,*[]int)  =>
               let w = filter((>6), c ) in
               ( acc, w )
           , 0, a)


