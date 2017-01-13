-- This test checks whether empty reduces are handled properly.
-- ==
-- input {
--   0
-- }
-- output {
--   false
--   0
-- }
fun main(n: int): (bool,int) =
  let (a,b) = reduce (\(accx,accy) (x,y): (bool,int)  ->
                       (accx && x, y)) (false,0) (
                     zip (replicate n true) (replicate n 1)) in
  (a,b)
