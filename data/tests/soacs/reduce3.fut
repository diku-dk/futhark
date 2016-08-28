-- This test checks whether empty reduces are handled properly.
-- ==
-- input {
--   0
-- }
-- output {
--   False
--   0
-- }
fun main(n: int): (bool,int) =
  let (a,b) = reduce(fn (accx,accy) (x,y): (bool,int)  =>
                       (accx && x, y),
                     (False,0),
                     zip(replicate n True, replicate n 1)) in
  (a,b)
