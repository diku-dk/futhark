-- This test checks whether empty reduces are handled properly.
-- ==
-- input {
--   0i64
-- }
-- output {
--   false
--   0
-- }
def main (n: i64) : (bool, i32) =
  let (a, b) =
    reduce (\(accx, accy) (x, y) : (bool, i32) ->
              (accx && x, y))
           (false, 0)
           (zip (replicate n true) (replicate n 1))
  in (a, b)
