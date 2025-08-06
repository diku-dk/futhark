-- Test that filter can be fused into reduce.
-- ==
-- input {
--   [9,-3,5,2]
-- }
-- output {
--   6
-- }

def divisibleBy (x: i32) (y: i32) : bool = y % x == 0

def main (a: []i32) : i32 =
  let threes = filter (divisibleBy 3) a
  in reduce (+) 0 threes
