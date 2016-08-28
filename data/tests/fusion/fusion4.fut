-- Test that filter can be fused into reduce.
-- ==
-- input {
--   [9,-3,5,2]
-- }
-- output {
--   6
-- }

fun divisibleBy(x: int) (y: int): bool = y % x == 0

fun main(a: []int): int =
  let threes = filter (divisibleBy 3) a in
  reduce (+) 0 threes
