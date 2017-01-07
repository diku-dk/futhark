-- Curry a simple function.
-- ==
-- input {
--   [8,5,4,3,2,1]
-- }
-- output {
--   [9,6,5,4,3,2]
-- }

fun add(x: int) (y: int): int = x + y

fun main(a: []int): []int =
  map (add(1)) a
