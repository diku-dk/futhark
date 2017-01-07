-- Test simple indexing of an array.
-- ==
-- input {
--   [4,3,2,1,0]
--   1
-- }
-- output {
--   3
-- }

fun main(a: []int, i: int): int =
  a[i]
