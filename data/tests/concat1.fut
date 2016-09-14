-- Basic 1D concatting.
-- ==
-- input {
--   [1,2]
--   [3,4]
-- }
-- output {
--   [1,2,3,4]
-- }
-- input {
--   empty(int)
--   [1,2,3]
-- }
-- output {
--   [1,2,3]
-- }

fun main(a: []int, b: []int): []int =
  concat a b
