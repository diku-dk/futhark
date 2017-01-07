-- Test basic constant size annotations.
-- ==
-- input {
--   [5,7,2,4,1,0,9]
-- }
-- output {
--   [5,7,2]
--   [4,1,0,9]
-- }
fun main(a: [7]int): ([3]int, [4]int) =
  split (3) a
