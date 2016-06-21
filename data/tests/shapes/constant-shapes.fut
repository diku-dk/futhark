-- Test basic constant size annotations.
-- ==
-- input {
--   [5,7,2,4,1,0,9]
-- }
-- output {
--   [5,7,2]
--   [4,1,0,9]
-- }
fun ([3]int, [4]int) main([7]int a) =
  split((3), a)
