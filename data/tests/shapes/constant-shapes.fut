-- Test basic constant size annotations.
-- ==
-- input {
--   [5,7,2,4,1,0,9]
-- }
-- output {
--   [5,7,2]
--   [4,1,0,9]
-- }
fun ([int,3], [int,4]) main([int,7] a) =
  split((3), a)
