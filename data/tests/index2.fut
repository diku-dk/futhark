-- Test indexing of an array of tuples.
-- ==
-- input {
--   [{1,1.0}, {2,2.0}, {3,3.0}]
--   1
-- }
-- output {
--   2
--   2.000000
-- }

fun {int,real} main([{int,real}] a, int i) =
  a[i]
