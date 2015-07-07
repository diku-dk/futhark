-- Test simple indexing of an array.
-- ==
-- input {
--   [[4,3],[3,2],[2,1],[1,0]]
--   1
-- }
-- output {
--   [3,2]
-- }

fun [int] main([[int]] a, int i) =
  a[i]
