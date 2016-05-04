-- Test an in-place update of an argument to main()
-- ==
-- input {
--   [[1],[2],[3],[4],[5]]
--   2
--   42
-- }
-- output {
--   [[1],[2],[42],[4],[5]]
-- }

fun [[int]] main(*[[int,n]] a, int i, int v) =
  let a[i] = replicate(n,v)
  in a
