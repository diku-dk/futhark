-- Test indexing of an array of tuples.
-- ==
-- input {
--   [1, 2, 3]
--   [1.0, 2.0, 3.0]
--   1
-- }
-- output {
--   2
--   2.000000
-- }

fun (int,f64) main([]int a, []f64 b, int i) =
  let c = zip(a,b)
  in c[i]
