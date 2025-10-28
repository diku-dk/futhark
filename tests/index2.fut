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

def main (a: []i32) (b: []f64) (i: i32) : (i32, f64) =
  let c = zip a b
  in c[i]
