-- Test indexing of an array of tuples.
-- ==
-- input {
--   [1, 2, 3]
--   [1.0f32, 2.0f32, 3.0f32]
--   1
-- }
-- output {
--   2
--   2.000000f32
-- }

def main (a: []i32) (b: []f32) (i: i32) : (i32, f32) =
  let c = zip a b
  in c[i]
