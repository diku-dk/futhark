-- Test that negation works for both integers and f32s.
-- ==
-- input {
--   [1,2,3]
-- }
-- output {
--   [-1, -2, -3]
--   [-1.000000f32, -2.000000f32, -3.000000f32]
-- }
def main (a: []i32) : ([]i32, []f32) =
  (map (0 -) a, map (0.0 -) (map f32.i32 a))
