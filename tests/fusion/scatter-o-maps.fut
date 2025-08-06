-- ==
-- compiled input {
--   [3i64, -1i64, 1i64, -10i64]
--   [2.0f32, 3.0f32, 3.0f32, 4.0f32]
--   [7.0f32, 8.0f32, 9.0f32, 10.0f32, 12.0f32, 15.0f32, 18.0f32]
-- }
-- output {
--   [7.0f32, 8.0f32, 9.0f32, 10.0f32, -3.0f32, 15.0f32, 3.0f32]
--   [0.0f32, 0.0f32, 0.0f32, 0.0f32, -9.0f32, 0.0f32, 9.0f32]
-- }
--
-- structure {
--   /WithAcc/Screma 1
-- }

def main [n] [m] (is: [n]i64) (vs: [n]f32) (xs: *[m]f32) =
  let is' = map (+ 5) is
  let vs' = map2 (\i v -> v * f32.i64 i) is vs
  let res1 = scatter xs is' vs'
  let vs'' = map2 (\i v -> (v * v) / f32.i64 i) is vs'
  let res2 = scatter (replicate m 0.0f32) is' vs''
  in (res1, res2)
