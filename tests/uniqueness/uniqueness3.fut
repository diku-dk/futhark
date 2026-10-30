-- ==
-- input {
-- }
-- output {
--   [1.000000f32, 0.000000f32]
--   [2.000000f32, 0.000000f32]
-- }
def main : ([]f32, []f32) =
  let n = 2
  let (arr1, arr2) = (replicate n 0.0, replicate n 0.0)
  let arr1[0] = 1.0
  let arr2[0] = 2.0
  in (arr1, arr2)
