-- ==
-- input {
-- }
-- output {
--   [1.000000, 0.000000]
--   [2.000000, 0.000000]
-- }
def main : ([]f64, []f64) =
  let n = 2
  let (arr1, arr2) = (replicate n 0.0, replicate n 0.0)
  let arr1[0] = 1.0
  let arr2[0] = 2.0
  in (arr1, arr2)
