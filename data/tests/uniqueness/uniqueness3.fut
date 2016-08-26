-- ==
-- input {
-- }
-- output {
--   [1.000000, 0.000000]
--   [2.000000, 0.000000]
-- }
fun main(): ([]f64,[]f64) =
  let n = 2 in
  let arrs = (copy(replicate(n, 0.0)), copy(replicate(n, 0.0))) in
  let (arr1, arr2) = arrs in
  let arr1[0] = 1.0 in
  let arr2[0] = 2.0 in
  (arr1, arr2)
