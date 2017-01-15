-- ==
-- input {
--   2
--   [9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]
-- }
-- output {
--   [9.000000, 8.000000]
-- }
fun main(n: i32, a: []f64): []f64 =
  let (first, rest) = split (n) a in first
