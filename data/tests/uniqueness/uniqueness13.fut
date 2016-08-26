-- ==
-- input {
--   42
-- }
-- output {
--   [1.000000]
--   [2.000000]
-- }
fun f(b_1: *[]int): ([]f64,[]f64) =
  ([1.0],[2.0])

fun main(n: int): ([]f64, []f64) =
  let a = iota(n) in
  let x = f(a) in
  x
