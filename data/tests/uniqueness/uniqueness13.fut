-- ==
-- input {
--   42
-- }
-- output {
--   [1.000000]
--   [2.000000]
-- }
fun ([f64],[f64]) f(*[int] b_1) =
  ([1.0],[2.0])

fun ([f64], [f64]) main(int n) =
  let a = iota(n) in
  let x = f(a) in
  x
