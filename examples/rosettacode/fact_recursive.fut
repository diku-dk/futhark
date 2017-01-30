-- ==
-- input {
--   10
-- }
-- output {
--   3628800
-- }
fun fact(n: i32): i32 =
  if n == 0 then 1
            else n * fact(n-1)

fun main(n: i32): i32 =
  fact(n)
