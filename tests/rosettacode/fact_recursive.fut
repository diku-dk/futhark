-- ==
-- input {
--   10
-- }
-- output {
--   3628800
-- }
fun fact(n: int): int =
  if n == 0 then 1
            else n * fact(n-1)

fun main(n: int): int =
  fact(n)
