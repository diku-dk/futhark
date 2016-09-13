-- ==
-- input {
--   10
-- }
-- output {
--   3628800
-- }
fun fact(n: int): int =
  loop (out = 1) = for i < n do
    out * (i+1)
  in out

fun main(n: int): int =
  fact(n)
