-- ==
-- input {
--   5
-- }
-- output {
--   8
-- }
fun fib(n: int): int = if n < 2 then 1 else fib(n-1) + fib(n-2)

fun main(n: int): int = fib(n)
