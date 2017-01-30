-- ==
-- input {
--   5
-- }
-- output {
--   8
-- }
fun fib(n: i32): i32 = if n < 2 then 1 else fib(n-1) + fib(n-2)

fun main(n: i32): i32 = fib(n)
