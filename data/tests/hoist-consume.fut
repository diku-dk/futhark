-- This test can fail if the (consuming) calls to fib are lifted in an
-- erroneous way.
-- ==
-- input {
--   10
-- }
-- output {
--   [42, 42, 42, 42, 42, 42, 42, 42, 42, 42]
-- }
fun fib(a: *[]int, i: int, n: int): *[]int =
  if i == n
  then a
  else if i < 2 then fib(a,i+1,n)
                else fib(a,i+1,n)

fun main(n: int): []int = fib(replicate(n,42),0,n)
