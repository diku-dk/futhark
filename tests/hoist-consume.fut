-- This test can fail if the (consuming) calls to fib2 are lifted in an
-- erroneous way.
-- ==
-- input {
--   10
-- }
-- output {
--   [42, 42, 42, 42, 42, 42, 42, 42, 42, 42]
-- }

fun fib2(a: *[]i32, i: i32, n: i32): *[]i32 =
  a

fun fib(a: *[]i32, i: i32, n: i32): *[]i32 =
  if i == n
  then a
  else if i < 2 then fib2(a,i+1,n)
                else fib2(a,i+1,n)

fun main(n: i32): []i32 = fib(replicate n 42,0,n)
