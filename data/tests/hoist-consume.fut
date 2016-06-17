-- This test can fail if the (consuming) calls to fib are lifted in an
-- erroneous way.
-- ==
-- input {
--   10
-- }
-- output {
--   [42, 42, 42, 42, 42, 42, 42, 42, 42, 42]
-- }
fun *[]int fib(*[]int a, int i, int n) =
  if i == n
  then a
  else if i < 2 then fib(a,i+1,n)
                else fib(a,i+1,n)

fun []int main(int n) = fib(replicate(n,42),0,n)
