-- ==
-- compiled input {
--   5
-- }
-- output {
--   [1, 1, 2, 3, 5]
-- }
fun *[int,n] fib(*[int,n] a, int i) =
  if i == n
  then a
  else if i < 2 then let a[i] = 1 in fib(a,i+1)
                else let a[i] = a[i-1]+a[i-2] in fib(a,i+1)

fun [int] main(int n) = fib(replicate(n,0),0)
