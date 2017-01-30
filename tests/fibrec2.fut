-- ==
-- compiled input {
--   5
-- }
-- output {
--   [1, 1, 2, 3, 5]
-- }
fun fib(a: *[n]i32, i: i32): *[n]i32 =
  if i == n
  then a
  else if i < 2 then let a[i] = 1 in fib(a,i+1)
                else let a[i] = a[i-1]+a[i-2] in fib(a,i+1)

fun main(n: i32): []i32 = fib(replicate n 0,0)
