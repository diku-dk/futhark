-- ==
-- input {
--   42
--   1337
-- }
-- output {
--   24730855
-- }
fun fun1(a: int, b: int): int = a + b

fun fun2(a: int, b: int): int = fun1(a,b) * (a+b)

fun fun3(a: int, b: int): int = fun2(a,b) + a + b

fun main(n: int, m: int): int =
  fun1(n,m) + fun2(n+n,m+m) + fun3(3*n,3*m) + fun2(2,n) + fun3(n,3)
