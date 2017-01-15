-- Test that we cannot consume anything inside an anonymous function.
-- ==
-- error:

fun f(a: *[]int): int = a[0]

fun main(n: int): int =
  let a = iota(n) in
  reduce (\(sum: int, i: int): int  -> sum + f(a)) 0 (iota(10))
