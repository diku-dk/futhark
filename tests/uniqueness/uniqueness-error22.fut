-- Test that we cannot consume anything inside an anonymous function.
-- ==
-- error:

fun f(a: *[]i32): i32 = a[0]

fun main(n: i32): i32 =
  let a = iota(n) in
  reduce (\(sum: i32, i: i32): i32  -> sum + f(a)) 0 (iota(10))
