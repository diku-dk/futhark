-- Test that you can shadow a function with a variable.
--
-- ==
-- error: f
fun f (x: i32): i32 = x + 2

fun main(x: i32): i32 =
  let f = 3
  in f x
