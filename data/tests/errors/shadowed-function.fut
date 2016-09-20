-- Test that you can shadow a function with a variable.
--
-- ==
-- error: f
fun f (x: int): int = x + 2

fun main(x: int): int =
  let f = 3
  in f x
