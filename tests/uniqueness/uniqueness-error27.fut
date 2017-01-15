-- You may not consume a free variable inside of a lambda.
--
-- ==
-- error:

fun consume(a: *[]i32): []i32 = a

fun main(a: *[]i32): [][]i32 =
  map (\i -> consume a) (iota 10)
