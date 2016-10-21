-- You may not consume a free variable inside of a lambda.
--
-- ==
-- error:

fun consume(a: *[]int): []int = a

fun main(a: *[]int): [][]int =
  map (fn i => consume a) (iota 10)
