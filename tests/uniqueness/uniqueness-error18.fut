-- Check that unique components of a return tuple do not alias each
-- other.
-- ==
-- error:

fun main(n: int): (*[]int, *[]int) =
  let a = iota(n) in
  (a, a)
