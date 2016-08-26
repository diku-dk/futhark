-- Test that we can index a non-variable.
--
-- ==
-- input { 3 2 } output { 2 }

fun main(n: int, i: int): int =
  iota(n)[i]
