-- Using unsafe we can avoid a bounds check.
--
-- ==
-- structure { Assert 0 }

fun main(a: []int, i: int): int =
  unsafe a[i]
