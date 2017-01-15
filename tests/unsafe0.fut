-- Using unsafe we can avoid a bounds check.
--
-- ==
-- structure { Assert 0 }

fun main(a: []i32, i: i32): i32 =
  unsafe a[i]
