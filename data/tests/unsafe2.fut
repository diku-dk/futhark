-- Using unsafe we can also avoid assertions due to shape checks.
--
-- ==
-- structure { Assert 0 }

fun [n](int,int) main([n]int a, [m]int b) =
  unsafe zip(a, b)
