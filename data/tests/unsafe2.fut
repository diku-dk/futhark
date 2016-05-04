-- Using unsafe we can also avoid assertions due to shape checks.
--
-- ==
-- structure { Assert 0 }

fun [(int,int),n] main([int,n] a, [int,m] b) =
  unsafe zip(a, b)
