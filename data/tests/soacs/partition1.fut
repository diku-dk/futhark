-- Simple test of the partition SOAC.
-- ==
-- input {
--   [0,1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   [0, 2, 4, 6, 8]
--   [3, 9]
--   [1, 5, 7]
-- }

fun bool divisible_by_two(int x) = x % 2 == 0

fun bool divisible_by_three(int x) = x % 3 == 0

fun ([int], [int], [int]) main([int] a) =
  partition(divisible_by_two, divisible_by_three, a)
