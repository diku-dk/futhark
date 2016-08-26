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

fun divisible_by_two(x: int): bool = x % 2 == 0

fun divisible_by_three(x: int): bool = x % 3 == 0

fun main(a: []int): ([]int, []int, []int) =
  partition(divisible_by_two, divisible_by_three, a)
