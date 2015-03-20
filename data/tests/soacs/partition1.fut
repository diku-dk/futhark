// Simple test of the partition SOAC.

fun bool divisible_by_two(int x) = x % 2 == 0

fun bool divisible_by_three(int x) = x % 3 == 0

fun {[int], [int], [int]} main([int] a) =
  partition(divisible_by_two, divisible_by_three, a)
