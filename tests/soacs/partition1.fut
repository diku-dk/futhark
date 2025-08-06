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

def divisible_by_two (x: i32) : bool = x % 2 == 0

def divisible_by_three (x: i32) : bool = x % 3 == 0

def main (a: []i32) : ([]i32, []i32, []i32) =
  partition2 divisible_by_two divisible_by_three a
