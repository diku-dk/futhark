-- https://rosettacode.org/wiki/Filter
--
-- Selects all even numbers from an array.
--
-- ==
-- input { [1, 2, 3, 4, 5, 6, 7, 8, 9] }
-- output { [2, 4, 6, 8] }
-- input { empty([0]i32) }
-- output { empty([0]i32) }
-- input { [1,3] }
-- output { empty([0]i32) }

def main (as: []i32) : []i32 =
  filter (\x -> x % 2 == 0) as
