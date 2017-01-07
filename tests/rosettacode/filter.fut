-- https://rosettacode.org/wiki/Filter
--
-- Selects all even numbers from an array.
--
-- ==
-- input { [1, 2, 3, 4, 5, 6, 7, 8, 9] }
-- output { [2, 4, 6, 8] }
-- input { empty(int) }
-- output { empty(int) }
-- input { [1,3] }
-- output { empty(int) }

fun main(as: []int): []int =
  filter (fn x => x%2 == 0) as