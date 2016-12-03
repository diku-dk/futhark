-- Equality checking of arrays.
--
-- More subtle than it looks, as you also have to compare the dimensions.
-- ==
-- input { empty(int) empty(int) }
-- output { true }
-- input { empty(int) [1] }
-- output { false }
-- input { [1] empty(int) }
-- output { false }
-- input { [1,2] [1,2] }
-- output { true }
-- input { [1,2] [3,4] }
-- output { false }

fun main (xs: []int, ys: []int) = xs == ys
