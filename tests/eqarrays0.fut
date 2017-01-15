-- Equality checking of arrays.
--
-- More subtle than it looks, as you also have to compare the dimensions.
-- ==
-- input { empty(i32) empty(i32) }
-- output { true }
-- input { empty(i32) [1] }
-- output { false }
-- input { [1] empty(i32) }
-- output { false }
-- input { [1,2] [1,2] }
-- output { true }
-- input { [1,2] [3,4] }
-- output { false }

fun main (xs: []i32, ys: []i32) = xs == ys
