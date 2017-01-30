-- Reverse an array using indexing.
--
-- ==
-- input { [1,2,3,4] }  output { [4,3,2,1] }
-- input { empty(i32) } output { empty(i32) }

fun main(as: []i32): []i32 = as[::-1]
