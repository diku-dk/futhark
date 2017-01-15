-- Reverse an array using indexing.
--
-- ==
-- input { [1,2,3,4] }  output { [4,3,2,1] }
-- input { empty(int) } output { empty(int) }

fun main(as: []int): []int = as[::-1]
