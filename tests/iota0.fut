-- Does iota work at all?
-- ==
-- input { 0 }
-- output { empty(int) }
-- input { 2 }
-- output { [0,1] }

fun main(n: int): []int = iota(n)
