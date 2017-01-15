-- Wrong, because the map function must return an array.
-- ==
-- error:

fun main(a: []int): ([]int) = map (\(x: int): int  -> x + 2) a
