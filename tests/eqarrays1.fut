-- Equality checking of arrays of two dimensions.
-- ==
-- input { [[1,2],[3,4]] [[1,2],[3,4]] }
-- output { true }
-- input { [[1,2],[3,4]] [[1,2],[3,5]] }
-- output { false }

fun main (xs: [][]int, ys: [][]int) = xs == ys
