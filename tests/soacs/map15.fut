-- Test that a map not using its parameters can be turned into a
-- replicate.
--
-- ==
-- input { 2 [1,2,3] }
-- output { [4, 4, 4] }
-- structure { Map 0 Replicate 1 }

fun main(x: int, a: []int): []int =
  map (fn (y: int): int  =>
        x + 2) a
